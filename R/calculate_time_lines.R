#' Calculate explit start and end dates for the given project plan.
#' 
#' Usually this function is called after a raw plan with tasks was prepared by \link{wrangle_raw_plan}.
#' It takes a set of tasks and their estimated duration as well as
#' dependencies between those tasks. This implicitly defines start and end dates
#' for each task and this function calculates the corresponding explicit
#' start and end times.
#' 
#' @param df A \code{data.frame} containing one task in each row.
#' @return A \code{data.table} object with explicit start and end times for every (grouped) task
#'
#' @seealso \link{wrangle_raw_plan}, \link{gantt_by_sections}
#' @export
#' @import data.table
calculate_time_lines <- function(df) {
  df$time_start <- NA
  df$time_start <- lubridate::as_date(df$time_start)
  df$time_end <- NA
  df$time_end <- lubridate::as_date(df$time_end)

  # this way we can leverage call by reference
  df <- data.table::data.table(df)
  for (i in 1:nrow(df)) {
    h.calculate_time_lines_at(df, i)
  }
  h.calc_end_to_deadline(df)
  df
}

h.calc_dist_to_deadline <- function(date_vec, deadline_vec) {
  raw_dist <- deadline_vec - date_vec
  overdue <- as.integer(raw_dist < 0)
  nmb_weekends <- floor(abs(raw_dist) / 7)
  nmb_weekends <- nmb_weekends + 
    abs(overdue - 1) * (wday(deadline_vec) < wday(date_vec)) + 
    overdue  * (wday(deadline_vec) > wday(date_vec))
  nmb_weekends <- -1 * overdue * nmb_weekends + abs(overdue - 1) * nmb_weekends
  
  raw_dist - 2 * nmb_weekends
}


h.calc_end_to_deadline <- function(df) {
  idx <- !is.na(df$deadline)
  df[, dist_end_to_deadline := NA]
  if (any(idx)) {
    with(NULL, df[idx, dist_end_to_deadline := h.calc_dist_to_deadline(time_end, deadline)])
    h.log_rows(df, df$dist_end_to_deadline <= 0, "DEADLINE TODAY OR ALREADY UNMET", warn_columns = c("project", "section", "id", "time_start", "time_end", "deadline", "progress", "resource", "task"))
  }
}

h.turn_weekend_day_to_monday <- function(day) {
  if (lubridate::wday(day) == 7) {
    futile.logger::flog.debug(glue::glue("Change the saturday {day} to monday {day + 2}"))
    day <- day + 2
  } else if (lubridate::wday(day) == 1) {
    futile.logger::flog.debug(glue::glue("Change the sunday {day} to monday {day + 1}"))
    day <- day + 1
  }
  day
}

h.exclude_weekends <- function(start, end) {
  if (end < start) {
    msg <- glue::glue("Specified end time {end} is before the start time {start}")
    futile.logger::flog.error(msg)
    stop(msg)
  }

  shift <- h.turn_weekend_day_to_monday(start) - start
  if (shift > 0) {
    futile.logger::flog.warn(glue::glue("start {start} is on a weekend. shift start {start} and end {end} by {shift} day(s)."))
    start <- start + shift
    end <- end + shift
  }

  nmb_workdays <- as.integer(end - start)
  nmb_workweeks <- floor(nmb_workdays / 5)
  nmb_days_remain <- nmb_workdays %% 5

  end <- start + 7 * nmb_workweeks + nmb_days_remain

  if (lubridate::wday(end) %in% c(1, 7)) {
    # if we stop working on saturday we actually have to work till monday
    # if we stop working on sunday we actually have to work till tuesday
    futile.logger::flog.debug(glue::glue("The task ends on {end} which is a weekend. Hence, the actual end is 2 days on {end + 2}"))
    end <- end + 2
  }
  end
}

h.calculate_end_time <- function(earliest_start_time, is_waiting, est_days, fixed_end_date) {
  if (!is.na(as.character(fixed_end_date))) {
    end <- fixed_end_date
    end <- h.turn_weekend_day_to_monday(end)
    return(end)
  }

  if (is_waiting) {
    end <- lubridate::as_date(lubridate::now())
    end <- h.turn_weekend_day_to_monday(end)
    return(end)
  }

  end <- earliest_start_time + est_days
  h.exclude_weekends(earliest_start_time, end)
}


h.calculate_time_lines_at <- function(dt_ref, row) {
  futile.logger::flog.debug(glue::glue("Calculate time lines for row -{row}-"), dt_ref[row, ], capture = TRUE)
  if (!is.na(dt_ref$time_start[row]) & !is.na(dt_ref$time_end[row])) {
    return()
  }


  ids_prior <- unlist(dt_ref$prior_ids[row])
  prior_tasks <- with(NULL, dt_ref[id %in% ids_prior, ])
  fsd <- dt_ref$fixed_start_date[row]

  # seems to be some bug because is.na(earliest_start_time) returns FALSE?!?
  if (is.na(as.character(fsd))) {
    earliest_start_time <- max(prior_tasks$time_end)
  } else {
    earliest_start_time <- fsd
  }

  futile.logger::flog.debug(glue::glue("Try to calculate earliest start time for row -{row}- based on prior tasks"), prior_tasks, capture = TRUE)
  while (is.na(earliest_start_time)) {
    prior_tasks <- with(NULL, prior_tasks[is.na(time_end)])
    na_id <- prior_tasks$id[1]
    futile.logger::flog.info(glue::glue("Nonsorted entry -{dt_ref$id[row]}- must follow after -{na_id}-"))

    first_na_idx <- which(dt_ref$id == na_id)[1]
    h.calculate_time_lines_at(dt_ref, first_na_idx)

    prior_tasks <- with(NULL, dt_ref[id %in% ids_prior])
    futile.logger::flog.debug(glue::glue("Try to calculate earliest start time for row -{row}- based on prior tasks"), prior_tasks, capture = TRUE)
    earliest_start_time <- max(prior_tasks$time_end)
  }
  futile.logger::flog.debug(glue::glue("Earliest start time found for row -{row}-: {earliest_start_time}"))

  end <- h.calculate_end_time(earliest_start_time, dt_ref$waiting[row], dt_ref$est_days[row], dt_ref$fixed_end_date[row])

  with(NULL, dt_ref[row, time_start := earliest_start_time])
  with(NULL, dt_ref[row, time_end := end])
  futile.logger::flog.debug(glue::glue("Timelines for the current row -{row}-"), dt_ref[row], capture = TRUE)


  if (end < earliest_start_time) {
    futile.logger::flog.warn("-time_start- is before -time_end-", dt_ref[row, ], capture = TRUE)
  }
}
