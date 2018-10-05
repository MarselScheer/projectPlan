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
  df
  # h.mark_unmet_deadlines(df)
}

h.turn_weekend_day_to_monday <- function(day) {
  if (lubridate::wday(day) == 7) {
    day <- day + 2
  } else if (lubridate::wday(day) == 1) {
    day <- day + 1
  }
  day
}

h.exclude_weekends <- function(start, end){
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
  
  if (lubridate::wday(end) %in% c(1,7)) {
    # if we stop working on saturday we actually have to work till monday
    # if we stop working on sunday we actually have to work till tuesday
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
  futile.logger::flog.debug("Calculate time lines for", dt_ref[row, ], capture = TRUE)
  if (!is.na(dt_ref$time_start[row]) & !is.na(dt_ref$time_end[row])) {
    return()
  }


  ids_prior <- unlist(dt_ref$prior_ids[row])
  prior_tasks <- dt_ref[id %in% ids_prior, ]
  fsd <- dt_ref$fixed_start_date[row]

  # seems to be some bug because is.na(earliest_start_time) returns FALSE?!?
  if (is.na(as.character(fsd))) {
    earliest_start_time <- max(prior_tasks$time_end)
  } else {
    futile.logger::flog.info("A fixed start date was provided. Use this start date irrespective any possible dependencies", dt_ref[row, ], capture = TRUE)
    earliest_start_time <- fsd
  }


  while (is.na(earliest_start_time)) {
    na_id <- prior_tasks %>%
      dplyr::filter(is.na(time_end)) %>%
      dplyr::slice(1) %>%
      with(id)
    futile.logger::flog.info(glue::glue("Nonsorted entry -{dt_ref$id[row]}- must follow after -{na_id}-"))

    first_na_idx <- which(dt_ref$id == na_id)[1]
    h.calculate_time_lines_at(dt_ref, first_na_idx)

    prior_tasks <- dt_ref[id %in% ids_prior]
    earliest_start_time <- max(prior_tasks$time_end)
  }

  end <- h.calculate_end_time(earliest_start_time, dt_ref$waiting[row], dt_ref$est_days[row], dt_ref$fixed_end_date[row])

  dt_ref[row, time_start := earliest_start_time]
  dt_ref[row, time_end := end]

  if (end < earliest_start_time) {
    futile.logger::flog.warn("-time_start- is before -time_end-", dt_ref[row, ], capture = TRUE)
  }
}
