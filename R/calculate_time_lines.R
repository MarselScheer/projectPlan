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
  TODAY <- lubridate::as_date(lubridate::now())
  df <- data.table::data.table(df)
  for (i in 1:nrow(df)) {
    h.calculate_time_lines_at(df, i, TODAY)
  }
  h.set_deadline_for_waiting_tasks(df)  
  h.calc_end_to_deadline(df)
  df
}

#' @export
collapse_projects <- function(dt, projects, task_label = "{project} collapsed") {
  ret <- dt
  for (p in unique(projects)) {
    ret <- h.collapse_project(ret, p, task_label)
  }
  ret
}

#' @export
collapse_complete_sections <- function(dt) {
  
  is.completed <- function(progress, aborted) {
    all(progress[!aborted] == 100)
  }
  
  complete_sections <- dt[, .(complete = is.completed(progress, aborted)), by = "section"][complete == TRUE]
  for (proj_sec in complete_sections$section) {
    proj_sec <- unlist(strsplit(proj_sec, "::"))
    dt <- collapse_section(dt, project = proj_sec[1], section = proj_sec[2], task_label = "{project}::{section} completed")
  }
  dt
}

#' @export
collapse_complete_projects <- function(dt) {
  
  is.completed <- function(progress, aborted) {
    all(progress[!aborted] == 100)
  }
  
  complete_projects <- dt[, .(complete = is.completed(progress, aborted)), by = "project"][complete == TRUE]
  collapse_projects(dt, projects = complete_projects$project, task_label = "{project} completed")
}


#' @export
collapse_section <- function(dt, project, section, task_label = "{project}::{section} collapsed") {
  if (missing(project)) {
    msg <- "The parameter project must be specified."
    logger::log_error(msg)
    stop(msg)
  }
  idx <- 
  
  if (missing(section)) {
    msg <- "The parameter section must be specified."
    logger::log_error(msg)
    stop(msg)
  }
  
  idx <- (dt$project == project) & (dt$section == glue::glue("{project}::{section}"))

  if (all(idx == FALSE)) {
    msg <- glue::glue("project-section-combination -{project}::{section}- does not exist in the project plan.")
    logger::log_error(msg)
    stop(msg)
  }
  task_label <- as.character(glue::glue(task_label))
  ret <- h.collapse_time_lines(dt[idx], group_by = c("project", "section"), task_label = task_label)
  ret$section <- task_label
  
  
  data.table::rbindlist(list(dt[!idx], ret), fill = TRUE)
}

h.collapse_project <- function(dt, project, task_label) {
  if (missing(project)) {
    msg <- "Parameter project must be specified."
    logger::log_error(glue::glue("{msg} Valid entries for example are: "))
    logger::log_error(h.capture_table(head(unique(dt$project))))
    stop(msg)
  }
  idx <- dt$project == project
  
  if (all(idx == FALSE)) {
    msg <- glue::glue("The project -{project}- does not exist in the project plan.")
    logger::log_error(glue::glue("{msg} Valid entries for example are: "))
    logger::log_error(h.capture_table(head(unique(dt$project))))
    stop(msg)
  }
  task_label = as.character(glue::glue(task_label))
  ret <- h.collapse_time_lines(dt[idx], group_by = "project", task_label)
  ret$section <- task_label
  
  data.table::rbindlist(list(dt[!idx], ret), fill = TRUE)  
}


h.collapse_time_lines <- function(dt, group_by, task_label) {
  ret <- dt
  
  all_complete <- FALSE
  not_aborted = !ret$aborted
  if (all(ret$progress[not_aborted] == 100)) {
    if (all(ret$aborted)) {
      logger::log_info(glue::glue("Collapsing time lines if all tasks are aborted, will present them as completed. This was done for {task_label}"))
    }
    all_complete <- TRUE
  }
  
  min_dist_end_to_deadline <- function(deadline, dist_end_to_deadline) {
    min_dline <- suppressWarnings(min(deadline, na.rm = TRUE))
    if (is.infinite(min_dline)) {
      return(difftime(NA, NA))
    }
    
    idx_min_dline <- which(min_dline == deadline)
    min(dist_end_to_deadline[idx_min_dline])
  }
  
  if (all(ret$aborted)) {
    ret <- with(NULL, ret[ , .(
      time_start = min(time_start),
      time_end = min(time_start),
      aborted = T
    ), by = group_by])
  } else {
    ret <- with(NULL, ret[ not_aborted, .(
      time_start = min(time_start),
      time_end = max(time_end),
      deadline = suppressWarnings(min(deadline, na.rm = TRUE)),
      dist_end_to_deadline = min_dist_end_to_deadline(deadline, dist_end_to_deadline),
      aborted = F
    ), by = group_by])
    ret$deadline[is.infinite(ret$deadline)] <- NA
  }
  
  ret$progress <- 0
  if (all_complete) {
    ret$progress <- 100
  }
  
  ret$task <- as.character(task_label)
  ret$waiting <- FALSE
  ret$resource <- "collapsed"
  
  ret
}

h.set_deadline_for_waiting_tasks <- function(dt_ref) {
  with(NULL, dt_ref[waiting & is.na(deadline) == TRUE, deadline := time_end])
}


h.calc_dist_to_deadline <- function(date_vec, deadline_vec) {
  raw_dist <- deadline_vec - date_vec
  overdue <- as.integer(raw_dist < 0)
  nmb_weekends <- floor(abs(raw_dist) / 7)
  nmb_weekends <- nmb_weekends +
    (1 - overdue) * (lubridate::wday(deadline_vec) < lubridate::wday(date_vec)) +
    overdue * (lubridate::wday(deadline_vec) > lubridate::wday(date_vec))
  nmb_weekends <- -1 * overdue * nmb_weekends + abs(overdue - 1) * nmb_weekends

  raw_dist - 2 * nmb_weekends
}


h.calc_end_to_deadline <- function(df) {
  idx <- !is.na(df$deadline) 

  if (any(idx)) {
    with(NULL, df[idx, dist_end_to_deadline := h.calc_dist_to_deadline(time_end, deadline)])
  } else {
    with(NULL, df[, dist_end_to_deadline := NA])
  }
}

h.turn_weekend_day_to_monday <- function(day) {
  if (lubridate::wday(day) == 7) {
    logger::log_debug(glue::glue("Change the saturday {day} to monday {day + 2}"))
    day <- day + 2
  } else if (lubridate::wday(day) == 1) {
    logger::log_debug(glue::glue("Change the sunday {day} to monday {day + 1}"))
    day <- day + 1
  }
  day
}

h.exclude_weekends <- function(start, end) {
  if (end < start) {
    msg <- glue::glue("Specified end time {end} is before the start time {start}")
    logger::log_error(msg)
    stop(msg)
  }

  shift <- h.turn_weekend_day_to_monday(start) - start
  if (shift > 0) {
    logger::log_warn(glue::glue("start {start} is on a weekend. Shift end {end} by {shift} day(s)."))
    logger::log_debug(glue::glue("In order to correctly exclude weekends, also start {start} is shifted by {shift} day(s) locally but not in the project plan"))
    start <- start + shift
    end <- end + shift
  }

  logger::log_debug(glue::glue("Exclude weekends between {start} and {end}"))
  nmb_workdays <- as.integer(end - start)
  nmb_workweeks <- floor(nmb_workdays / 5)
  nmb_days_remain <- nmb_workdays %% 5

  end <- start + 7 * nmb_workweeks + nmb_days_remain

  if (lubridate::wday(end) %in% c(1, 7)) {
    # if we stop working on saturday we actually have to work till monday
    # if we stop working on sunday we actually have to work till tuesday
    logger::log_debug(glue::glue("The task ends on {end} which is a weekend. Hence, the actual end is 2 days on {end + 2}"))
    end <- end + 2
  } else if (nmb_workweeks == 0 && lubridate::wday(end) < lubridate::wday(start)) {
    # start this friday and end next monday. 
    # in this cased nmb_workweeks is 0 but weekend isn't yet excluded.
    end <- end + 2  
  }
  end
}

h.calculate_end_time <- function(earliest_start_time, est_days, fixed_end_date) {
  if (!is.na(as.character(fixed_end_date))) {
    end <- fixed_end_date
    end <- h.turn_weekend_day_to_monday(end)
    return(end)
  }

  end <- earliest_start_time + est_days
  h.exclude_weekends(earliest_start_time, end)
}


h.calculate_time_lines_at <- function(dt_ref, row, today) {
  logger::log_debug(glue::glue("Calculate time lines for row -{row}-"))
  logger::log_debug(h.capture_table(dt_ref[row, ]))
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

  logger::log_debug(glue::glue("Try to calculate earliest start time for row -{row}- based on prior tasks"))
  logger::log_debug(h.capture_table(prior_tasks))
  while (is.na(earliest_start_time)) {
    prior_tasks <- with(NULL, prior_tasks[is.na(time_end)])
    na_id <- prior_tasks$id[1]
    logger::log_info(glue::glue("Nonsorted entry -{dt_ref$id[row]}- must follow after -{na_id}-"))

    first_na_idx <- which(dt_ref$id == na_id)[1]
    h.calculate_time_lines_at(dt_ref, first_na_idx, today)

    prior_tasks <- with(NULL, dt_ref[id %in% ids_prior])
    logger::log_debug(glue::glue("Try to calculate earliest start time for row -{row}- based on prior tasks"))
    logger::log_debug(h.capture_table(prior_tasks))
    earliest_start_time <- max(prior_tasks$time_end)
  }
  logger::log_debug(glue::glue("Earliest start time found for row -{row}-: {earliest_start_time}"))

  end <- h.calculate_end_time(earliest_start_time, dt_ref$est_days[row], dt_ref$fixed_end_date[row])

  with(NULL, dt_ref[row, time_start := earliest_start_time])
  with(NULL, dt_ref[row, time_end := end])
  with(NULL, dt_ref[row & waiting == TRUE, time_end := pmax(time_end, today)])
  
  logger::log_debug(glue::glue("Timelines for the current row -{row}-"))
  logger::log_debug(h.capture_table(dt_ref[row]))

  if (end < earliest_start_time) {
    logger::log_warn("-time_start- is before -time_end-")
    logger::log_warn(h.capture_table(dt_ref[row]))
  }
}
