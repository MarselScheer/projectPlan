#' Calculate explit start and end dates for the given project plan.
#'
#' Usually this function is called after a raw plan with tasks was prepared by \link{wrangle_raw_plan}.
#' It takes a set of tasks and their estimated duration as well as
#' dependencies between those tasks. This implicitly defines start and end dates
#' for each task and this function calculates the corresponding explicit
#' start and end times
#'
#' @param df A \code{data.frame} preprocessed by \link{wrangle_raw_plan}.
#' @return A \code{data.table} object with explicit start and end times for every (grouped) task
#' \describe{
#'   \item{misc. columns}{columns from df}
#'   \item{time_start/end}{calculated start- and end-time of a task}
#'   \item{dist_end_to_deadline}{number of workdays (weekends exludede) from the calculated end-time 
#'                               to the specified deadline}
#' }
#'
#' @seealso \link{wrangle_raw_plan}, \link{gantt_by_sections}
#' @examples 
#' raw_plan <- import_xlsx(system.file("template","projects.xlsx", package = "projectPlan"))
#' pre_plan <- wrangle_raw_plan(raw_plan)
#' calculate_time_lines(pre_plan)
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

#' Combines all tasks of a project to one entry
#' 
#' This function is especially helpful if one wants to show a complete project
#' only as one entry in the Gantt-chart
#' @param dt time lines processed by \link{calculate_time_lines}
#'
#' @param projects that will be combined to one entry each
#' @param task_label the label that is used for the corresonding collapsed entry
#'
#' @return dt but all entries for the provided projects are collapsed to one entry. 
#'         Start and end time for the collapsed entry are the minimum and maximum
#'         start and end times of the single tasks and deadline is set to the minimum
#'         deadline (if available).
#' @export
collapse_projects <- function(dt, projects, task_label = "{project} collapsed") {
  ret <- dt
  for (p in unique(projects)) {
    ret <- h.collapse_project(ret, p, task_label)
  }
  ret
}

#' Combines all sections that do not contain an 'uncompleted' task to one entry
#' 
#' This function is especially helpful if one wants to show a completed sections
#' only as one entry in the Gantt-chart
#' @param dt time lines processed by \link{calculate_time_lines}
#'
#' @return dt but all sections that do not contain an 'uncompleted' task are collapsed to one entry. 
#'         Start and end time for the collapsed entry are the minimum and maximum
#'         start and end times of the single tasks and deadline is set to the minimum
#'         deadline (if available).
#' @export
collapse_complete_sections <- function(dt) {
  
  is.completed <- function(progress, aborted) {
    all(progress[!aborted] == 100)
  }
  
  complete_sections <- with(NULL, dt[, .(complete = is.completed(progress, aborted)), by = "section"][complete == TRUE])
  for (proj_sec in complete_sections$section) {
    proj_sec <- unlist(strsplit(proj_sec, "::"))
    dt <- collapse_section(dt, project = proj_sec[1], section = proj_sec[2], task_label = "{project}::{section} completed")
  }
  dt
}

#' Combines all projects that do not contain an 'uncompleted' task to one entry
#' 
#' This function is especially helpful if one wants to show a completed project
#' only as one entry in the Gantt-chart
#' @param dt time lines processed by \link{calculate_time_lines}
#'
#' @return dt but all projects that do not contain an 'uncompleted' task are collapsed to one entry. 
#'         Start and end time for the collapsed entry are the minimum and maximum
#'         start and end times of the single tasks and deadline is set to the minimum
#'         deadline (if available).
#' @export
collapse_complete_projects <- function(dt) {
  
  is.completed <- function(progress, aborted) {
    all(progress[!aborted] == 100)
  }
  
  complete_projects <- with(NULL, dt[, .(complete = is.completed(progress, aborted)), by = "project"][complete == TRUE])
  collapse_projects(dt, projects = complete_projects$project, task_label = "{project} completed")
}


#' Combines all tasks of a section to one entry
#' 
#' This function is especially helpful if one wants to show a complete section
#' only as one entry in the Gantt-chart
#' @param dt time lines processed by \link{calculate_time_lines}
#'
#' @param project that conatins the section that should be collapsed 
#' @param section that will be collapsed to one entry
#' @param task_label the label that is used for the corresonding collapsed entry
#'
#' @return dt but all entries for the provided section in the provided project are collapsed to one entry. 
#'         Start and end time for the collapsed entry are the minimum and maximum
#'         start and end times of the single tasks and deadline is set to the minimum
#'         deadline (if available).
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
    logger::log_error("{msg} Valid entries for example are: ")
    logger::log_error(h.capture_table(utils::head(unique(dt$project))))
    stop(msg)
  }
  idx <- dt$project == project
  
  if (all(idx == FALSE)) {
    msg <- glue::glue("The project -{project}- does not exist in the project plan.")
    logger::log_error("{msg} Valid entries for example are: ")
    logger::log_error(h.capture_table(utils::head(unique(dt$project))))
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
      logger::log_info("Collapsing time lines if all tasks are aborted, will present them as completed. This was done for {task_label}")
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
    logger::log_debug("Change the saturday {day} to monday {day + 2}")
    day <- day + 2
  } else if (lubridate::wday(day) == 1) {
    logger::log_debug("Change the sunday {day} to monday {day + 1}")
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
    logger::log_warn("start {start} is on a weekend. Shift end {end} by {shift} day(s).")
    logger::log_debug("In order to correctly exclude weekends, also start {start} is shifted by {shift} day(s) locally but not in the project plan")
    start <- start + shift
    end <- end + shift
  }

  logger::log_debug("Exclude weekends between {start} and {end}")
  nmb_workdays <- as.integer(end - start)
  nmb_workweeks <- floor(nmb_workdays / 5)
  nmb_days_remain <- nmb_workdays %% 5

  end <- start + 7 * nmb_workweeks + nmb_days_remain

  if (lubridate::wday(end) %in% c(1, 7)) {
    # if we stop working on saturday we actually have to work till monday
    # if we stop working on sunday we actually have to work till tuesday
    logger::log_debug("The task ends on {end} which is a weekend. Hence, the actual end is 2 days on {end + 2}")
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
  
  # TODO: more comments/debug-statements
  logger::log_debug("Calculate time lines for row -{row}-")
  logger::log_debug(h.capture_table(dt_ref[row, ]))
  if (!is.na(dt_ref$time_start[row]) & !is.na(dt_ref$time_end[row])) {
    return()
  }
  if (!is.na(dt_ref$fixed_start_date[row])) {
    dt_ref[row, unscheduled := FALSE]
  }

  ids_prior <- unlist(dt_ref$prior_ids[row])
  prior_tasks <- with(NULL, dt_ref[id %in% ids_prior, ])
  if (nrow(prior_tasks) == 0 && is.na(dt_ref$fixed_start_date[row])) {
    logger::log_debug("Initialize fixed_start_date because no prior-tasks and no explicit start-date exist.
                      Furthermore, such a task is assumed to be in status -UNSCHEDULED-")
    with(NULL, dt_ref[row, fixed_start_date := lubridate::as_date(lubridate::now())])
    with(NULL, dt_ref[row, unscheduled := TRUE])
  }
  fsd <- dt_ref$fixed_start_date[row]

  # seems to be some bug because is.na(earliest_start_time) returns FALSE?!?
  if (is.na(as.character(fsd))) {
    earliest_start_time <- max(prior_tasks$time_end)
  } else {
    earliest_start_time <- fsd
  }

  logger::log_debug("Try to calculate earliest start time for row -{row}- based on prior tasks")
  logger::log_debug(h.capture_table(prior_tasks))
  
  while (is.na(earliest_start_time)) {
    prior_tasks <- with(NULL, prior_tasks[is.na(time_end)])
    na_id <- prior_tasks$id[1]
    logger::log_info("Nonsorted entry -{dt_ref$id[row]}- must follow after -{na_id}-")

    first_na_idx <- which(dt_ref$id == na_id)[1]
    h.calculate_time_lines_at(dt_ref, first_na_idx, today)

    prior_tasks <- with(NULL, dt_ref[id %in% ids_prior])
    logger::log_debug("Try to calculate earliest start time for row -{row}- based on prior tasks")
    logger::log_debug(h.capture_table(prior_tasks))
    earliest_start_time <- max(prior_tasks$time_end)
  }
  logger::log_debug("Earliest start time found for row -{row}-: {earliest_start_time}")

  end <- h.calculate_end_time(earliest_start_time, dt_ref$est_days[row], dt_ref$fixed_end_date[row])

  with(NULL, dt_ref[row, time_start := earliest_start_time])
  with(NULL, dt_ref[row, time_end := end])
  if (isTRUE(dt_ref$waiting[row])) {
    with(NULL, dt_ref[row, time_end := pmax(time_end, today)])  
  }
  
  if (is.na(dt_ref$fixed_start_date[row])) {
    dt_ref[row, unscheduled := h.is_unscheduled(dt_ref, dt_ref$depends_on[row], dt_ref$start[row])]  
  }

  if (isTRUE(dt_ref$user_unscheduled[row])) {
    dt_ref[row, unscheduled := TRUE]
  }
  
  logger::log_debug("Timelines for the current row -{row}-")
  logger::log_debug(h.capture_table(dt_ref[row]))

  if (end < earliest_start_time) {
    logger::log_warn("-time_start- is before -time_end-")
    logger::log_warn(h.capture_table(dt_ref[row]))
  }
}

#' Determines if a task is scheduled
#'
#' Helperfunction used during calculation of the time lines.
#' @param dt_ref A \code{data.frame} preprocessed by \link{wrangle_raw_plan}.
#' @param depends_on_ids ids of necessary task
#' @param start_ids ids of prior tasks that are not necessary
#'
#' @return TRUE if task is considered as unscheduled, otherwise FALSE.
#'   Note a that if completion of A and B are necessary to start C,
#'   then C is scheduled only if A and B are scheduled.
#'   On the other hand, if A and B are only prior tasks of C, but the
#'   dependency is only formulated via start-column, then C is considered
#'   as scheduled if A or B is scheduled.
#'   If C has prior tasks that are necessary and non-necessary, only the
#'   necessary tasks are used to determin the schedule-status.
h.is_unscheduled <- function(dt_ref, depends_on_ids, start_ids) {
  
  ret <- TRUE
  
  depends_on_ids <- unlist(depends_on_ids)
  necessary_prior_tasks <- with(NULL, dt_ref[id %in% depends_on_ids, ])
  if (nrow(necessary_prior_tasks) > 0) {
    logger::log_debug("Necessary prior tasks considered for determining schedule-status.
                      This are the necessary prior tasks:")
    logger::log_debug(h.capture_table(necessary_prior_tasks))
    ret <- ret && any(necessary_prior_tasks$unscheduled)
  } else {
    # non-necessary tasks are only considered if no necessary tasks exist
    start_ids <- unlist(start_ids)
    prior_tasks <- with(NULL, dt_ref[id %in% start_ids, ])
    if (nrow(prior_tasks) > 0) {
      logger::log_debug("Non-necessary prior tasks considered for determining schedule-status. 
                      This are the dependent prior tasks:")
      logger::log_debug(h.capture_table(prior_tasks))
      ret <- ret && all(prior_tasks$unscheduled)
    }
  }

  return(ret)
}
