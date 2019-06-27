#' Prepares a raw project plan for further processing
#'
#' This function is usually called first, for instance after a raw project
#' plan was imported from an speadsheet.
#'
#' @param df Essential columns that must be provided are
#'
#'   task: description of the task, e.g. write architecture
#'
#'   resource: the resource that is working on that task
#'
#'   id: an identifier for the task. used to declare dependencies
#'
#'   section: a section is usually assigned to a set of tasks
#'
#'   project: a project is usually assigned to a set of sections
#'
#'   depends_on: a comma separated list of id's that the current task depends on or NA
#'
#'   start: a date when the task starts, NA or 'TODAY'. At the beginning of a project, this is usually NA and \link{calculate_time_lines} tries to calculate an explicit date
#'
#'   end: a date when the task ends or an integer (representing number of days the task will take to be completed) or 'WAIT'
#'
#'   progress: number between 0 and 100, indicating the progress of the task
#'
#'   deadline: NA or a date when the task must be completed
#'   
#' @param date_origin Reference date in format YYYY-mm-dd for converting an integer to a date. 
#'   For dates (post-1901) from Windows Excel origin should by 1899-12-30 (is the default value). 
#'   For dates (post-1901) from Mac Excel origin should by 1904-01-01. 
#'
#' @return \code{data.table} with columns preprocessed for calculating time lines with \link{calculate_time_lines}.
#'
#' @details The column start can contain the word 'TODAY' which is replaced by the current date.
#'   The column end can contain the word 'WAIT' which marks the task as a waiting task and internally it is assumed that the
#'   task ends today.
#'
#' @seealso \link{import_xlsx}, \link{calculate_time_lines}
#'
#' @export
wrangle_raw_plan <- function(df, date_origin = "1899-12-30") {
  h.log_start()
  
  df <- data.table::data.table(df)

  df <- h.rd_select_cols(df)
  df <- h.rd_remove_unnessary_rows(df)

  df <- h.rd_fill_with_default(df, "project", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "section", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "id", "NOT_SPECIFIED_", log_filling = FALSE, create_unique_entries = TRUE)
  df <- h.rd_fill_with_default(df, "est_duration", "1")
  df <- h.rd_fill_with_default(df, "status", "", log_filling = FALSE)
  df <- h.rd_fill_with_default(df, "resource", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "task", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "progress", "0")
  df$progress <- as.numeric(df$progress)

  df <- h.rd_preprocess_depends_on_column(df)
  df <- h.rd_preprocess_start_column(df, date_origin = date_origin)
  df <- h.rd_preprocess_end_column(df, date_origin = date_origin)
  df <- h.rd_preprocess_est_duration_column(df)
  df <- h.rd_preprocess_status_column(df)
  df <- h.rd_preprocess_deadline_column(df, date_origin = date_origin)
  h.rd_check_project_section_id_unique(df)

  df <- h.rd_make_id_unique_within_project(df)
  h.rd_check_start_time_available(df)
  h.rd_check_id_deps(df)

  h.log_end()
  
  df
}


h.replace_TAG_PREVIOUS <- function(df, col) {
  if (nrow(df) <= 1) {
    return(df)
  }
  
  idx <- which(purrr::map_lgl(df[[col]], ~ any(h.split_comma_list(.x) == h.TAG_PREVIOUS)))

  if (any(idx == 1, na.rm = TRUE)) {
    msg <- glue::glue("The tag -{h.TAG_PREVIOUS}- is not allowed as a first entry in column -{col}- in the first project. Ignore this entry")
    h.log_rows(df, idx[1], msg, warn_columns = c("project", "section", "id", col))
    idx <- idx[-1]
  }

  ret <- data.table::copy(df)
  ret[[col]][idx] <- purrr::map_chr(
    idx,
    function(row) {
      dep_ids <- h.split_comma_list(ret[[col]][row])
      if (any(dep_ids == h.TAG_PREVIOUS)) {
        prev_id <- paste0(ret[["project"]][row - 1], h.SEPERATOR, ret[["id"]][row - 1])
        dep_ids <- c(dep_ids[dep_ids != h.TAG_PREVIOUS], prev_id)
      }
      h.comma_list(dep_ids)
    })
  ret
}

h.replace_section_with_ids <- function(df, col) {
  h.log_start()
  
  project_section <- paste(df$project, df$section, sep = h.SEPERATOR)
  project_id <- paste(df$project, df$id, sep = h.SEPERATOR)
  idx <- purrr::map(df[[col]], ~ which(project_section == .x))
  replacement_ids <- purrr::map(idx, ~ h.comma_list(project_id[.x]))
  rows <- which(!is.na(unlist(replacement_ids)))
  df[[col]][rows] <- unlist(replacement_ids[rows])
  
  h.log_end()
  df
}


h.rd_preprocess_depends_on_column <- function(df) {
  df <- h.replace_TAG_PREVIOUS(df, "depends_on")
  df <- h.replace_section_with_ids(df, "depends_on")
}

h.rd_check_id_deps <- function(df) {
  h.log_start()
  
  id_col <- df$id
  prior_ids <- unique(sort(unlist(df$prior_ids)))

  unknow_ids <- setdiff(prior_ids, id_col)
  if (length(unknow_ids) > 0) {
    futile.logger::flog.warn(
      glue::glue("Dependencies to unknown ids were specified. Please check that the following ids are correct: "), data.frame(unknow_ids = unknow_ids),
      capture = TRUE
    )
  }
  
  h.log_end()
  
}


h.rd_check_start_time_available <- function(df) {
  h.log_start()
  
  futile.logger::flog.info("Check that the start time is at least implicitly defined.")


  # var is only for logger
  h.comma_list <- Vectorize(h.comma_list)
  df_log <- data.table::copy(df)
  cols <- c("depends_on", "start")
  df_log[, (cols) := lapply(.SD, h.comma_list), .SDcols = cols]

  idx <- with(df, is.na(fixed_start_date) & is.null(prior_ids))
  h.log_rows(
    df_log,
    idx,
    warn_msg = glue::glue("Missing explicit or implicit start time for the following entries")
  )
  
  h.log_end()
  
}

h.rd_make_id_unique_within_project <- function(df) {
  h.log_start()
  
  date_min <- function(v) {
    if (all(is.na(v))) {
      # strange behaviour if all NA then min (with na.rm = TRUE) will return Inf as expected, but NA is displayed.
      return(lubridate::as_date(NA))
    }
    min(v, na.rm = TRUE)
  }

  date_max <- function(v) {
    if (all(is.na(v))) {
      # strange behaviour if all NA then max (with na.rm = TRUE) will return -Inf as expected, but NA is displayed.
      return(lubridate::as_date(NA))
    }
    max(v, na.rm = TRUE)
  }

  df <- data.table::data.table(df)
  ret <- with(NULL, df[, .(
    depends_on = h.combine_comma_list_cols(depends_on),
    start = h.combine_comma_list_cols(start),
    prior_ids = h.combine_comma_list_cols(depends_on, start),
    section = h.combine_comma_list_cols(section),
    resource = h.combine_comma_list_cols(resource),
    task = h.combine_comma_list_cols(task),
    progress = mean(progress),
    deadline = date_min(deadline),
    fixed_start_date = date_min(fixed_start_date),
    fixed_end_date = date_max(fixed_end_date),
    est_days = sum(est_days, na.rm = TRUE),
    waiting = any(waiting),
    aborted = any(aborted),
    nmb_combined_entries = .N
  ),
  by = .(project, id)
  ])

  combined_entries <- with(NULL, ret[nmb_combined_entries > 1])
  if (nrow(combined_entries) > 0) {
    futile.logger::flog.info(
      "Some id-entries were combined (within the project) into one entry, this means for instance that the estimated days are summed up.",
      list(
        after_combining = combined_entries
      ),
      capture = TRUE
    )
  }

  add_prefix_preserve_other_projects <- function(prefix, str) {
    if (is.na(str)) {
      return(NA_character_)
    }

    v <- h.split_comma_list(str)

    idx <- which(!grepl(h.SEPERATOR, v))
    v[idx] <- paste(prefix, v[idx], sep = h.SEPERATOR)
    # strange effect. without sort some of the testcase failed but only during devtools::check!!!
    list(sort(v))
  }
  vadd_prefix_preserve_other_projects <- Vectorize(add_prefix_preserve_other_projects)


  with(
    NULL,
    ret[, ":="(section = paste(project, section, sep = h.SEPERATOR),
    id = paste(project, id, sep = h.SEPERATOR),
    depends_on = vadd_prefix_preserve_other_projects(project, depends_on),
    start = vadd_prefix_preserve_other_projects(project, start),
    prior_ids = vadd_prefix_preserve_other_projects(project, prior_ids))]
  )
  ret <- h.unique(ret)
  
  h.log_end()
  
  ret
}

h.unique <- function(dt) {
  h.log_start()
  
  # since data.table 1.12.0 unique does not work anymore if a column is a list 
  v_to_comma_list <- Vectorize(h.combine_comma_list_cols)
  with(NULL,
       dt[, ":="(
         depends_on = v_to_comma_list(depends_on),
         start = v_to_comma_list(start),
         prior_ids = v_to_comma_list(prior_ids)
       )])
  dt <- unique(dt)
  
  to_list <- function(str) {
    if (is.na(str)) {
      return(NA_character_)
    }
    list(h.split_comma_list(str))
  }
  v_to_list <- Vectorize(to_list)
  ret <- with(NULL,
       dt[, ":="(
         depends_on = v_to_list(depends_on),
         start = v_to_list(start),
         prior_ids = v_to_list(prior_ids)
       )])
  
  h.log_end()
  
  ret
}



h.rd_check_project_section_id_unique <- function(df) {
  h.log_start()
  #dont want that data.table::setorder change the order of df
  df_copy <- data.table::copy(df)
  
  id_in_multiple_sections <- with(NULL, unique(df_copy[, .(project, id, section)]))
  id_in_multiple_sections <- with(NULL, id_in_multiple_sections[, .(n = .N), by = c("project", "id")])
  id_in_multiple_sections <- with(NULL, id_in_multiple_sections[n > 1])
  id_in_multiple_sections <- id_in_multiple_sections$id

  with(NULL, data.table::setorder(df_copy, project, id, section))
  h.log_rows(
    df_copy,
    df_copy$id %in% id_in_multiple_sections,
    warn_msg = glue::glue("The same id -{h.comma_list(id_in_multiple_sections)}- used in different 'sections' of the same 'project' is probably an error")
  )
  
  h.log_end()
  
}


h.rd_preprocess_deadline_column <- function(df, date_origin) {
  h.log_start()
  
  df$raw_deadline <- df$deadline
  df$deadline <- h.convert_numeric_date(df$deadline, date_origin = date_origin)
  df$deadline <- suppressWarnings(lubridate::ymd(df$deadline))
  
  idx <- !is.na(df$deadline)
  if (any(idx)) {
    df$deadline[idx] <- lubridate::as_date(sapply(df$deadline[idx], h.turn_weekend_day_to_monday))
  }

  h.log_rows(
    df,
    with(df, xor(!is.na(deadline), !is.na(raw_deadline))),
    warn_msg = glue::glue("Entries in column 'deadline' must be a ymd-format"),
    error = TRUE
  )
  
  h.log_end()
  df
}

h.rd_preprocess_status_column <- function(df) {
  h.log_start()
  
  df$status <- toupper(df$status)
  df$waiting <- df$status == "AWAIT"
  df$aborted <- df$status == "ABORTED"
  df$status <- NULL
  
  h.log_end()
  
  df
}

h.rd_preprocess_est_duration_column <- function(df) {
  h.log_start()
  
  df$est_days <- suppressWarnings(as.numeric(df$est_duration))
  
  idx <- is.na(df$est_days)
  if (any(idx)) {
    df$est_days[idx] <- 1
  }
  
  h.log_rows(
    df,
    idx,
    warn_msg = glue::glue("Entries in column 'est_duration' must be integer. Replace those entries by 1."),
    warn_columns = c("project", "section", "id", "est_duration")
  )
  df$est_duration <- NULL
  
  h.log_end()
  
  df
}


h.rd_preprocess_end_column <- function(df, date_origin) {
  h.log_start()
  
  df$end <- h.convert_numeric_date(df$end, date_origin = date_origin)
  df$fixed_end_date <- suppressWarnings(lubridate::ymd(df$end))
  df$end <- NULL
  
  h.log_end()
  
  df
}

h.convert_numeric_date <- function(v_str, date_origin) {
  v_num <- suppressWarnings(as.numeric(v_str))
  num_idx <- !is.na(v_num)
  v_str[num_idx] <- as.character(as.Date(v_num[num_idx], origin = date_origin))
  v_str
}

h.rd_preprocess_start_column <- function(df, date_origin) {
  h.log_start()
  
  df <- h.replace_TAG_PREVIOUS(df, "start")
  df <- h.replace_section_with_ids(df, "start")
  
  TODAY <- as.character(lubridate::as_date(lubridate::now()))

  idx <- is.na(df$depends_on) & is.na(df$start)
  if (any(idx)) {
    df$start[idx] <- "TODAY"
    h.log_rows(
      df,
      idx,
      warn_msg = glue::glue("Some entries have empty -depends_on- AND -start- entries. Set -start- to 'TODAY'")
    )
  }

  idx <- df$start == "TODAY"
  if (any(idx, na.rm = TRUE)) {
    futile.logger::flog.info(glue::glue("Convert 'TODAY' in column -start- to the current date -{TODAY}-"))  
  }
  
  df$start <- h.convert_numeric_date(df$start, date_origin = date_origin)
  df$start[idx] <- TODAY
  df$fixed_start_date <- suppressWarnings(lubridate::ymd(df$start))
  df$start[!is.na(df$fixed_start_date)] <- NA
  
  h.log_end()
  
  df
}

h.rd_fill_with_default <- function(df, colname, def, log_filling = TRUE, create_unique_entries = FALSE) {
  h.log_start()
  
  idx <- is.na(df[[colname]])

  if (log_filling) {
    h.log_rows(
      df,
      idx,
      warn_msg = glue::glue("Some entries in column -{colname}- are not specified. Set those entries to -{def}-.")
    )
  }
  if (any(idx)) {
    idx <- which(idx)
    if (create_unique_entries) {
      def <- paste0(def, idx)
    }
    df[[colname]][idx] <- def
  }
  
  h.log_end()
  
  df
}

h.rd_remove_unnessary_rows <- function(df) {
  h.log_start()
  
  futile.logger::flog.info(glue::glue("Remove rows where project, section, id, start, end, resource, task are empty"))

  discard <- with(df, is.na(project) & is.na(section) & is.na(id) & is.na(start) & is.na(end) & is.na(resource) & is.na(task))

  futile.logger::flog.debug(glue::glue("Remove {sum(discard)} rows"))

  ret <- df[!discard, ]
  
  h.log_end()

  ret  
}

h.rd_select_cols <- function(df) {
  h.log_start()
  
  cols <- c("project", "section", "id", "depends_on", "start", "end", "est_duration", "status", "resource", "task", "progress", "deadline")
  futile.logger::flog.info(glue::glue("Select the necessary columns -{h.comma_list(cols)}-"))

  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    futile.logger::flog.warn(glue::glue("Create missing column(s): -{h.comma_list(missing_cols)}-"))

    new_cols <- data.table(matrix(NA, nrow = nrow(df), ncol = length(missing_cols)))
    names(new_cols) <- missing_cols
    df <- cbind(df, new_cols)
  }
  ret <- df[, lapply(.SD, as.character), .SDcols = cols]
  
  h.log_end()
  
  ret  
}
