#' Prepares a raw project plan for further processing
#'
#' This function is usually called first, for instance after a raw project
#' plan was imported from an speadsheet, see \link{import_xlsx}.
#'
#' @param df Essential columns that must be provided are
#'   \describe{
#'   \item{task}{description of the task, e.g. write architecture}
#'   \item{resource}{the resource that is working on that task}
#'   \item{id}{an identifier for the task. used to declare dependencies}
#'   \item{section}{a section is usually assigned to a set of tasks}
#'   \item{project}{a project is usually assigned to a set of sections}
#'   \item{depends_on}{a comma separated list of id's that the current task depends on or NA}
#'   \item{start}{a date when the task starts. Note, the start-date can be defined implicitly, see 
#'                the corresponding vignette.} 
#'   \item{end}{a date when the task ends}
#'   \item{est_duration}{number of estimated workdays this task will take}
#'   \item{status}{of the task, i.e. 'unscheduled', 'await', 'aborted'}
#'   \item{resource}{character for the resource that is allocated to the corresponding task}
#'   \item{progress}{number between 0 and 100, indicating the progress of the task}
#'   \item{deadline}{NA or a date when the task must be completed}
#'   \item{microtasks}{character that describes sub-tasks}
#'   \item{comments}{character with comments for the current task}
#'   }
#' @param date_origin Reference date in format YYYY-mm-dd for converting an integer to a date. 
#'   For dates (post-1901) from Windows Excel origin should by 1899-12-30 (is the default value). 
#'   For dates (post-1901) from Mac Excel origin should by 1904-01-01. 
#'
#' @return \code{data.table} with columns preprocessed for calculating time lines with \link{calculate_time_lines}.
#' \describe{
#'   \item{project}{copy from df where NA's are replaced by 'UNKNOWN'}
#'   \item{section}{copy from df where NA's are replaced by 'UNKNOWN'}
#'   \item{id}{copy from df where NA's are replaced by NOT_SPECIFIED_<number> 
#'             and a prefix for the project}
#'   \item{depends_on}{copy from df but adjusted for the project-prefix and PREVIOUS-tag}
#'   \item{start}{copy from df but adjusted for the project-prefix and PREVIOUS-tag}
#'   \item{prior_ids}{concatenation of depends_on and start}
#'   \item{resource}{copy from df where NA's are replaced by 'UNKNOWN'}
#'   \item{task}{copy from df where NA's are replaced by 'UNKNOWN'}
#'   \item{microtasks}{copy from df where NA's are replaced by '-'}
#'   \item{comments}{copy from df where NA's are replaced by '-'}
#'   \item{progress}{copy from df where NA's are replaced by 0}
#'   \item{deadline}{'copy' from df as date-object}
#'   \item{fixed_start_date}{'copy' of start from df as date-object}
#'   \item{fixed_end_date}{'copy' of end from df as date-object}
#'   \item{est_days}{copy of est_duration}
#'   \item{waiting/aborted/unscheduled}{TRUE/FALSE according to the status-column from df}
#'   \item{nmb_combined_entries}{number of tasks that were combined to one because of the same id}
#' }
#'
#' @seealso \link{import_xlsx}, \link{calculate_time_lines}
#' @examples
#' raw_plan <- import_xlsx(system.file("template","projects.xlsx", package = "projectPlan"))
#' wrangle_raw_plan(raw_plan)
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
  df <- h.rd_fill_with_default(df, "microtasks", "-")
  df <- h.rd_fill_with_default(df, "comments", "-")
  df$progress <- as.numeric(df$progress)

  df <- h.rd_preprocess_status_column(df)
  df <- h.rd_preprocess_depends_on_column(df)
  df <- h.rd_preprocess_start_column(df, date_origin = date_origin)
  df <- h.rd_preprocess_end_column(df, date_origin = date_origin)
  df <- h.rd_preprocess_est_duration_column(df)
  df <- h.rd_preprocess_deadline_column(df, date_origin = date_origin)
  h.rd_check_project_section_id_unique(df)

  df <- h.rd_make_id_unique_within_project(df)
  h.rd_check_start_time_available(df)
  h.rd_check_id_deps(df)

  h.log_end()
  
  df
}


#' Replace TAG_PREVIOUS with the id of the previous row
#'
#' @param df contains columns project, id and the column provided by col
#' @param col name of a column in df. Entries must be like "a,b,c"
#'
#' @return df where TAG_PREVIOUS in col is replace with the id of the previous row
h.replace_TAG_PREVIOUS <- function(df, col) {
  if (nrow(df) <= 1) {
    return(df)
  }
  
  # locate the rows that contain TAG_PREVIOUS in column col, in order to know where it needs
  # to be replaced
  idx <- which(purrr::map_lgl(df[[col]], ~ any(h.split_comma_list(.x) == h.TAG_PREVIOUS)))

  
  # TAG_PREVIOUS in the first row does not make sense
  if (any(idx == 1, na.rm = TRUE)) {
    msg <- glue::glue("The tag -{h.TAG_PREVIOUS}- is not allowed as a first entry in column -{col}- in the first project. Ignore this entry")
    h.log_rows(df, idx[1], msg, warn_columns = c("project", "section", "id", col))
    idx <- idx[-1]
  }

  # replace TAG_PREVIOUS in "row" with the id from "row - 1"
  ret <- data.table::copy(df)
  ret[[col]][idx] <- purrr::map_chr(
    idx,
    function(row) {
      # input is of form "a,b,c", split it into the single components and then replace TAG_PREVIOUS
      dep_ids <- h.split_comma_list(ret[[col]][row])
      if (any(dep_ids == h.TAG_PREVIOUS)) {
        prev_id <- paste0(ret[["project"]][row - 1], h.SEPERATOR, ret[["id"]][row - 1])
        dep_ids <- c(dep_ids[dep_ids != h.TAG_PREVIOUS], prev_id)
      }
      #input was "a,b,c" need to convert it back to this format
      h.comma_list(dep_ids)
    })
  ret
}

#' Replace a referenced section with ALL its ids
#'
#' User can specify a complete section as a dependency.
#' This function replaces this reference by all the
#' actual ids of the referenced section
#' 
#' @param df contains project, section, id and the column col
#' @param col where the replacement should happen
#'
#' @return df but an entry that references a section is replaced by
#'   ids of the referenced section. Note, this works if the corresponding
#'   entry consists only of ONE section. This means that only one
#'   entry is allowed
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


#' Replace TAG_PREVIOUS and section-references by the corresponding ids
#'
#' @param df project-plan
#'
#' @return dt references in the depends_on column were updated
h.rd_preprocess_depends_on_column <- function(df) {
  df <- h.replace_TAG_PREVIOUS(df, "depends_on")
  df <- h.replace_section_with_ids(df, "depends_on")
}

#' Check that all ids of priors really exist in the id-column
#'
#' Warns if referenced ids of priors does not exist
#' @param df contains columns id and prior_ids
#'
#' @return NULL
h.rd_check_id_deps <- function(df) {
  h.log_start()
  
  id_col <- df$id
  prior_ids <- unique(sort(unlist(df$prior_ids)))

  unknow_ids <- setdiff(prior_ids, id_col)
  if (length(unknow_ids) > 0) {
    logger::log_warn("Dependencies to unknown ids were specified. Please check that the following ids are correct: ")
    logger::log_warn(h.capture_table(data.frame(unknow_ids = unknow_ids)))
  }
  
  h.log_end()
  
}


#' Warns if no explicit start date nor prior ids are given
#'
#' @param df contains depends_on, start
#'
#' @return NULL
h.rd_check_start_time_available <- function(df) {
  h.log_start()
  
  logger::log_info("Check that the start time is at least implicitly defined.")


  # df_log is only nicer logging
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

#' Combine rows with the same id to one row
#'
#' Futhermore this functions extends the ids given by the user
#' with the corresponding project name which is the internally used id
#' 
#' @param df project-plan with depends_on, start, prior_ids, section,
#'   resource, task, progress, deadline, fixed_start_date, 
#'   fixed_end_date, est_days, waiting, aborted, unscheduled,
#'   microtasks, comments
#'
#' @return df where every internal id (project::id) is unique and 
#'   entries of depends_on, start, prior_ids where extend to follow
#'   project::id.
h.rd_make_id_unique_within_project <- function(df) {
  h.log_start()
  
  date_min <- function(v) {
    if (all(is.na(v))) {
      # strange behaviour if all NA then min (with na.rm = TRUE) will 
      # return Inf as expected, but NA is displayed.
      return(lubridate::as_date(NA))
    }
    min(v, na.rm = TRUE)
  }

  date_max <- function(v) {
    if (all(is.na(v))) {
      # strange behaviour if all NA then max (with na.rm = TRUE) will 
      # return -Inf as expected, but NA is displayed.
      return(lubridate::as_date(NA))
    }
    max(v, na.rm = TRUE)
  }

  # if project and id are the same for two rows they got combined
  # into one row:
  df <- data.table::data.table(df)
  ret <- with(NULL, df[, .(
    depends_on = h.combine_comma_list_cols(depends_on),
    start = h.combine_comma_list_cols(start),
    prior_ids = h.combine_comma_list_cols(depends_on, start),
    section = h.combine_comma_list_cols(section),
    resource = h.combine_comma_list_cols(resource),
    task = h.combine_comma_list_cols(task),
    microtasks = h.combine_comma_list_cols(microtasks),
    comments = h.combine_comma_list_cols(comments),
    progress = mean(progress),
    deadline = date_min(deadline),
    fixed_start_date = date_min(fixed_start_date),
    fixed_end_date = date_max(fixed_end_date),
    est_days = sum(est_days, na.rm = TRUE),
    waiting = any(waiting),
    aborted = any(aborted),
    unscheduled = any(unscheduled),
    nmb_combined_entries = .N
  ),
  by = .(project, id)
  ])

  # combining could be a mistake, so inform the user about what
  # was combined
  combined_entries <- with(NULL, ret[nmb_combined_entries > 1])
  if (nrow(combined_entries) > 0) {
    logger::log_info("Some id-entries were combined (within the project) into one entry, this means for instance that the estimated days are summed up.")
    logger::log_info(h.capture_table(combined_entries))
  }

  # TODO: separate function for extending ids to internal_ids
  add_prefix_preserve_other_projects <- function(prefix, str) {
    # internally the full id is the combination of project and id
    # the user provided (where h.SEPERATOR is used to paste them together). 
    # Here the entries in str are extended with
    # the corresponding prefix IF they do not already use h.SEPERATOR
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
    ret[, ":="(
      section = paste(project, section, sep = h.SEPERATOR),
      id = paste(project, id, sep = h.SEPERATOR),
      depends_on = vadd_prefix_preserve_other_projects(project, depends_on),
      start = vadd_prefix_preserve_other_projects(project, start),
      prior_ids = vadd_prefix_preserve_other_projects(project, prior_ids))
    ]
  )
  ret <- h.unique(ret)
  
  h.log_end()
  
  ret
}

#' Remove duplicated rows from a data.table 
#'
#' @param dt contains columns depends_on, start, prior_ids
#'
#' @details before unique is applied the columns depends_on, start, prior_ids
#'   are processed by normalizing them. For instance a rows where depends_on
#'   contain "a,b,c,b", "c,b,a,a" are logical the same, and they are both
#'   normalized to "a,b,c".
#' @return dt with duplicated rows removed and where entries in depends_on, 
#'   start and prior_ids converted from "a,b,c" to list(c("a", "b", "c"))
h.unique <- function(dt) {
  h.log_start()
  
  # since data.table 1.12.0 unique does not work anymore if a column is a list 
  v_to_comma_list <- Vectorize(h.combine_comma_list_cols)
  # want to remove duplicate elements in depends_on, start, prior_ids
  with(NULL,
       dt[, ":="(
         depends_on = v_to_comma_list(depends_on),
         start = v_to_comma_list(start),
         prior_ids = v_to_comma_list(prior_ids)
       )])
  dt <- unique(dt)
  
  to_list <- function(str) {
    # turn "a,b,c" into list(c("a", "b", "c")) for
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



#' Logs which id is used in different sections of the same project
#'
#' @param df contains project, section, id
#'
#' @return NULL
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
    warn_msg = glue::glue(
      "The same id -{h.comma_list(id_in_multiple_sections)}- ",
      "used in different 'sections' of the same 'project' ", 
      "is probably an error")
  )
  
  h.log_end()
  
}

#' Converts excel date for deadline into a R-date-object
#'
#' @param df contains deadline
#' @param date_origin date that is represented by the integer zero
#'
#' @return df with raw_deadline (original column) and deadline as date, where
#'   deadlines that are on a weekend are shifted to the next monday
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

#' Converts the status column of the raw project-plan into multiple columns
#'
#' @param df raw project-plan
#'
#' @return df with status-column replaced by logical columns waiting, aborted and unscheduled
h.rd_preprocess_status_column <- function(df) {
  h.log_start()
  
  df$status <- toupper(df$status)
  df$waiting <- df$status == "AWAIT"
  df$aborted <- df$status == "ABORTED"
  df$unscheduled <- df$status == "UNSCHEDULED"
  df$status <- NULL
  
  h.log_end()
  
  df
}

#' Converts est_duration-column of the raw project-plan into est_days
#'
#' @param df raw project-plan
#'
#' @return df with est_duration replaced by est_days, where missing est_days
#'   is set to 1 if est_duration cannot be converted to an integer
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


#' Converts the end column of the raw project-plan into a fixed end date
#'
#' @param df raw project-plan
#' @param date_origin reference date that is represented by the integer zero
#'
#' @return df with end column replaced by fixed_end_date
h.rd_preprocess_end_column <- function(df, date_origin) {
  h.log_start()
  
  df$end <- h.convert_numeric_date(df$end, date_origin = date_origin)
  df$fixed_end_date <- suppressWarnings(lubridate::ymd(df$end))
  
  idx <- !is.na(df$fixed_end_date) & df$waiting
  if (any(idx)) {
    h.log_rows(
      df,
      idx,
      warn_msg = glue::glue("Some entries have an -end_date- AND are in waiting status.")
    )
  }
  
  df$end <- NULL
  
  h.log_end()
  
  df
}

#' Where the vector contains integers they are converted into dates
#'
#' Excel usually stores dates as number of days since a certain
#' date_origin. But the references to other task are made by ids
#' so an implicitly defined date is preserved because the integers
#' which (stored as characters) are converted to dates which then
#' gets converted again back to character.
#' 
#' @param v_str vector of characters
#' @param date_origin date that is represented by the integer zero
#'
#' @return v_str where the entries that could be converted to integer are
#'   converted to "character-dates".
h.convert_numeric_date <- function(v_str, date_origin) {
  v_num <- suppressWarnings(as.numeric(v_str))
  num_idx <- !is.na(v_num)
  
  # Use character here because the other entries define references to ids
  # which should not get lost.
  v_str[num_idx] <- as.character(as.Date(v_num[num_idx], origin = date_origin))
  v_str
}

#' Replace references to ids and convert explict (numeric) dates to R-dates
#'
#' @param df project-plan
#' @param date_origin date that is represented by the integer zero
#'
#' @return df with entries in column start replaced by ids they reference to via
#'   TAG_PREVIOUS or complete section-references. Furthermore, an integer is
#'   converted to a date based on the date_origin. Note that character is used
#'   to be able to keep ids and dates in the same column. If no implicit or 
#'   explicit start date can be derived it is set to current date.
h.rd_preprocess_start_column <- function(df, date_origin) {
  h.log_start()
  
  df <- h.replace_TAG_PREVIOUS(df, "start")
  df <- h.replace_section_with_ids(df, "start")
  
  TODAY <- as.character(lubridate::as_date(lubridate::now()))

  # no implicit nor explicit definition of the start date for a 
  # task. Assume that it starts today and inform the user via logger
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
    logger::log_info("Convert 'TODAY' in column -start- to the current date -{TODAY}-")  
  }
  
  df$start <- h.convert_numeric_date(df$start, date_origin = date_origin)
  df$start[idx] <- TODAY
  df$fixed_start_date <- suppressWarnings(lubridate::ymd(df$start))
  df$start[!is.na(df$fixed_start_date)] <- NA
  
  h.log_end()
  
  df
}

#' Function replaces NA with a default
#'
#' @param df data.frame where replacement should happen
#' @param colname of the column where replacement should happen
#' @param def value used to replace NAs
#' @param log_filling TRUE if the replaced entries should be logged
#' @param create_unique_entries if the def-values should be modified so that
#'   the replacement is made with unique values
#'
#' @return df with NAs in colname replaced by def (may be extended so that new entries are unique)
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

#' Remove rows if certain columns of the raw project-plan contain only NAs
#'
#' @param df raw project-plan
#'
#' @return df with rows removed if project, section, id, start, end, resource
#'   and task is NA.
h.rd_remove_unnessary_rows <- function(df) {
  h.log_start()
  
  logger::log_info("Remove rows where project, section, id, start, end, resource, task are empty")

  discard <- with(df, is.na(project) & is.na(section) & is.na(id) & 
                    is.na(start) & is.na(end) & is.na(resource) & 
                    is.na(task))

  logger::log_debug("Remove {sum(discard)} rows")

  ret <- df[!discard, ]
  
  h.log_end()

  ret  
}

#' Selects certain columns (as character-columns) from the raw project-plan 
#'
#' @param df raw project-plan
#'
#' @return df but only the columns project, section, id, depends_on, 
#' start, end, est_duration, status, resource, task, progress,
#' deadline, microtasks and comments are kept. If one of the columns is missing, 
#' then it is added as a column containing only NAs.
h.rd_select_cols <- function(df) {
  h.log_start()
  
  cols <- c("project", "section", "id", "depends_on", "start", "end", 
            "est_duration", "status", "resource", "task", "progress", 
            "deadline", "microtasks", "comments")
  logger::log_info("Select the necessary columns -{h.comma_list(cols)}-")

  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    logger::log_warn("Create missing column(s): -{h.comma_list(missing_cols)}-")

    new_cols <- data.table(matrix(NA, nrow = nrow(df), ncol = length(missing_cols)))
    names(new_cols) <- missing_cols
    df <- cbind(df, new_cols)
  }
  ret <- df[, lapply(.SD, as.character), .SDcols = cols]
  
  h.log_end()
  
  ret  
}

