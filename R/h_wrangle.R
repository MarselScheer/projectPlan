# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Calls all necessary for wrangling a raw project plan
#' 
#' @return tibble with all columns preprocessed for calculating time lines
#' 
#' @export
wrangle_raw_plan <- function(df) {
  df <- data.table::data.table(df)
  
  df <- h.rd_select_cols(df)
  df <- h.rd_remove_unnessary_rows(df)
  
  df <- h.rd_fill_with_default(df, "project", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "section", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "id", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "end", "1")
  df <- h.rd_fill_with_default(df, "resource", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "task", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "progress", "0") 
  df[, progress := as.numeric(progress)]

  df <- h.rd_preprocess_start_column(df)
  df <- h.rd_preprocess_end_column(df)
  df <- h.rd_preprocess_deadline_column(df)
  h.rd_check_project_section_id_unique(df)

  df <- h.rd_make_id_unique_within_project(df)
  h.rd_check_start_time_available(df)
  h.rd_check_id_deps(df)

  df
}

h.rd_check_id_deps <- function(df) {
  id_col <- df$id
  prior_ids <- unique(sort(unlist(df$prior_ids)))
  
  unknow_ids <- setdiff(prior_ids, id_col)
  if (length(unknow_ids) > 0) {
    futile.logger::flog.warn(
      glue::glue("Dependencies to unknown ids were specified. Please check that the following ids are correct: "), data.frame(unknow_ids = unknow_ids),
      capture = TRUE
    )
  }
}


h.rd_check_start_time_available <- function(df) {
  futile.logger::flog.info("Check that the start time is at least implicitly defined.")

  
  # var is only for logger
  h.comma_list = Vectorize(h.comma_list)
  df_log <- data.table::copy(df)
  cols <- c("depends_on", "start")
  df_log[, (cols) := lapply(.SD, h.comma_list), .SDcols = cols]

  idx <- with(df, is.na(fixed_start_date) & is.null(prior_ids))
  h.log_rows(
    df_log,
    idx,
    warn_msg = glue::glue("Missing explicit or implicit start time for the following entries")
  )
}

h.rd_make_id_unique_within_project <- function(df) {
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
  ret <- df[, .(
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
    nmb_combined_entries = .N
  ),
  by = .(project, id)
  ]

  combined_entries <- ret[nmb_combined_entries > 1]
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
    list(v)
  }
  vadd_prefix_preserve_other_projects <- Vectorize(add_prefix_preserve_other_projects)


  ret[, ":="(section = paste(project, section, sep = h.SEPERATOR),
  id = paste(project, id, sep = h.SEPERATOR),
  depends_on = vadd_prefix_preserve_other_projects(project, depends_on),
  start = vadd_prefix_preserve_other_projects(project, start),
  prior_ids = vadd_prefix_preserve_other_projects(project, prior_ids))]

  unique(ret)
}


h.rd_check_project_section_id_unique <- function(df) {
  id_in_multiple_sections <-
    df %>%
    dplyr::select(project, id, section) %>%
    dplyr::distinct() %>%
    dplyr::count(project, id) %>%
    dplyr::filter(n > 1) %>%
    with(id)

  df <- df[order(df$project, df$id, df$section), ]
  h.log_rows(
    df,
    df$id %in% id_in_multiple_sections,
    warn_msg = glue::glue("The same id -{h.comma_list(id_in_multiple_sections)}- used in different 'sections' of the same 'project' is probably an error")
  )
}


h.rd_preprocess_deadline_column <- function(df) {
  df$raw_deadline <- df$deadline
  df$deadline <- suppressWarnings(lubridate::ymd(df$deadline))
  h.log_rows(
    df,
    with(df, xor(!is.na(deadline), !is.na(raw_deadline))),
    warn_msg = glue::glue("Entries in column 'deadline' must be a ymd-format"),
    error = TRUE
  )
  df
}

h.rd_preprocess_end_column <- function(df) {
  df$waiting <- df$end == "WAIT"
  df$est_days <- suppressWarnings(as.numeric(df$end))
  df$fixed_end_date <- suppressWarnings(lubridate::ymd(df$end))

  h.log_rows(
    df,
    with(df, (!waiting & is.na(est_days) & is.na(fixed_end_date))),
    warn_msg = glue::glue("Entries in column 'end' must be 'WAIT', an integer, or a date using a ymd-format"),
    error = TRUE
  )
  # if (any(idx)) {
  #   futile.logger::flog.warn(glue::glue("Entries in column 'end' must be 'WAIT', an integer, or a date using a ymd-format"))
  #   h.log_rows(df, idx)
  # }
  df$end <- NULL
  df
}

h.rd_preprocess_start_column <- function(df) {
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


  futile.logger::flog.info(glue::glue("Convert the 'TODAY' in column 'start' to the current date -{TODAY}-"))

  df$start[df$start == "TODAY"] <- TODAY
  df$fixed_start_date <- suppressWarnings(lubridate::ymd(df$start))
  df$start[!is.na(df$fixed_start_date)] <- NA

  df
}

h.rd_fill_with_default <- function(df, colname, def) {
  idx <- is.na(df[[colname]])

  h.log_rows(
    df,
    idx,
    warn_msg = glue::glue("Some entries in column -{colname}- are not specified. Set those entries to -{def}-.")
  )
  if (any(idx)) {
    df[[colname]][idx] <- def
  }
  df
}

h.rd_remove_unnessary_rows <- function(df) {
  futile.logger::flog.info(glue::glue("Remove rows where project, section, id, start, end, resource, task are empty"))
  
  discard <- with(df, is.na(project) & is.na(section) & is.na(id) & is.na(start) & is.na(end) & is.na(resource) & is.na(task))

  futile.logger::flog.debug(glue::glue("Remove {sum(discard)} rows"))

  df[!discard, ]
}

h.rd_select_cols <- function(df) {
  cols <- c("project", "section", "id", "depends_on", "start", "end", "resource", "task", "progress", "deadline")

  futile.logger::flog.info(glue::glue("Select the necessary columns -{h.comma_list(cols)}-"))
  df <- df[, lapply(.SD, as.character), .SDcols = cols]
}
