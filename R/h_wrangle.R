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
#' @examples
h.rd_wrangle <- function(df) {
  df <- h.rd_select_cols(df)
  df <- dplyr::mutate_all(df, as.character)

  df <- h.rd_remove_unnessary_rows(df)
  df <- h.rd_fill_with_default(df, "project", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "section", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "id", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "end", "1")
  df <- h.rd_fill_with_default(df, "resource", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "task", "UNKNOWN")
  df <- h.rd_fill_with_default(df, "progress", "0")
  df <- h.rd_preprocess_start_column(df)
  df <- h.rd_preprocess_end_column(df)
  df <- h.rd_preprocess_deadline_column(df)
  h.check_section_id_unique(df)


  df
}

h.SEPERATOR <- "::"

h.comma_list <- function(v) {
  paste0(v, collapse = ", ")
}

h.check_section_id_unique <- function(df) {
  id_in_multiple_sections <-
    df %>%
    dplyr::select(id, section) %>%
    dplyr::distinct() %>%
    dplyr::count(id) %>%
    dplyr::filter(n > 1) %>%
    with(id)

  df <- df[order(df$id, df$section), ]
  h.log_every_row(
    df,
    df$id %in% id_in_multiple_sections,
    warn_msg = glue::glue("The same id {h.comma_list(id_in_multiple_sections)} used in different 'sections' is probably an error")
  )
}


h.rd_preprocess_deadline_column <- function(df) {
  df$raw_deadline <- df$deadline
  df$deadline <- suppressWarnings(lubridate::ymd(df$deadline))
  h.log_rows(
    df,
    with(df, xor(!is.na(deadline), !is.na(raw_deadline))),
    warn_msg = glue::glue("Entries in column 'deadline' must be a ymd-format")
  )
  df
}

h.rd_preprocess_end_column <- function(df) {
  df$waiting <- df$end == "WAIT"
  df$est_days <- suppressWarnings(as.numeric(df$end))
  df$fixed_end_date <- suppressWarnings(lubridate::ymd(df$end))

  h.log_rows(
    df,
    with(df, (is.na(est_days) & is.na(fixed_end_date))),
    warn_msg = glue::glue("Entries in column 'end' must be 'WAIT', an integer, or a date using a ymd-format")
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

  futile.logger::flog.info(glue::glue("Convert the 'TODAY' in column 'start' to the current date {TODAY}"))

  df$start[df$start == "TODAY"] <- TODAY
  df$fixed_start_date <- suppressWarnings(lubridate::ymd(df$start))
  df$start[!is.na(df$fixed_start_date)] <- NA

  df
}

h.log_rows <- function(df, idx, warn_msg) {
  if (any(idx)) {
    futile.logger::flog.warn(warn_msg)
    futile.logger::flog.debug("Rows:", df[idx, ], capture = TRUE)
  }
}

h.rd_fill_with_default <- function(df, colname, def) {
  idx <- is.na(df[[colname]])

  h.log_rows(
    df,
    idx,
    warn_msg = glue::glue("Some entries in column '{colname}' are not specified. Set those entries to {def}.")
  )
  if (any(idx)) {
    df[[colname]][idx] <- def
  }
  df
}

h.rd_remove_unnessary_rows <- function(df) {
  discard <- with(df, is.na(project) & is.na(section) & is.na(id) & is.na(start) & is.na(end) & is.na(resource) & is.na(task))

  futile.logger::flog.info(glue::glue("Remove rows where section, id, start, end, resource, task are empty"))
  futile.logger::flog.debug(glue::glue("Remove {sum(discard)} rows"))

  df[!discard, ]
}

h.rd_select_cols <- function(df) {
  cols <- c("project", "section", "id", "depends_on", "start", "end", "resource", "task", "progress", "deadline")

  futile.logger::flog.info(glue::glue("Select the necessary columns {h.comma_list(cols)}"))
  df <- df[, cols]
}
