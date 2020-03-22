h.SEPERATOR <- "::"
h.TAG_PREVIOUS <- "PREVIOUS"


#' Paste unique entries vector together using comma as a separator
#'
#' @param v vector of characters
#'
#' @return string with the unique entries of v separated by comma
h.comma_list <- function(v) {
  v <- sort(unique(v))
  ret <- paste0(v, collapse = ", ")
  if (ret == "") {
    ret <- NA_character_
  }
  ret
}

#' Split string into a vector where the separator is a comma 
#'
#' @param str to be split
#'
#' @return a vector of trimmed characters splitted at commas
h.split_comma_list <- function(str) {
  str <- stringr::str_split_fixed(str, pattern = ",", n = Inf)
  stringr::str_trim(unlist(str))
}

#' Split string at commas into a vector of unique entries
#'
#' @param str to be split
#'
#' @return a vector of unique trimmed characters splitted at commas
h.comma_list_2_uniq_vec <- function(str) {
  unique(h.split_comma_list(str))
}

#' Combines two strings (with comma as separators) to one string
#'
#' @param v string with entries separated by commas
#' @param w as v. By default just an empty string
#'
#' @return unique entries of v and w together as string with comma 
#'   as a separator
h.combine_comma_list_cols <- function(v, w = "") {
  ret <- h.comma_list_2_uniq_vec(v)
  w <- h.comma_list_2_uniq_vec(w)
  ret <- unique(c(ret, w))

  h.comma_list(ret[ret != ""])
}


#' Logs particular rows of a data.frame as error, warning or info 
#'
#' @param df containing the rows that might get logged
#' @param idx logical index of rows to be logged. If all entries are FALSE nothing is logged
#' @param warn_msg to be passed to the R-function stop() if parameter error is TRUE
#' @param warn_columns columns logged in the logger-info if error is FALSE
#' @param error if an error or warning is logged
#'
#' @return No explicit object is returned
h.log_rows <- function(df, idx, warn_msg, warn_columns = c("project", "section", "id"), error = FALSE) {
  if (any(idx, na.rm = TRUE)) {
    if (error) {
      logger::log_error(warn_msg)
      logger::log_error(h.capture_table(df[idx, ]))
      stop(warn_msg)
    } else {
      logger::log_warn("{warn_msg} (change logging-threshold to INFO to see all columns)")
      logger::log_info(h.capture_table(df[idx, .SD, .SDcols = warn_columns]))
    }
  }
}

#' Captures a data.table in one string in order to log it with logger
#'
#' @param dt to be captured
#'
#' @return string with the captured dt
h.capture_table <- function(dt) {
  ret <- c("Captured table: ", utils::capture.output(dt))
  paste(ret, collapse = "\n")
}

#' Just calls log_trace with predefined msg
#'
#' @return NULL
h.log_start = function(){
  logger::log_trace("Start")
}

#' Just calls log_trace with predefined msg
#'
#' @return NULL
h.log_end = function(){
  logger::log_trace("End")
}

