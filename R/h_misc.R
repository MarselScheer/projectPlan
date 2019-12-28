h.SEPERATOR <- "::"
h.TAG_PREVIOUS <- "PREVIOUS"


h.comma_list <- function(v) {
  v <- sort(unique(v))
  ret <- paste0(v, collapse = ", ")
  if (ret == "") {
    ret <- NA_character_
  }
  ret
}

h.split_comma_list <- function(str) {
  str <- stringr::str_split_fixed(str, pattern = ",", n = Inf)
  stringr::str_trim(unlist(str))
}

h.comma_list_2_uniq_vec <- function(str) {
  unique(h.split_comma_list(str))
}

h.combine_comma_list_cols <- function(v, w = "") {
  ret <- h.comma_list_2_uniq_vec(v)
  w <- h.comma_list_2_uniq_vec(w)
  ret <- unique(c(ret, w))

  h.comma_list(ret[ret != ""])
}


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

h.capture_table <- function(dt) {
  ret <- c("Captured table: ", utils::capture.output(dt))
  paste(ret, collapse = "\n")
}

h.log_start = function(){
  logger::log_trace("Start")
}
h.log_end = function(){
  logger::log_trace("End")
}

