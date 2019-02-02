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
      futile.logger::flog.error(warn_msg)
      futile.logger::flog.error("Rows:", df[idx, ], capture = TRUE)
      stop(warn_msg)
    } else {
      futile.logger::flog.warn(glue::glue("{warn_msg} (change logging-threshold to INFO to see all columns)"), df[idx, .SD, .SDcols = warn_columns], capture = TRUE)
      futile.logger::flog.info("Rows:", df[idx, ], capture = TRUE)
    }
  }
}

h.log_start = function(){
  mc <- sys.call(sys.parent())
  mc <- capture.output(print(mc))
  futile.logger::flog.trace(glue::glue("Start {mc}"))
}
h.log_end = function(){
  mc <- sys.call(sys.parent())
  mc <- capture.output(print(mc))
  futile.logger::flog.trace(glue::glue("End {mc}"))
}

