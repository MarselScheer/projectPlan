h.SEPERATOR <- "::"

h.comma_list <- function(v) {
  v <- sort(unique(v))
  ret <- paste0(v, collapse = ", ")
  if (ret == "") {
    ret <- NA
  }
  ret
}

h.log_rows <- function(df, idx, warn_msg) {
  if (any(idx)) {
    futile.logger::flog.warn(warn_msg)
    futile.logger::flog.debug("Rows:", df[idx, ], capture = TRUE)
  }
}
