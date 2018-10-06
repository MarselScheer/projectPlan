h.SEPERATOR <- "::"

h.comma_list <- function(v) {
  v <- sort(unique(v))
  ret <- paste0(v, collapse = ", ")
  if (ret == "") {
    ret <- NA_character_
  }
  ret
}

h.split_comma_list <- function(str) {
  str %>%
    stringr::str_split_fixed(pattern = ",", n = Inf) %>%
    unlist() %>%
    stringr::str_trim()
}

h.comma_list_2_uniq_vec <- function(str) {
  str %>%
    h.split_comma_list() %>%
    unique()
}

h.combine_comma_list_cols <- function(v, w = "") {
  ret <- h.comma_list_2_uniq_vec(v)
  w <- h.comma_list_2_uniq_vec(w)
  ret <- unique(c(ret, w))

  h.comma_list(ret[ret != ""])
}


h.log_rows <- function(df, idx, warn_msg, error = FALSE) {
  if (any(idx)) {
    if (error) {
      futile.logger::flog.error(warn_msg)
      futile.logger::flog.error("Rows:", df[idx, ], capture = TRUE)
      stop(warn_msg)
    } else {
      futile.logger::flog.warn(glue::glue("{warn_msg} (change logging-threshold to INFO to see which rows are affected)"), df[idx, c("project", "section", "id")], capture = TRUE)
      futile.logger::flog.info("Rows:", df[idx, ], capture = TRUE)
    }
  }
}
