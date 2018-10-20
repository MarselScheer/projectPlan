#' @export
import_xlsx <- function(fName, sheets) {
  futile.logger::flog.info(glue::glue("Import {fName}."))

  if (missing(sheets)) {
    sheets <- readxl::excel_sheets(fName)
  }

  dt_list <-
    lapply(
      sheets,
      function(s) {
        futile.logger::flog.info(glue::glue("Import {fName} - {s}"))
        df <- readxl::read_xlsx(fName, sheet = s, skip = 0)
        if (nrow(df) == 0) {
          futile.logger::flog.info(glue::glue("Skip the empty sheet -{s}"))
          return(NULL)
        }
        if (!("project" %in% names(df))) {
          empty_rows <- with(df, is.na(section) & is.na(id) & is.na(start) & is.na(end) & is.na(resource) & is.na(task))
          futile.logger::flog.info(glue::glue("No project column found. Create one using the sheetname -{s}"))
          df$project <- NA_character_
          df$project[!empty_rows] <- s
        }
        df
      }
    )
  data.table::rbindlist(dt_list, fill = TRUE)
}
