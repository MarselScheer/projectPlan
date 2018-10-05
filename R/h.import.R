#' @export
import_xlsx <- function(fName, sheets, prefix) {
  futile.logger::flog.info(glue::glue("Import {fName}."))
  
  if (missing(sheets)) {
    sheets <- readxl::excel_sheets(fName)
  }
  if (missing(prefix)) {
    prefix <- sheets
  }
  
  if (length(sheets) != length(prefix)) {
    msg <- "sheets and prefix must have the same length"
    futile.logger::flog.error(msg)
    stop(msg)
  }
  
  dt_list <-
    lapply(sheets,
      function(s) {
        futile.logger::flog.info(glue::glue("Import {fName} - {s}"))
         readxl::read_xlsx(fName, sheet = s, skip = 0)
        
      }
    )
  
  data.table::rbindlist(dt_list, fill = TRUE)
}
