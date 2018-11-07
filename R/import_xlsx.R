#' Import-helper-function for excel-sheets
#'  
#' 
#' @param fName File name of the xlsx-file that should be imported
#'
#' @param sheets list of sheet-names that are imported. If not specified all sheets from the xlsx-file are imported.
#'   If the sheet does not contain a project-column, then the name of the sheet is used as a project name.
#'   
#' @return One \code{data.table} containing all imported sheets. Usually, such an object needs to be preprocessed by 
#'   \link{wrangle_raw_plan}
#'
#' @seealso \link{wrangle_raw_plan}
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
