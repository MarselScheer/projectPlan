#' Import-helper-function for excel-sheets
#'
#' Note that the package comes with a template-xlsx-file that contains all
#' the columns that are expected by the package to work properly.
#' This template is located in the template-folder of the installed package.
#' Alternatively, you can download the template from 
#' https://github.com/MarselScheer/projectPlan/inst/template
#' @param fName File name of the xlsx-file that should be imported
#'
#' @param sheets list of sheet-names that are imported. If not specified all 
#'   sheets from the xlsx-file are imported. If the sheet does not contain 
#'   a project-column, then the name of the sheet is used as a project name.
#'
#' @return One \code{data.table} containing all imported sheets. 
#'   Usually, such an object needs to be preprocessed by
#'   \link{wrangle_raw_plan}
#'
#' @seealso \link{wrangle_raw_plan}
#' @export
import_xlsx <- function(fName, sheets) {
  logger::log_info("Import {fName}.")

  if (missing(sheets)) {
    sheets <- readxl::excel_sheets(fName)
  }

  dt_list <-
    lapply(
      sheets,
      function(s) {
        logger::log_info("Import {fName} - {s}")
        df <- readxl::read_xlsx(fName, sheet = s, skip = 0, col_types = "text")
        if (nrow(df) == 0) {
          logger::log_info("Skip the empty sheet -{s}")
          return(NULL)
        }
        if (!("project" %in% names(df))) {
          empty_rows <- with(df, is.na(section) & is.na(id) & is.na(start) & is.na(end) & is.na(resource) & is.na(task))
          logger::log_info("No project column found. Create one using the sheetname -{s}")
          df$project <- NA_character_
          df$project[!empty_rows] <- s
        }
        df
      }
    )
  data.table::rbindlist(dt_list, fill = TRUE)
}
