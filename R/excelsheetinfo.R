#' excelsheetinfo() Function
#'
#' This function reads in the excel file at the given path and then prompts the
#' user to identify which sheets inside the excel file are which data set type.
#' (Birth, Body Weight, or Outcome)
#'
#' @param path The path of the excel file to be analyzed.
#'
#' @return This function returns a mapping of each sheet in the excel to
#'   a particular data set type (Birth, Body Weight, or Outcome).
#' @import readxl
#' @export
excelsheetinfo <- function(path) {

  #load the file
  exl <- readxl::excel_sheets(path)
  #grab how many sheets there are
  n_sheets <- length(exl)

  cat("Found", n_sheets, "sheets in file:", paste(exl, collapse = ", "), "\n\n")

  sheet_types <- c("Birth", "Body Weight", "Outcome")
  sheet_nums <- list()

  for (type in sheet_types) {
    valid_input <- FALSE
    while (!valid_input) {
      #ask user to designate the different data type to each sheet
      sheet_num <- readline(paste0("Enter sheet number (1-", n_sheets, ") for ", type, " data: "))
      sheet_num <- as.numeric(sheet_num)

      if (!is.na(sheet_num) && sheet_num >= 1 && sheet_num <= n_sheets &&
          !(sheet_num %in% unlist(sheet_nums))) {
        sheet_nums[[type]] <- sheet_num
        valid_input <- TRUE
      } else {
        cat("Invalid input. Please enter a number between 1 and", n_sheets,
            "that hasn't been used yet.\n")
      }
    }
  }

  return(sheet_nums)
}
