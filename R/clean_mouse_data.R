#' clean_mouse_data() Function
#'
#' This function cleans the mouse data by obtaining sheet information on the excel
#' file. It then uses this sheet information and the loaded excel file to
#' call the individual clean functions to fully clean it.
#'
#' @param file_path Path of the excel file that contains your data. The file
#' must contain three tabs - birth data, body weight data, and outcome data.
#'
#' @return A list containing:
#'    \item{birth}{cleaned birth data}
#'    \item{weight}{cleaned weight data}
#'    \item{outcome}{cleaned outcome data}
#' @import readxl
#' @export
clean_mouse_data <- function(file_path) {
  #obtain which tabs correspond to which dataset type
  sheet_map <- excelsheetinfo(file_path)

  birthdata <- readxl::read_excel(file_path, sheet = sheet_map[["Birth"]])
  clean_birth_data <- clean_birth_data(birthdata)

  weightdata <- readxl::read_excel(file_path, sheet = sheet_map[["Body Weight"]])
  clean_weight_data <- clean_weight_data(weightdata, clean_birth_data)

  outcomedata <- readxl::read_excel(file_path, sheet = sheet_map[["Outcome"]])
  clean_outcome_data <- clean_outcome_data(outcomedata, clean_birth_data)

  return(list(
    birth = clean_birth_data,
    weight = clean_weight_data,
    outcome = clean_outcome_data
  ))

}
