#' clean_outcome_data() Function
#'
#' This function cleans outcome data by checking column names, IDs, and
#' data types. If any errors are found, the function prompts the user
#' to fix it.
#'
#' @param data A data frame containing outcome values and dates. Column names
#' should be formatted as "ID", "Outcome 1", "Date Outcome 1",
#' ..., "Outcome k", "Date Outcome k".
#'
#' @param birthdata Contains cleaned birth data.
#'
#' @return A cleaned outcome data set with correct column names and IDs
#' @export
clean_outcome_data <- function(data, birthdata) {
  var = "Outcome"

  #checks that column names are appropriate
  columnnames <- check_columns(data, var)
  data <- columnnames$data

  #check that IDs are correct
  IDcheck <- checkIDs(data, birthdata, var)
  data <- IDcheck$data

  #check that outcome values are numbers or NA
  data <- checkquantvars(data, var)

  return(data)
}
