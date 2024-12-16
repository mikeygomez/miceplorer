#' clean_weight_data() Function
#'
#' This function cleans weight data by checking column names, IDs, and
#' data types. If any errors are found, the function prompts the user
#' to fix it.
#'
#' @param data A data frame containing weight data values and dates. Column names
#' should be formatted as "ID", "Body Weight 1", "Date Body Weight 1",
#' ..., "Body Weight k", "Date Body Weight k".
#'
#' @param birthdata Contains cleaned birth data.
#'
#' @return A cleaned weight data set with correct column names, IDs,
#'   body weight value types, and date format
#' @export
clean_weight_data <- function(data, birthdata) {
  var = "Body Weight"

  #checks that column names are appropriate
  columnnames <- check_columns(data, var)
  data <- columnnames$data

  #check that IDs are correct
  IDcheck <- checkIDs(data, birthdata, var)
  data <- IDcheck$data

  #check that body weight values are numbers or NA
  data <- checkquantvars(data, var)

  return(data)
}
