#' clean_outcome_data() Function
#'
#' @param data Weight data to be cleaned.
#' @param birthdata Contains cleaned birth data.
#'
#' @return A cleaned outcome data set with correct column names, IDs,
#'   body weight value types, and date format
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
