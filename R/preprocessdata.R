#' preprocessdata() Function
#'
#' This function preprocesses a data set by extracting measurements for either
#' "Body Weight" or "Outcome" and modifying the structure of the data that is
#' appropriate for subsequent analysis. The function reshapes the data into one
#' each row is a unique date and each column is a unique ID.
#'
#' @param data A data frame containing measurement values and dates. Column names
#' should be formatted as "ID", "Measurement name 1", "Date Measurement name 1",
#' ..., "Measurement name k", "Date Measurement name k".
#'
#' @param var A character string specifying the type of measurement to be processed
#' - either "Body Weight" or "Outcome".
#'
#' @return A matrix containing the reshaped data with each row being a unique
#' date and each column being a unique ID.
#'
#' @export
preprocessdata <- function(data, var){
  #var is "Body Weight" or "Outcome"

  #how many measurements we have
  n_measurements <- length(colnames(data)[colnames(data) != "ID"]) / 2

  #create empty matrix with appropriate size
  mod_data <- matrix(nrow = n_measurements, ncol = length(data$ID))

  #create rownames but this will only be a placeholder for now
  rownames(mod_data) <- character(n_measurements)

  #nake IDs the column names
  colnames(mod_data) <- data$ID

  # loop through each variable-date pair
  for(i in 1:n_measurements) {
    var_col <- paste(var, i)
    date_col <- paste("Date", var, i)

    # add variable as a row
    mod_data[i,] <- data[[var_col]]

    #add the date as the row name
    rownames(mod_data)[i] <- as.character(data[[date_col]][1])
  }

  return(mod_data)
}
