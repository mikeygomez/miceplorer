#' preprocessdata() Function
#'
#' @param data Data set to be preprocessed.
#' @param var Type of measurement - "Body Weight" or "Outcome"
#'
#' @return Modified data set proper for analysis.
#' @export
preprocessdata <- function(data, var){
  #var is "Body Weight" or "Outcome"

  #how many measurements we have
  n_measurements <- length(colnames(data)[colnames(data) != "ID"]) / 2

  #create empty matrix with appropriate size
  mod_data <- matrix(nrow = n_measurements, ncol = length(data$ID))

  #create rownames but this will only be a placeholder for now
  rownames(mod_data) <- character(n_measurements)

  #Make IDs the column names
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
