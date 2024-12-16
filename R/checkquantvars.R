#' checkquantvars() Function
#'
#' This function makes sure that the outcome or body weight measurements get
#' converted to numeric. If it finds any strings, it prompts user to input
#' a numeric value or NA.
#'
#' @param data A data frame containing measurement values and dates. Column names
#' should be formatted as "ID", "Measurement name 1", "Date Measurement name 1",
#' ..., "Measurement name k", "Date Measurement name k".
#'
#' @param name Common name of columns that should contain quantitative variables.
#' Should be "Outcome" or "Body Weight"
#'
#' @return This function returns a dataset with numeric values in appropriate columns.
#' @export
checkquantvars <- function(data, name) {

  #validating that name is appropriate
  if (!name %in% c("Body Weight", "Outcome")) {
    stop("'name' must be either 'Body Weight' or 'Outcome'")
  }

  #grabs column names for quantitative variables, excluding dates
  column_names <- grep(paste0("^(?!Date)", name), colnames(data), value=TRUE, perl=TRUE)

  #loop through the columns
  for (col in column_names) {

    #identify rows where the value is not numeric but without converting to NA
    problem_rows <- which(is.na(suppressWarnings(as.numeric(data[[col]]))) &
                            !is.na(data[[col]]))  # Keep the original non-numeric values for the prompt

    if (length(problem_rows) > 0) {
      cat(sprintf("\nA data entry error has been found in column: %s\n", col))

      #loop through the problematic rows
      for (row in problem_rows) {
        current_value <- data[row, col]
        cat(sprintf("Row %d: Current value is: %s\n", row, current_value))

        #ask user for valid input
        input <- readline(sprintf("Enter a numeric value or 'NA' to leave it missing for row %d: ", row))

        if (input == "NA") {
          data[row, col] <- "NA"  # Ensure it's numeric NA
        } else {
          #keep asking until valid input
          while (is.na(as.numeric(input)) && input != "NA") {
            cat("Invalid input. Please enter a valid number or 'NA'\n")
            input <- readline(sprintf("Enter a numeric value or 'NA' to leave it missing for row %d: ", row))
          }

          #add input as character string to dataframe
          data[row, col] <- input
        }
      }
    }

    #convert the entire column to numeric
    if (!is.numeric(data[[col]])) {
      data[[col]] <- suppressWarnings(as.numeric(as.character(data[[col]])))
    }
  }

  return(data)
}



