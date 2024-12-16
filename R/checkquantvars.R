#' checkquantvars() Function
#'
#' @param data Data set to be checked
#' @param name Common name of columns that should contain quantitative variables.
#' Should be "Outcome" or "Body Weight"
#'
#' @return This function takes in a data set and makes sure that all quantitative
#' variable columns contain numeric values. If it finds a non-numeric value,
#' it asks user to input NA or a numeric value.
#' @export
checkquantvars <- function(data, name) {

  # Validating that name is appropriate
  if (!name %in% c("Body Weight", "Outcome")) {
    stop("'name' must be either 'Body Weight' or 'Outcome'")
  }

  # Grabs column names for quantitative variables, excluding dates
  column_names <- grep(paste0("^(?!Date)", name), colnames(data), value=TRUE, perl=TRUE)

  # Loop through each column
  for (col in column_names) {

    # Identify rows where the value is not numeric (or NA) but without converting to NA
    problem_rows <- which(is.na(suppressWarnings(as.numeric(data[[col]]))) &
                            !is.na(data[[col]]))  # Keep the original non-numeric values for the prompt

    if (length(problem_rows) > 0) {
      cat(sprintf("\nA data entry error has been found in column: %s\n", col))

      # Loop through the problematic rows
      for (row in problem_rows) {
        current_value <- data[row, col]
        cat(sprintf("Row %d: Current value is: %s\n", row, current_value))

        # Prompt user for valid input
        input <- readline(sprintf("Enter a numeric value or 'NA' to leave it missing for row %d: ", row))

        if (input == "NA") {
          data[row, col] <- "NA"  # Ensure it's numeric NA
        } else {
          # Keep prompting until a valid numeric input is entered
          while (is.na(as.numeric(input)) && input != "NA") {
            cat("Invalid input. Please enter a valid number or 'NA'\n")
            input <- readline(sprintf("Enter a numeric value or 'NA' to leave it missing for row %d: ", row))
          }

          # Convert input to numeric, replacing the value in the column
          data[row, col] <- input  # Ensure it's numeric
        }
      }
    }

    # Now convert the entire column to numeric if it's not already
    if (!is.numeric(data[[col]])) {
      data[[col]] <- suppressWarnings(as.numeric(as.character(data[[col]])))
    }
  }

  return(data)
}



