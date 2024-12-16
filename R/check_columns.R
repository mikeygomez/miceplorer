#' check_columns() Function
#'
#' This function makes sure that the column names are appropriately named. If any
#' incorrect column names are found, it asks user to change it.
#'
#' @param data A data frame containing measurement values and dates. Column names
#' should be formatted as "ID", "Measurement name 1", "Date Measurement name 1",
#' ..., "Measurement name k", "Date Measurement name k".
#'
#' @param var A character string specifying the type of measurement to be processed
#' - either "Body Weight" or "Outcome".
#'
#' @return This function returns a list:
#'    \item{data}{cleaned dataset}
#'    \item{mismatches}{any incorrect column names that were found}
#' @import knitr
#' @export

check_columns <- function(data, var) {

  # validating that var is appropriate
  if (!var %in% c("Body Weight", "Outcome")) {
    stop("'var' must be either 'Body Weight' or 'Outcome'")
  }

  # ID must be the first column
  if ("ID" != colnames(data)[1]) {
    repeat {
      response <- readline(sprintf("The first column should contain mouse IDs. The current column name is '%s'. Would you like to change it to 'ID' (y/n/stop)?\n",
                                   colnames(data)[1]))

      if (response == "y" || response == "yes") {
        colnames(data)[1] <- "ID"
        cat("Column name changed to 'ID'.\n")
        break
      } else if (response == "n" || response == "no") {
        cat("No changes were made.\n")
        break
      } else if (response == "stop") {
        stop("'ID' column needs to be the first column. Check ordering or naming of columns.")
      } else {
        cat("Invalid input. Please enter 'y' for yes, 'n' for no, or 'stop' to stop.\n")
      }
    }
  }


  # non-id columns and number of non-id columns
  cols <- colnames(data)[colnames(data) != "ID"]
  n_measurements <- length(cols) / 2

  # find expected column names based on how many columns there are
  #(1 should be ID and the rest should be value-date paired columns)
  expected_names <- c("ID")
  for (i in 1:n_measurements) {
    expected_names <- c(
      expected_names,
      sprintf("%s %d", var, i),
      sprintf("Date %s %d", var, i)
    )
  }

  # check for any mismatched names
  mismatched <- colnames(data) != expected_names

  if (any(mismatched)) {
    mismatch_df <- data.frame(
      Column_Number = which(mismatched),
      Expected_Name = expected_names[mismatched],
      Found_Name = colnames(data)[mismatched],
      stringsAsFactors = FALSE
    )

    # print table of potential column name mistakes
    cat("Potential Column Name Mistakes:\n")
    print(knitr::kable(mismatch_df, format = "pipe",
                       col.names = c("Column #", "Expected Name", "Found Name")))

    # prompt user to fix
    response <- readline("Would you like to fix these column names? (y/n): ")
    if (tolower(response) == "y") {
      colnames(data) <- expected_names
      cat("\nColumn names have been corrected.\n")
    }
  } else {
    cat("Proceed with caution.\n")
  }

  return(list(
    data = data,
    mismatches = if(any(mismatched)) mismatch_df else NULL
  ))
}
