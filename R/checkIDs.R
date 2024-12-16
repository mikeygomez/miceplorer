#' checkIDs() Function
#'
#' This function makes sure that the IDs in a dataset align with those found in
#' the cleaned birth data set.
#'
#' @param data A data frame containing measurement values and dates. Column names
#' should be formatted as "ID", "Measurement name 1", "Date Measurement name 1",
#' ..., "Measurement name k", "Date Measurement name k".
#'
#' @param birthdata Contains cleaned birth data.
#'
#' @param var A character string specifying the type of measurement to be processed
#' - either "Body Weight" or "Outcome".
#'
#' @return A list containing:
#'   \item{invalid_ids}{table of invalid IDs found}
#'   \item{remaining_valid_ids}{any remaining valid IDs not found in dataset}
#'   \item{changes_made}{number of invalid IDs found and changed}
#' @import dplyr knitr
#' @export
checkIDs <- function(data, birthdata, var) {
  cat(sprintf("\nCurrently checking %s Data\n", var))

  # find any missing IDs
  missing_ids <- birthdata %>%
    dplyr::filter(!ID %in% data$ID) %>%
    dplyr::pull(ID)

  #find any IDs that ARE NOT valid
  invalid_ids <- data %>%
    dplyr::filter(!ID %in% birthdata$ID)

  # generate summary table of mistakes
  if (nrow(invalid_ids) > 0) {
    invalid_table <- invalid_ids %>%
      dplyr::select(ID, dplyr::everything()) %>%
      dplyr::mutate(Row_Number = dplyr::row_number())

    #make table of IDs that are in birthdata but not found in current data set
    available_ids_table <- data.frame(
      Option_Number = seq_along(missing_ids),
      Available_ID = missing_ids
    )

    # print tables
    cat("\nInvalid IDs Found:\n")
    print(knitr::kable(invalid_table, format = "pipe"))

    cat("\nAvailable Valid IDs:\n")
    print(knitr::kable(available_ids_table, format = "pipe"))

    # for each invalid ID, ask user to fix it by changing it to a valid ID
    for (i in seq_len(nrow(invalid_ids))) {
      row <- invalid_ids[i, ]
      cat(sprintf("\nProcessing Invalid ID: %s (Row %d of %d)\n",
                  row$ID, i, nrow(invalid_ids)))

      while (TRUE) {
        choice <- readline("Enter number from Available IDs table above or 'remove' to delete observation: ")

        #delete the sample if prompted
        if (choice == "remove") {
          data <- data %>% dplyr::filter(ID != row$ID)
          break
        } else {
          choice_num <- as.numeric(choice)
          #if the number choice is valid, change the ID in dataset
          if (!is.na(choice_num) && choice_num <= length(missing_ids)) {
            data$ID[data$ID == row$ID] <- missing_ids[choice_num]
            missing_ids <- missing_ids[-choice_num]

            #update the available IDs table by deleting the chosen ID
            available_ids_table <- data.frame(
              Option_Number = seq_along(missing_ids),
              Available_ID = missing_ids
            )
            #print the updated available IDs table
            cat("\nUpdated Available IDs:\n")
            print(knitr::kable(available_ids_table, format = "pipe"))
            break
          } else {
            cat("Invalid input. Please enter a number between 1 and",
                length(missing_ids), "\n")
          }
        }
      }
    }
  } else {
    cat("No invalid IDs found. Proceed with analysis.\n")
  }

  return(list(
    data = data,
    summary = list(
      invalid_ids = if(exists("invalid_table")) invalid_table else NULL,
      remaining_valid_ids = if(length(missing_ids) > 0) missing_ids else NULL,
      changes_made = nrow(invalid_ids)
    )
  ))
}

