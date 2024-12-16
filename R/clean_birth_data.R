#' clean_birth_data() Function
#'
#' This function cleans birth data by checking treatment assignments and IDs.
#'
#' @param data A data frame containing birth information. Column names
#' should be formatted as "ID", "Sex", "Num", "Treatment"
#'
#' @return Returns a cleaned birth dataset.
#' @export
clean_birth_data <- function(data) {
  sexes = c("M", "F")
  treatments = c("Plac", "Tmt")
  result <- data

  for (i in seq_len(nrow(data))) {
    #grabs row
    row = data[i, ]
    #checks treatment assignment
    if (!row$Treatment %in% treatments) {
      cat("Found typo in treatment value for Mouse ID:", row$ID, "| Current Treatment:", row$Treatment, "\n")

      #fixes treatment assignment based on input
      choice <- readline("Enter 1 for Placebo, 2 for Treatment, or 3 to remove: ")
      if (choice == "1") result$Treatment[i] <- "Plac"
      if (choice == "2") result$Treatment[i] <- "Tmt"
      if (choice == "3") result <- result[-i,]
    }
    #checks ID based on sex and number columns
    if (!grepl(paste0(row$Sex,"_", row$Num), row$ID)) {
      #fixes ID based on input
      choice <- readline("Enter 1 to change ID, or 2 to remove entry: ")
      if (choice == "1") result$ID[i] <- paste0(row$Sex, "_", row$Num)
      if (choice == "2") result <- result[-i,]
    }
  }
  return(result)
}
