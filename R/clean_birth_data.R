#' clean_birth_data() Function
#'
#' @param data Unclean dataset
#'
#' @return Returns a clean, pre-processed dataset
#' @export
clean_birth_data <- function(data) {
  sexes = c("M", "F")
  treatments = c("Plac", "Tmt")
  result <- data

  for (i in seq_len(nrow(data))) {
    row = data[i, ]
    if (!row$Treatment %in% treatments) {
      cat("Found typo in treatment value for Mouse ID:", row$ID, "| Current Treatment:", row$Treatment, "\n")

      choice <- readline("Enter 1 for Placebo, 2 for Treatment, or 3 to remove: ")
      if (choice == "1") result$Treatment[i] <- "Plac"
      if (choice == "2") result$Treatment[i] <- "Tmt"
      if (choice == "3") result <- result[-i,]
    }
    if (!grepl(paste0(row$Sex,"_", row$Num), row$ID)) {
      choice <- readline("Enter 1 to change ID, or 2 to remove entry: ")
      if (choice == "1") result$ID[i] <- paste0(row$Sex, "_", row$Num)
      if (choice == "2") result <- result[-i,]
    }
  }
  return(result)
}
