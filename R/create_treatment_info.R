#' create_treatment_info() Function
#'
#' This function takes in a cleaned birth data set and outputs a dataframe with
#' only the ID and treatment columns. This dataframe is useful in some analyses.
#'
#' @param birth_data This is the cleaned birth data set from the clean_mouse_data
#' function output
#'
#' @return The output is a dataset with only ID and treatment assignment.
#' @import dplyr
#' @export
create_treatment_info <- function(birth_data) {
  #check if columns exist
  if (!all(c("ID", "Treatment") %in% colnames(birth_data))) {
    stop("Birth data must contain 'ID' and 'Treatment' columns")
  }

  # only include ID and treatment columns
  treatment_info <- birth_data %>%
    dplyr::select(ID, Treatment)

  return(treatment_info)
}
