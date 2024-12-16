#' weightloss_check() Function
#'
#' This function analyzes weight loss data and identifies mice that have exceeded a weight loss threshold set by you.
#' The function returns two summary tables of weight loss: one for all mice and one for those that have been flagged by exceeding the weight loss threshold.
#'
#' @param data A data frame containing the cleaned weight data for mice - you can obtain and clean this data set using the clean_mouse_data() function.
#' @param threshold A numeric value specifying the percentage weight loss threshold for removal. Mice that have lost more than this percentage of their initial weight will be flagged for sacrifice! The default is 20%.
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{sac_mice}{A summary table of all mice that have exceeded the weight loss threshold. The table includes `mouse_id`, `initial_weight`, `current_weight`, and `percentage_loss`.}
#'   \item{all_mice}{A summary table of all mice, including `mouse_id`, `initial_weight`, `current_weight`, and `percentage_loss`. This table shows the weight change for all subjects, regardless of whether they exceed the threshold.}
#' }
#'
#' @import dplyr knitr
#' @export
weightloss_check <- function(data, threshold = 20) {
  var = "Body Weight"
  moddata <- preprocessdata(data, var)

  #find baseline and latest weight measurements to calc % change
  first_weights <- moddata[1, ]
  last_weights <-  moddata[nrow(moddata), ]

  # calculate percent change
  percent_change <- ((last_weights - first_weights) / first_weights) * 100

  # make a summary table for all mice
  summary_table <- data.frame(
    ID = names(first_weights),
    First_Weight = first_weights,
    Last_Weight = last_weights,
    Percent_Change = percent_change,
    First_Date = rownames(moddata)[1],
    Last_Date = rownames(moddata)[nrow(moddata)],
    Should_Remove = percent_change <= -threshold
  ) %>%
    dplyr::arrange(Percent_Change)

  # find mice that should be SACed based on weight loss
  sac_mice <- summary_table %>%
    dplyr::filter(Should_Remove)

  # print out the results
  if (nrow(sac_mice) > 0) {
    cat("\nMice Exceeding", threshold, "% Weight Loss:\n")
    print(knitr::kable(sac_mice %>%
                         dplyr::select(-Should_Remove),
                       digits = 1))
  } else {
    cat("\nNo mice have exceeded the weight loss threshold.\n")
  }

  return(list(
    sac_mice = sac_mice,
    all_mice = summary_table
  ))
}
