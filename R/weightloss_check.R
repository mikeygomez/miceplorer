#' weightloss_check() Function
#'
#' @param data Data to be analyzed
#' @param threshold Percentage weight loss threshold for removal (default = 20%)
#'
#' @return A list containing:
#'   \item{sac_mice}{Summary table of all mice that exceed the weight threshold}
#'   \item{all_mice}{Summary table of all mice and weight}
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
