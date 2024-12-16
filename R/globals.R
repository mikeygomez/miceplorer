#' @importFrom stats residuals time
#' @importFrom dplyr %>%
NULL

utils::globalVariables(c(
  "ID", "Percent_Change", "Should_Remove", "Treatment",
  "max_resid", "n_large", "p_tmt", "treatment",
  "variable", "time", "residuals"
))
