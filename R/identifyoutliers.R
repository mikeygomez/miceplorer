#' identifyoutliers() Function
#'
#' @param data Data to be analyzed
#' @param var Type of measurement - "Body Weight" or "Outcome"
#' @param treatment_info Data set that maps ID to treatment assignment. This can
#'  be made by inputting birthdata into create_treatment_info() function.
#'
#' @return A list containing:
#'   \item{model}{Fitted time-series model}
#'   \item{outliers}{Data frame of identified outliers}
#'   \item{plot}{ggplot plot of the data with outliers highlighted}
#' @import dplyr ggplot2 nlme
#' @export
identifyoutliers <- function(data, var, treatment_info) {

  # Validate inputs
  if (!var %in% c("Body Weight", "Outcome")) {
    stop("'var' must be either 'Body Weight' or 'Outcome'")
  }

  if (!all(c("ID", "Treatment") %in% colnames(treatment_info))) {
    stop("treatment_info must contain 'ID' and 'Treatment' columns")
  }

  mod_data <- preprocessdata(data, var)

  analysis_data <- data.frame(
    date = as.Date(rep(rownames(mod_data), ncol(mod_data))),
    variable = as.vector(mod_data),
    ID = rep(colnames(mod_data), each = nrow(mod_data))
  ) %>%
    dplyr::left_join(treatment_info, by = "ID") %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      time = as.numeric(date - min(date)),
      Treatment = factor(Treatment)
    ) %>%
    dplyr::arrange(ID, time)

  # Filter out rows with NAs before fitting the model
  analysis_data <- analysis_data %>%
    dplyr::filter(!is.na(variable), !is.na(time), !is.na(Treatment))

  # Fit the model
  timeseries_model <- nlme::gls(
    variable ~ time * Treatment,
    correlation = nlme::corAR1(form = ~ time | ID),
    weights = nlme::varIdent(form = ~ 1 | Treatment),
    data = analysis_data,
    method = "REML"
  )

  # Calculate residuals
  analysis_data$residuals <- stats::residuals(timeseries_model, type = "normalized")

  # Standardize residuals
  analysis_data <- analysis_data %>%
    dplyr::group_by(Treatment) %>%
    dplyr::mutate(
      sd_resid = sd(residuals, na.rm = TRUE),  # Standard deviation of residuals within treatment
      standardized_resid = residuals / sd_resid  # Standardize the residuals
    ) %>%
    dplyr::ungroup()

  # Find outliers based on standardized residuals
  outliers <- analysis_data %>%
    dplyr::group_by(ID, Treatment) %>%
    dplyr::summarise(
      mean_resid = mean(standardized_resid, na.rm = TRUE),
      max_resid = max(abs(standardized_resid), na.rm = TRUE),
      n_large = sum(abs(standardized_resid) > 2)  # Threshold for standardized residuals
    ) %>%
    dplyr::filter(max_resid > 3 | n_large >= 2)  # Outlier criteria based on standardized residuals

  # Create the plot
  p <- ggplot2::ggplot(analysis_data, ggplot2::aes(x = date, y = variable, group = ID)) +
    ggplot2::geom_line(ggplot2::aes(color = ID %in% outliers$ID)) +
    ggplot2::facet_wrap(~Treatment) +
    ggplot2::scale_color_manual(values = c("grey", "red"),
                                name = "Outlier",
                                labels = c("No", "Yes")) +
    ggplot2::theme_minimal()

  # Print the outlier message
  if (nrow(outliers) > 0) {
    message(sprintf("Found %d subjects with outlying measurements.", nrow(outliers)))
  } else {
    message("No outliers detected.")
  }

  print(p)

  return(list(
    model = timeseries_model,
    outliers = outliers,
    plot = p
  ))
}

