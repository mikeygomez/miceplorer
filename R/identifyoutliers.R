#' identifyoutliers() Function
#'
#' This function builds a time series model based off of inputted data and using
#' standardized residuals, it identifies potential outlier data.
#'
#' @param data A data frame containing measurement values and dates. Column names
#' should be formatted as "ID", "Measurement name 1", "Date Measurement name 1",
#' ..., "Measurement name k", "Date Measurement name k".
#'
#' @param var A character string specifying the type of measurement to be processed
#' - either "Body Weight" or "Outcome".
#'
#' @param treatment_info Data set that maps ID to treatment assignment. This can
#'  be made by inputting birthdata into create_treatment_info() function.
#'
#' @return A list containing:
#'   \item{model}{Fitted time-series model}
#'   \item{outliers}{Data frame of identified outliers}
#'   \item{plot}{ggplot plot of the data with outliers highlighted}
#' @import dplyr ggplot2 nlme
#' @importFrom nlme collapse
#' @export
#'
identifyoutliers <- function(data, var, treatment_info) {

  #validate inputs
  if (!var %in% c("Body Weight", "Outcome")) {
    stop("'var' must be either 'Body Weight' or 'Outcome'")
  }

  if (!all(c("ID", "Treatment") %in% colnames(treatment_info))) {
    stop("treatment_info must contain 'ID' and 'Treatment' columns")
  }

  mod_data <- preprocessdata(data, var)

  #preprocess data for time series analysis
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

  #filter out rows with NAs before fitting the model
  analysis_data <- analysis_data %>%
    dplyr::filter(!is.na(variable), !is.na(time), !is.na(Treatment))

  #fit the model
  timeseries_model <- nlme::gls(
    variable ~ time * Treatment,
    correlation = nlme::corAR1(form = ~ time | ID),
    weights = nlme::varIdent(form = ~ 1 | Treatment),
    data = analysis_data,
    method = "REML"
  )

  #calculate residuals
  analysis_data$residuals <- stats::residuals(timeseries_model, type = "normalized")

  #standardize residuals
  analysis_data <- analysis_data %>%
    dplyr::group_by(Treatment) %>%
    dplyr::mutate(
      sd_resid = sd(residuals, na.rm = TRUE),
      standardized_resid = residuals / sd_resid
    ) %>%
    dplyr::ungroup()

  #find outliers based on standardized residuals
  outliers <- analysis_data %>%
    dplyr::group_by(ID, Treatment) %>%
    dplyr::summarise(
      mean_resid = mean(standardized_resid, na.rm = TRUE),
      max_resid = max(abs(standardized_resid), na.rm = TRUE),
      n_large = sum(abs(standardized_resid) > 2)
    ) %>%
    dplyr::filter(max_resid > 3 | n_large >= 2)

  #create the plot
  p <- ggplot2::ggplot(analysis_data, ggplot2::aes(x = date, y = variable, group = ID)) +
    ggplot2::geom_line(ggplot2::aes(color = ID %in% outliers$ID)) +
    ggplot2::facet_wrap(~Treatment) +
    ggplot2::scale_color_manual(values = c("grey", "red"),
                                name = "Outlier",
                                labels = c("No", "Yes")) +
    ggplot2::theme_minimal()

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

