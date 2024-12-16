#' plot_indmeasurement() Function
#'
#' @param data Data to be analyzed
#' @param var Type of measurement - "Body Weight" or "Outcome"
#' @param treatment_info Data set that maps ID to treatment assignment. This can
#'  be made by inputting birthdata into create_treatment_info() function.
#'
#' @return A list containing:
#'   \item{p_tmt}{ggplot2 visualization of treatment group}
#'   \item{p_plac}{ggplot2 visualization of placebo group}
#' @import dplyr ggplot2
#' @export
plot_indmeasurement <- function(data, var, treatment_info) {

  # Preprocess data
  mod_data <- preprocessdata(data, var)

  # Transform data for ggplot
  plot_data <- data.frame(
    date = as.Date(rep(rownames(mod_data), ncol(mod_data))),
    variable = as.vector(mod_data),
    ID = rep(colnames(mod_data), each = nrow(mod_data))
  ) %>%
    dplyr::left_join(treatment_info, by = "ID")

  # Create treatment plot
  p_tmt <- plot_data %>%
    dplyr::filter(Treatment == "Tmt") %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = variable, group = ID, color = ID)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::ggtitle("Treatment Mice")

  # Create placebo plot
  p_plac <- plot_data %>%
    dplyr::filter(Treatment == "Plac") %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = variable, group = ID, color = ID)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::ggtitle("Placebo Mice")

  # Return a list of plots
  return(list(treatment = p_tmt, placebo = p_plac))
}

