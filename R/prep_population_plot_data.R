#' prepping population data to plot
#' 
#' Restricts data to the 2010–2100 projection period and summarizes
#' total population across IAMs by calculating the
#' multi-model mean, minimum, and maximum for each SSP and year
#'
#' @title prep_population_plot_data
#'
#' @param baseline_data filtered population data
#'
#' @return 
#' @export
prep_population_plot_data <- function(baseline_data) {
  
  prepped_data <- baseline_data %>%
    dplyr::filter(year >= 2010, year <= 2100) %>%
    dplyr::group_by(ssp, year, unit) %>%
    dplyr::summarise(
      value_mean = mean(value, na.rm = TRUE),
      value_lower = min(value, na.rm = TRUE),
      value_upper = max(value, na.rm = TRUE),
      n_models = dplyr::n_distinct(model),
      .groups = "drop"
    )
  
  return(prepped_data)
}