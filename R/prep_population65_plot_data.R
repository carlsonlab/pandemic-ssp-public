#' Prepare population aged 65+ data for plotting
#' 
#' Restricts data to the 2010–2100 projection period and summarizes
#' population aged 65 years and older across integrated assessment models
#' by calculating the multi-model mean, minimum, and maximum for each SSP
#' and year
#'
#' @param baseline_data Filtered population aged 65+ data.
#' @return Summarized population aged 65+ trajectories.
#' @export
prep_population65_plot_data <- function(baseline_data) {
  population65_plot_data <- baseline_data |>
    dplyr::filter(year >= 2010, year <= 2100) |>
    dplyr::group_by(ssp, year, unit) |>
    dplyr::summarise(
      value_mean = mean(value, na.rm = TRUE),
      value_lower = min(value, na.rm = TRUE),
      value_upper = max(value, na.rm = TRUE),
      n_models = dplyr::n_distinct(model),
      .groups = "drop"
    )

  return(population65_plot_data)
}
