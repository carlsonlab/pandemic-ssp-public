#' Prepare forest-area data for plotting
#' 
#' Restricts data to the 2010–2100 projection period and summarizes
#' forest area across integrated assessment models by calculating the
#' multi-model mean, minimum, and maximum for each SSP and year
#'
#' @param baseline_data Filtered forest-area data.
#' @return Summarized forest-area trajectories.
#' @export
prep_forest_plot_data <- function(baseline_data) {
  forest_plot_data <- baseline_data |>
    dplyr::filter(year >= 2010, year <= 2100) |>
    dplyr::group_by(ssp, year, unit) |>
    dplyr::summarise(
      value_mean = mean(value, na.rm = TRUE),
      value_lower = min(value, na.rm = TRUE),
      value_upper = max(value, na.rm = TRUE),
      n_models = dplyr::n_distinct(model),
      .groups = "drop"
    )

  return(forest_plot_data)
}
