#' Prepare cardiovascular mortality data for plotting
#' 
#' Restricts data to the 2010–2100 projection period and aggregates
#' cardiovascular disease mortality across regions for each SSP and year
#'
#' @param ssp_data Filtered cardiovascular mortality data.
#' @return Global cardiovascular mortality trajectories.
#' @export
prep_cardiovascular_plot_data <- function(ssp_data) {
  cardiovascular_plot_data <- ssp_data |>
    dplyr::filter(year >= 2010, year <= 2100) |>
    dplyr::group_by(ssp, year, unit) |>
    dplyr::summarise(
      value_mean = sum(value, na.rm = TRUE),
      n_countries = dplyr::n_distinct(region),
      .groups = "drop"
    )

  return(cardiovascular_plot_data)
}
