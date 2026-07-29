#' Prepare built-up-area data for plotting
#'
#' Restricts data to the 2010–2100 projection period and summarizes
#' built-up area across IAMs by calculating the
#' multi-model mean, minimum, and maximum for each SSP and year
#' 
#' @param baseline_data Filtered built-up-area data.
#' @return Summarized built-up-area trajectories.
#' @export
prep_builtup_plot_data <- function(baseline_data) {
  builtup_plot_data <- baseline_data |>
    dplyr::filter(year >= 2010, year <= 2100) |>
    dplyr::group_by(ssp, year, unit) |>
    dplyr::summarise(
      value_mean = mean(value, na.rm = TRUE),
      value_lower = min(value, na.rm = TRUE),
      value_upper = max(value, na.rm = TRUE),
      n_models = dplyr::n_distinct(model),
      .groups = "drop"
    )

  return(builtup_plot_data)
}
