#' Prepare Gini data for plotting
#' 
#' Restricts data to the 2010–2100 projection period and summarizes
#' population-weighted Gini coefficients across available model-scenario
#' trajectories for each SSP and year
#'
#' @param ssp_data Filtered Gini data.
#' @return Summarized Gini trajectories.
#' @export
prep_gini_plot_data <- function(ssp_data) {
  gini_plot_data <- ssp_data |>
    dplyr::filter(year >= 2010, year <= 2100) |>
    dplyr::group_by(ssp, year, unit) |>
    dplyr::summarise(
      value_mean = mean(value, na.rm = TRUE),
      n_trajectories = dplyr::n_distinct(
        interaction(model, scenario, drop = TRUE)
      ),
      .groups = "drop"
    )

  return(gini_plot_data)
}
