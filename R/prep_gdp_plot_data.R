#' Prepare GDP (PPP) data for plotting
#' 
#' Restricts data to the 2010–2100 projection period and summarizes
#' GDP measured at PPP across available model-scenario
#' trajectories for each SSP and year. Values are divided by 1,000 to convert
#' the plotted trajectories to trillions of international USD per year
#'
#' @param ssp_data Filtered GDP data.
#' @return Summarized GDP trajectories.
#' @export
prep_gdp_plot_data <- function(ssp_data) {
  gdp_plot_data <- ssp_data |>
    dplyr::filter(year >= 2010, year <= 2100) |>
    dplyr::group_by(ssp, year) |>
    dplyr::summarise(
      value_mean = mean(value, na.rm = TRUE) / 1000,
      n_trajectories = dplyr::n_distinct(
        interaction(model, scenario, drop = TRUE)
      ),
      .groups = "drop"
    )

  return(gdp_plot_data)
}
