#' Prepare gender inequality data for plotting
#' 
#' Restricts the data to the 2010–2100 projection period and summarizes
#' Gender Inequality Index values across available model-scenario
#' trajectories for each SSP and year
#'
#' @param ssp_data Filtered gender inequality data.
#' @return Summarized gender inequality trajectories.
#' @export
prep_gender_plot_data <- function(ssp_data) {
  gender_plot_data <- ssp_data |>
    dplyr::filter(year >= 2010, year <= 2100) |>
    dplyr::group_by(ssp, year, unit) |>
    dplyr::summarise(
      value_mean = mean(value, na.rm = TRUE),
      n_trajectories = dplyr::n_distinct(
        interaction(model, scenario, drop = TRUE)
      ),
      .groups = "drop"
    )

  return(gender_plot_data)
}
