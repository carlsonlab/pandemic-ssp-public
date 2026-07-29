#' Filter World forest-area SSP trajectories
#'
#' Selects the World-level forest area variable from the IAMC database and
#' retains only the marker SSP and baseline scenarios for plotting
#' 
#' @param raw_long Long-format IAMC data.
#' @return Filtered forest-area data.
#' @export
filter_forest_world_baseline <- function(raw_long) {
  forest_filtered <- raw_long |>
    dplyr::filter(
      region == "World",
      variable == "Land Cover|Forest"
    ) |>
    filter_baseline_or_plain_ssp()

  if (nrow(forest_filtered) == 0) {
    stop("No World forest area rows found.")
  }

  return(forest_filtered)
}
