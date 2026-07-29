#' Filter cardiovascular disease mortality SSP trajectories
#'
#' Selects cardiovascular disease mortality projections from IAMC
#' database and retains only the marker SSP and baseline scenarios.
#' Rows with missing values are removed before plotting
#' 
#' @param raw_long Long-format IAMC data.
#' @return Filtered cardiovascular mortality data.
#' @export
filter_cardiovascular_ssp <- function(raw_long) {
  cardiovascular_filtered <- raw_long |>
    dplyr::filter(
      variable == "Deaths by Category of Cause|Cardiovascular Disease"
    ) |>
    filter_baseline_or_plain_ssp() |>
    dplyr::filter(!is.na(value))

  if (nrow(cardiovascular_filtered) == 0) {
    stop("No cardiovascular mortality rows found.")
  }

  return(cardiovascular_filtered)
}
