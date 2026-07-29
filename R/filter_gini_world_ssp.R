#' Filter World income inequality SSP trajectories
#'
#' Selects World-level population-weighted Gini income inequality
#' coefficient from IAMC database and retains only marker SSP
#' and baseline scenarios. Rows with missing values are removed before
#' plotting
#' 
#' @param raw_long Long-format IAMC data.
#' @return Filtered Gini data.
#' @export
filter_gini_world_ssp <- function(raw_long) {
  gini_filtered <- raw_long |>
    dplyr::filter(
      region == "World",
      variable == "Gini Income Inequality Coefficient [weighted by Population]"
    ) |>
    filter_baseline_or_plain_ssp() |>
    dplyr::filter(!is.na(value))

  if (nrow(gini_filtered) == 0) {
    stop("No World population-weighted Gini rows found.")
  }

  return(gini_filtered)
}
