#' Filter World GDP (PPP) SSP trajectories
#'
#' Selects World-level GDP measured at purchasing
#' power parity (PPP) from IAMC database and retains only marker
#' SSP and baseline scenarios. Rows with missing values are removed before
#' plotting
#' 
#' @param raw_long Long-format IAMC data.
#' @return Filtered GDP (PPP) data.
#' @export
filter_gdp_world_ssp <- function(raw_long) {
  gdp_filtered <- raw_long |>
    dplyr::filter(
      region == "World",
      variable == "GDP|PPP"
    ) |>
    filter_baseline_or_plain_ssp() |>
    dplyr::filter(!is.na(value))

  if (nrow(gdp_filtered) == 0) {
    stop("No World GDP|PPP rows found.")
  }

  return(gdp_filtered)
}
