#' Filter World livestock-production SSP trajectories
#'
#' Selects the World-level livestock production variable from IAMC
#' database and retains only marker SSP and baseline scenarios for
#' plotting
#' 
#' @param raw_long Long-format IAMC data.
#' @return Filtered livestock-production data.
#' @export
filter_livestock_world_baseline <- function(raw_long) {
  livestock_filtered <- raw_long |>
    dplyr::filter(
      region == "World",
      variable == "Agricultural Production|Livestock"
    ) |>
    filter_baseline_or_plain_ssp()

  if (nrow(livestock_filtered) == 0) {
    stop("No World livestock production rows found.")
  }

  return(livestock_filtered)
}
