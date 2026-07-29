#' Filter World built-up-area SSP trajectories
#'
#'#' Selects World-level built-up area variable from IAMC database and
#' retains only marker SSP and baseline scenarios for plotting
#' 
#' @param raw_long Long-format IAMC data.
#' @return Filtered built-up-area data.
#' @export
filter_builtup_world_baseline <- function(raw_long) {
  builtup_filtered <- raw_long |>
    dplyr::filter(
      region == "World",
      variable == "Land Cover|Built-up Area"
    ) |>
    filter_baseline_or_plain_ssp()

  if (nrow(builtup_filtered) == 0) {
    stop("No World SSP baseline rows found for Land Cover|Built-up Area.")
  }

  return(builtup_filtered)
}
