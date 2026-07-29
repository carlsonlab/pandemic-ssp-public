#' filter relevant population data
#'
#' Selects the World-level population variable from IAMC database and
#' retains only marker SSP and baseline scenarios for plotting
#' 
#' @title filter_population_world_baseline
#'
#' @param raw_long population data
#'
#' @return 
#' @export
filter_population_world_baseline <- function(raw_long) {
  
  pop_filtered <- raw_long %>%
    dplyr::filter(
      region == "World",
      variable == "Population"
    ) %>%
    filter_baseline_or_plain_ssp()
  
  return(pop_filtered)
}