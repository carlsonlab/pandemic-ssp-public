#' Filter World gender inequality SSP trajectories
#'
#' Selects the World-level Gender Inequality Index from the IAMC database
#' and retains only the marker SSP and baseline scenarios. Rows with
#' missing values are removed before plotting
#' 
#' @param raw_long Long-format IAMC data.
#' @return Filtered gender inequality data.
#' @export
filter_gender_world_ssp <- function(raw_long) {
  gender_filtered <- raw_long |>
    dplyr::filter(
      region == "World",
      variable == "Gender Inequality Index"
    ) |>
    filter_baseline_or_plain_ssp() |>
    dplyr::filter(!is.na(value))

  if (nrow(gender_filtered) == 0) {
    stop("No World Gender Inequality Index rows found.")
  }

  return(gender_filtered)
}
