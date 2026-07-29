#' Filter and aggregate World population aged 65+
#'
#' Selects World-level population projections for all five-year age groups
#' aged 65 years and older, retains only the marker SSP and baseline
#' scenarios, and sums the age groups by model, scenario, and year
#'
#' @param raw_long Long-format IAMC data.
#' @return Population aged 65+ by model, scenario, and year.
#' @export
filter_population65_world_baseline <- function(raw_long) {
  population65_filtered <- raw_long |>
    dplyr::filter(
      region == "World",
      stringr::str_detect(variable, "^Population\\|"),
      stringr::str_detect(
        variable,
        paste0(
          "Age 65-69|Age 70-74|Age 75-79|",
          "Age 80-84|Age 85-89|Age 90-94|",
          "Age 95-99|Age 100\\+"
        )
      )
    ) |>
    filter_baseline_or_plain_ssp() |>
    dplyr::group_by(model, scenario, ssp, region, unit, year) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  if (nrow(population65_filtered) == 0) {
    stop("No World SSP population 65+ rows found.")
  }

  return(population65_filtered)
}
