#' filter baseline or plain
#'
#'#' Retains only the five marker SSP scenarios (SSP1–SSP5) and their
#' corresponding baseline scenarios. Removes scenario variants before
#' plotting the baseline SSP trajectories
#' 
#' @title filter_baseline_or_plain_ssp
#'
#' @param data 
#'
#' @return 
#' @export
filter_baseline_or_plain_ssp <- function(data) {
  
  filtered_ssp <- data %>%
    dplyr::mutate(
      ssp = stringr::str_extract(scenario, "SSP[1-5]"),
      is_baseline = stringr::str_detect(
        scenario,
        stringr::regex("Baseline", ignore_case = TRUE)
      ),
      is_plain_ssp = scenario %in% paste0("SSP", 1:5)
    ) %>%
    dplyr::filter(
      !is.na(ssp),
      is_baseline | is_plain_ssp
    )
  
  return(filtered_ssp)
}