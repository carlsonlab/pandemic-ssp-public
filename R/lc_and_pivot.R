#' make var names lowercase and pivot the data
#'
#' Converts all column names to lowercase, identifies columns whose names are
#' four-digit years, and pivots those year columns from wide to long format
#' 
#' @title lc_and_pivot
#'
#' @param raw uncleaned SSP data
#'
#' @return 
#' @export
lc_and_pivot <- function(raw){
  
  names(raw) <- tolower(names(raw))
  
  year_cols <- names(raw)[stringr::str_detect(names(raw), "^\\d{4}$")]
  if (length(year_cols) == 0) stop("No year columns detected.")
  
  raw_long <- raw %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(year_cols),
      names_to = "year",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      year = as.numeric(year),
      value = as.numeric(value)
    )
  
  required_cols <- c(
    "model", "scenario", "region", "variable",
    "unit", "year", "value"
  )
  missing_cols <- setdiff(required_cols, names(raw_long))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  return(raw_long)
}