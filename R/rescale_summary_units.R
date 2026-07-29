#' Rescale summary values
#' 
#' Divides the mean, lower-bound, and upper-bound trajectory values by a
#' common divisor (converts plotted values to more readable units)
#'  
#' @param df Data frame containing value_mean, value_lower, and value_upper.
#' @param divisor Numeric divisor.
#' @return Rescaled data frame.
#' @export
rescale_summary_units <- function(df, divisor = 1000) {
  rescaled_df <- df |>
    dplyr::mutate(
      value_mean = value_mean / divisor,
      value_lower = value_lower / divisor,
      value_upper = value_upper / divisor
    )
}
