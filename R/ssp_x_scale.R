#' setting specifications for year in plot
#'
#' Creates a continuous year axis covering the specified projection period,
#' with tick marks shown at 10-year intervals
#' 
#' @title ssp_x_scale
#'
#' @param start_year start year for plotting
#' @param end_year end year for plotting
#'
#' @return 
#' @export
ssp_x_scale <- function(start_year = 2010, end_year = 2100) {
  
  ggplot2::scale_x_continuous(
    breaks = seq(start_year, end_year, 10),
    limits = c(start_year, end_year)
  )
}