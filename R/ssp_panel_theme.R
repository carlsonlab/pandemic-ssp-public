#' set some ggplot theme basics
#' 
#' Defines the shared graphical theme used by the individual SSP trajectory
#' panels, including legend placement, grid-line formatting, title styling,
#' and legend spacing
#'
#' @title ssp_panel_theme
#'
#' @param 
#'
#' @return 
#' @export
ssp_panel_theme <- function() {
  ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = grid::unit(0.4, "cm"),
      legend.spacing.x = grid::unit(0.2, "cm")
    )
}