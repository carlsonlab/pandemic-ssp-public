#' prepping population data to plot
#' 
#' Creates Figure 3 panel showing projected World population across
#' the marker SSP and baseline scenarios. Shaded ribbons represent the
#' range across IAMs, while lines show the multi-model mean
#'
#' @title plot_total_population
#'
#' @param plot_data prepped population data
#' @param ssp_cols colors for SSPs
#'
#' @return 
#' @export
plot_total_population <- function(plot_data, ssp_cols) {
  
  pop_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = year, color = ssp, fill = ssp)
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = value_lower, ymax = value_upper),
      alpha = 0.18,
      color = NA
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = value_mean),
      linewidth = 1.25
    ) +
    ggplot2::scale_color_manual(values = ssp_cols) +
    ggplot2::scale_fill_manual(values = ssp_cols) +
    ssp_x_scale() +
    ggplot2::labs(
      x = NULL,
      y = "million",
      title = "Total Population"
    ) +
    ssp_panel_theme()
    
    pop_plot <- fix_panel_layers(pop_plot) +
      labs(
        title = "A) Total Population",
        y = "billion people"
      )
    
    pop_plot <- make_readable(pop_plot)
  
  return(pop_plot)
}