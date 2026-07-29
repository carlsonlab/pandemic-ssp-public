#' Plot population aged 65+
#' 
#' Creates the Figure 3 panel showing projected World population aged
#' 65 years and older across the marker SSP and baseline scenarios.
#' Shaded ribbons represent the range across integrated assessment
#' models, while lines show the multi-model mean
#'
#' @param plot_data Prepared population aged 65+ data.
#' @param ssp_cols Named SSP color vector.
#' @return A ggplot object.
#' @export
plot_population65 <- function(plot_data, ssp_cols) {
  
  population65_plot <- ggplot2::ggplot(
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
      title = "Population 65+"
    ) +
    ssp_panel_theme()
  
  population65_plot <- fix_panel_layers(population65_plot) +
    labs(
      title = "D) Population 65+",
      y = "billion people"
    )
  
  population65_plot <- make_readable(population65_plot)

  return(population65_plot)
}
