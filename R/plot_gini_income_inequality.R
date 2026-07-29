#' Plot income inequality
#' 
#' Creates Figure 3 panel showing projected World population-weighted
#' Gini income inequality trajectories across the marker SSP and baseline
#' scenarios
#'
#' @param plot_data Prepared Gini data.
#' @param ssp_cols Named SSP color vector.
#' @return A ggplot object.
#' @export
plot_gini_income_inequality <- function(plot_data, ssp_cols) {
  
  gini_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = year, y = value_mean, color = ssp)
  ) +
    ggplot2::geom_line(linewidth = 1.25) +
    ggplot2::scale_color_manual(values = ssp_cols) +
    ssp_x_scale() +
    ggplot2::labs(
      x = NULL,
      y = "Gini coefficient",
      title = "Income Inequality"
    ) +
    ssp_panel_theme()
  
  gini_plot <- fix_panel_layers(gini_plot) +
    labs(
      title = "D) Income Inequality",
      y = "Gini coefficient"
    )
  
  gini_plot <- make_readable(gini_plot)

  return(gini_plot)
}
