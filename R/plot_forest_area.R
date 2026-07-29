#' Plot forest area
#' 
#' Creates Figure 3 panel showing projected World forest area across
#' the marker SSP and baseline scenarios. Shaded ribbons represent the range
#' across integrated assessment models, while lines show the multi-model mean
#'
#' @param plot_data Prepared forest-area data.
#' @param ssp_cols Named SSP color vector.
#' @return A ggplot object.
#' @export
plot_forest_area <- function(plot_data, ssp_cols) {
  
  forest_plot <- ggplot2::ggplot(
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
      y = "million ha",
      title = "Land cover (forest)"
    ) +
    ssp_panel_theme()
  
  forest_plot <- fix_panel_layers(forest_plot) +
    labs(
      title = "H) Land cover (forest)",
      y = "billion ha"
    )
  
  forest_plot <- make_readable(forest_plot)

  return(forest_plot)
}
