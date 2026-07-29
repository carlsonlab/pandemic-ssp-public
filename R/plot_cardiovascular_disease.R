#' Plot cardiovascular disease mortality
#' 
#' Creates Figure 3 panel showing projected cardiovascular disease
#' mortality across the marker SSP and baseline scenarios
#'
#' @param plot_data Prepared cardiovascular mortality data.
#' @param ssp_cols Named SSP color vector.
#' @return A ggplot object.
#' @export
plot_cardiovascular_disease <- function(plot_data, ssp_cols) {
  
  cardiovascular_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = year, y = value_mean, color = ssp)
  ) +
    ggplot2::geom_line(linewidth = 1.25) +
    ggplot2::scale_color_manual(values = ssp_cols) +
    ssp_x_scale() +
    ggplot2::labs(
      x = NULL,
      y = "million deaths/yr",
      title = "Cardiovascular Disease"
    ) +
    ssp_panel_theme()
  
  cardiovascular_plot <- fix_panel_layers(cardiovascular_plot) +
    labs(
      title = "E) Cardiovascular Disease",
      y = "million deaths/yr"
    )
  
  cardiovascular_plot <- make_readable(cardiovascular_plot)

  return(cardiovascular_plot)
}
