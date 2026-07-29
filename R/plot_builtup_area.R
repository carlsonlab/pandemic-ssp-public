#' Plot built-up area
#' 
#' Creates the Figure 3 panel showing projected World built-up area across
#' the marker SSP and baseline scenarios. Shaded ribbons represent the range
#' across integrated assessment models, while lines show the multi-model mean
#'
#' @param plot_data Prepared built-up-area data.
#' @param ssp_cols Named SSP color vector.
#' @return A ggplot object.
#' @export
plot_builtup_area <- function(plot_data, ssp_cols) {
  
  builtup_plot <- ggplot2::ggplot(
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
      title = "Land cover (built-up)"
    ) +
    ssp_panel_theme()
  
  builtup_plot <- fix_panel_layers(builtup_plot) +
    labs(
      title = "G) Land cover (built-up)",
      y = "million ha"
    )
  
  builtup_plot <- make_readable(builtup_plot)
  
  return(builtup_plot)
}
