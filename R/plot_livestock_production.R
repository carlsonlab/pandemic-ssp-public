#' Plot livestock production
#' 
#' Creates Figure 3 panel showing projected World livestock production
#' across the marker SSP and baseline scenarios. Shaded ribbons represent
#' the range across integrated assessment models, while lines show the
#' multi-model mean
#'
#' @param plot_data Prepared livestock-production data.
#' @param ssp_cols Named SSP color vector.
#' @return A ggplot object.
#' @export
plot_livestock_production <- function(plot_data, ssp_cols) {
  
  livestock_plot <- ggplot2::ggplot(
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
      y = "million t DM/yr",
      title = "Livestock Production"
    ) +
    ssp_panel_theme()
  
  livestock_plot <- fix_panel_layers(livestock_plot) +
    labs(
      title = "I) Livestock Production",
      y = "million t DM/yr"
    )
  
  livestock_plot <- make_readable(livestock_plot)

  return(livestock_plot)
}
