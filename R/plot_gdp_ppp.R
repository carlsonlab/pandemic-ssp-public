#' Plot GDP (PPP)
#' 
#' Creates Figure 3 panel showing projected World
#' GDP, measured at PPP, across the marker SSP
#' and baseline scenarios
#'
#' @param plot_data Prepared GDP data.
#' @param ssp_cols Named SSP color vector.
#' @return A ggplot object.
#' @export
plot_gdp_ppp <- function(plot_data, ssp_cols) {
  
  gdp_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = year, y = value_mean, color = ssp)
  ) +
    ggplot2::geom_line(linewidth = 1.25) +
    ggplot2::scale_color_manual(values = ssp_cols) +
    ssp_x_scale() +
    ggplot2::labs(
      x = NULL,
      y = "trillion international USD/yr",
      title = "GDP (PPP)"
    ) +
    ssp_panel_theme()
  
  gdp_plot <- fix_panel_layers(gdp_plot) +
    labs(
      title = "B) GDP (PPP)",
      y = "trillion international USD/yr"
    )
  
  gdp_plot <- make_readable(gdp_plot)

  return(gdp_plot)
}
