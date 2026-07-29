#' Plot gender inequality
#' 
#' Creates Figure 3 panel showing projected World Gender Inequality Index
#' trajectories across the marker SSP and baseline scenarios
#'
#' @param plot_data Prepared gender inequality data.
#' @param ssp_cols Named SSP color vector.
#' @return A ggplot object.
#' @export
plot_gender_inequality <- function(plot_data, ssp_cols) {
  
  gender_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = year, y = value_mean, color = ssp)
  ) +
    ggplot2::geom_line(linewidth = 1.25) +
    ggplot2::scale_color_manual(values = ssp_cols) +
    ssp_x_scale() +
    ggplot2::labs(
      x = NULL,
      y = "index",
      title = "Gender Inequality"
    ) +
    ssp_panel_theme()
  
  gender_plot <- fix_panel_layers(gender_plot) +
    labs(
      title = "F) Gender Inequality",
      y = "index"
    )
  
  gender_plot <- make_readable(gender_plot)

  return(gender_plot)
}
