#' combine panels of Fig. 3 into one figure
#' 
#' Arranges the nine Figure 3 panels into a 3 × 3 layout, creates a
#' shared legend for the SSP scenarios, and returns the complete figure
#' ready for export
#'
#' @param p1-p9 individual panels to be combined
#' @param ssp_cols color scheme
#' @return A ggplot object.
#' @export
plot_trajectories <- function(p1, p2, p3, p4, p5, p6, p7, p8, p9, ssp_cols){
  
  # Shared legend
  legend_df <- tibble(ssp = factor(names(ssp_cols), levels = names(ssp_cols)),
                      x = seq(1.65, 4.85, length.out = length(ssp_cols)))
  
  ssp_legend <- ggplot(legend_df) +
    annotate("text", x = 0.20, y = 0.58, label = "Scenarios",
             hjust = 0, vjust = 0.5, fontface = "bold", size = 2.45) +
    geom_rect(aes(xmin = x - 0.18, xmax = x + 0.18, ymin = 0.47, ymax = 0.71,
                  fill = ssp),
              alpha = 0.32, color = NA, show.legend = FALSE) +
    geom_segment(aes(x = x - 0.20, xend = x + 0.20, y = 0.59, yend = 0.59,
                     color = ssp),
                 linewidth = 0.58, show.legend = FALSE) +
    geom_text(aes(x = x, y = 0.24, label = ssp),
              size = 2.15, hjust = 0.5) +
    annotate("segment", x = 5.15, xend = 5.35, y = 0.64, yend = 0.64, 
             linewidth = 0.30) +
    annotate("text", x = 5.45, y = 0.64, label = "Mean", hjust = 0, vjust = 0.5,
             size = 2.05) +
    annotate("segment", x = 5.17, xend = 5.35, y = 0.42, yend = 0.34,
             linewidth = 0.26) +
    annotate("text", x = 5.45, y = 0.34, label = "Range", hjust = 0,
             vjust = 0.5, size = 2.05) +
    scale_color_manual(values = ssp_cols, guide = "none") +
    scale_fill_manual(values = ssp_cols, guide = "none") +
    coord_cartesian(xlim = c(0, 6.50), ylim = c(0.02, 0.86), clip = "off") +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0))
  
  # Three-row layout with additional spacing between rows
  
  top_row <- patchwork::wrap_plots(p1, p2, p3, ncol = 3)
  
  middle_row <- patchwork::wrap_plots(p4, p5, p6, ncol = 3)
  
  bottom_row <- patchwork::wrap_plots(p7, p8, p9, ncol = 3)
  
  figure3_panels <- patchwork::wrap_plots(top_row, plot_spacer(), middle_row,
                                          plot_spacer(), bottom_row, ncol = 1,
                                          heights = c(1, 0.04, 1, 0.04, 1)) &
    theme(legend.position = "none")
  
  figure3_combined <- patchwork::wrap_plots(figure3_panels, ssp_legend,
                                            ncol = 1, heights = c(1, 0.06))
  
  return(figure3_combined)
}
