#' changing small plotting specs
#' 
#' Standardizes axis limits, text sizes, margins, etc. so that all Figure 3 panels 
#' share a consistent appearance
#'
#' @param p a plot
#' @return 
#' @export
make_readable <- function(p) {
  
  p +
    scale_x_continuous(breaks = c(2010, 2050, 2090),
                       limits = c(2010, 2100)) +
    guides(color = "none", fill = "none") +
    theme_bw(base_size = 6.5) +
    theme(plot.title = element_text(size = 6.7, face = "plain", hjust = 0,
                                    margin = margin(b = 3)),
          axis.title = element_text(size = 6.3),
          axis.title.y = element_text(margin = margin(r = 3), vjust = 0.5),
          axis.text = element_text(size = 5.7),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.margin = margin(0.3, 1.2, 0.3, 1.2))
}
