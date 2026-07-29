#' fixing panel stuff
#' 
#' Removes point layers from a ggplot object and assigns a consistent line
#' width to all line layers. Harmonizes the appearance
#' of panels before they are combined into Figure 3
#'
#' @param p a plot
#' @param line_width 
#' @return 
#' @export
fix_panel_layers <- function(p, line_width = 0.42) {
  
  p$layers <- Filter(
    function(layer) !inherits(layer$geom, "GeomPoint"),
    p$layers
  )
  
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, "GeomLine")) {
      p$layers[[i]]$aes_params$linewidth <- line_width
      p$layers[[i]]$aes_params$size <- line_width
    }
  }
  
  p
}