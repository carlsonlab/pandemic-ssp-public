#' plot to visualize pandemic drivers as circular dendrogram plot
#' hattip to code from 
#' https://r-graph-gallery.com/339-circular-dendrogram-with-ggraph.html
#' https://stackoverflow.com/questions/78713737/ggraph-adjusting-text-label-position-on-circular-dendrogram
#'
#' @title plot_drivers_dendrogram
#'
#' @param drivers tbl_df of pandemic drivers
#'
#' @return 
#' @export
plot_drivers_dendrogram <- function(drivers){
  
  stage_category <- drivers %>% 
    dplyr::select(stage, category) %>% 
    distinct() %>% 
    rename(from = stage, to = category)
  
  category_driver <- drivers %>% 
    dplyr::select(category, drivers) %>% 
    distinct() %>% 
    rename(from = category, to = drivers)
  
  # create data frame giving the hierarchical structure of the data
  d1 <- data.frame(from = "origin", to = rev(sort(unique(drivers$stage))))
  d2 <- data.frame(stage_category)
  d3 <- data.frame(category_driver)
  edges <- rbind(d1, d2, d3)
  
  # create a vertices data.frame. One line per object of our hierarchy
  vertices <- data.frame(
    name = unique(c(as.character(edges$from), as.character(edges$to)))) 
  
  # identify inner nodes so we can plot them with separate colors if desired
  vertices$inner_nodes = ifelse(
    vertices$name %in% unique(c(drivers$stage)), 
    TRUE, FALSE)
  
  # identify middle nodes so we can plot them with separate colors if desired
  vertices$middle_nodes <- ifelse(
    vertices$name %in% unique(c(drivers$category)), 
    TRUE, FALSE)
  
  # add stage for coloring of inner nodes
  vertices$stage <- edges$from[match(vertices$name, edges$to)]
  vertices$stage[2:5] <- vertices$name[2:5]
  vertices$stage[20:83] <- NA
  
  vertices <- vertices %>% 
    left_join(drivers[, c("drivers", "stage")],
              by = c("name" = "drivers")) %>% 
    mutate(stage = coalesce(stage.x, stage.y)) %>% 
    dplyr::select(-c(stage.x, stage.y))
  
  # Create graph object
  mygraph <- igraph::graph_from_data_frame(edges, vertices = vertices)
  
  # set color palette
  pal <- PNWColors::pnw_palette("Starfish", 7)[c(1, 3, 5, 7)]
  
  # Make the plot
  ggraph(mygraph, layout = "dendrogram", circular = TRUE) + 
    geom_edge_diagonal(colour = "grey") +
    
    # inner nodes (4)
    geom_node_text(aes(filter = inner_nodes, x = x*1.15, y = y*1.15, 
                       label = stringr::str_wrap(name, 15)), 
                   size = 5, alpha = 1, fontface = "bold") +
    geom_node_point(aes(filter = inner_nodes, x = x*1.07, y = y*1.07, 
                        fill = stage), 
                    pch = 21, size = 15, alpha = 0.5, show.legend = FALSE) +
    
    # middle nodes (14)
    geom_node_text(aes(filter = middle_nodes, x = x*1.15, y = y*1.15, 
                       label = stringr::str_wrap(name, 15)),
                   size = 3.5, alpha = 1, repel = T) +
    geom_node_point(aes(filter = middle_nodes, x = x*1.07, y = y*1.07, 
                        fill = stage), 
                    pch = 21, size = 10, alpha = 0.5, show.legend = FALSE) +
    
    # outer nodes (64)
    geom_node_text(aes(filter = leaf, x = x*1.15, y = y*1.15, 
                       label = name,
                       angle = ifelse(x >= 0,
                                      asin(y) * 360 / 2 / pi, 
                                      360 - asin(y) * 360 / 2 / pi),
                       hjust = ifelse(x >= 0, 0, 1)),
                   size = 2.5, alpha = 1) +
    geom_node_point(aes(filter = leaf, x = x*1.07, y = y*1.07, fill = stage),
                    pch = 21, size = 6, alpha = 0.5) +
    
    scale_fill_manual(values = pal, labels = c("Cross-stage drivers", 
                                               "Pandemic preparedness\nand response",
                                               "Pandemic spread\nand impacts",
                                               "Pathogen spillover"),
                      guide = guide_legend(reverse = T)) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.88, 0.07),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.text = element_text(size = 12),
      legend.key.width = unit(1, "cm")
    ) +
    expand_limits(x = c(-1.6, 1.5), y = c(-1.5, 1.6))
  
}
