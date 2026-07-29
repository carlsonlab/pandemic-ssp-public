#' plot to visualize how pandemic drivers are represented in the SSPs
#' 
#' @title plot_driver_representation
#'
#' @param drivers tbl_df of pandemic drivers
#'
#' @return 
#' @export
plot_driver_representation <- function(drivers){
  
  mycols <- MetBrewer::met.brewer("Cassatt1", 12)
  mylabs <- c("Pathogen\nspillover", "Pandemic\nspread and\nimpacts", 
              "Pandemic\npreparedness\nand response", "Cross-stage\ndrivers")
  mytheme <- theme_test() +
    theme(legend.text = element_text(size = 8))
  
  dat <- drivers %>%
    dplyr::mutate(
      stage = factor(stage, 
                     levels = c("Pathogen spillover", 
                                "Pandemic spread and impacts", 
                                "Pandemic preparedness and response", 
                                "Cross-stage drivers")),
      narr_rep = factor(narr_rep,
                        levels = c("Explicit in SSP narratives", 
                                   "Implicit in SSP narratives", 
                                   "Explored in SSP extensions",
                                   "Implicit in SSP extensions",
                                   "Requires new extensions or modeling", 
                                   "Not aligned with SSP narratives")),
      quant_proj = factor(quant_proj, 
                          levels = c("Projections available (core SSP quantifications)",
                                     "Projections available (standard IAM variables)", 
                                     "Projections available (extensions or downstream modeling studies)",
                                     "Projections possible (extensions or downstream modeling studies)", 
                                     "Quantifiable but misaligned with SSP framework", 
                                     "Difficult to quantify")))
  
  narr_rep_plot <- dat %>% 
    group_by(stage, narr_rep) %>%
    summarize(number = n()) %>%
    ggplot(aes(x = stage, y = number, fill = narr_rep)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(name = "Narrative representation", 
                      values = as.vector(mycols[1:6])) +
    ylab("Proportion") + xlab(NULL) +
    scale_x_discrete(labels = mylabs) +
    mytheme

  quant_proj_plot <- dat %>%
    group_by(stage, quant_proj) %>%
    summarize(number = n()) %>%
    ggplot(aes(x = stage, y = number, fill = quant_proj)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(name = "Quantitative projections", 
                      values = as.vector(mycols[12:7])) +
    ylab("Proportion") + xlab(NULL) +
    scale_x_discrete(labels = mylabs) +
    mytheme

  joined_plot <- narr_rep_plot + quant_proj_plot + 
    plot_layout(axis_titles = "collect", guides = 'collect') + 
    theme(legend.justification = 'left')
  
  return(joined_plot)

}
