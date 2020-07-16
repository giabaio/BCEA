
#' @import ggplot2, grid
#' 
ceac_multi_ggplot <- function(he,
                              colour_params, ...) {
  
  df <- cbind("k" = rep(he$k, he$n_comparators),
              "ce" = c(he$m.ce))
  df <-
    data.frame(df,
               "comp" = as.factor(sort(rep(
                 1:he$n_comparators, length(he$k)
               )))) %>% 
    setNames(c("k", "ce", "comp"))
  
  graph_params <- prepare_graph_params_multi(...)
  
  ggplot(df, aes(x = k, y = ce)) +
    theme_bw() +
    geom_line(aes(linetype = comp)) +
    scale_linetype_manual("",
                          labels = label,
                          values = lty) +
    labs(title = "Cost-effectiveness acceptability curve\nfor multiple comparisons",
         x = "Willingness to pay",
         y = "Probability of most cost effectiveness") +
    theme(
      text = element_text(size = 11),
      legend.key.size = grid::unit(0.66, "lines"),
      legend.spacing = grid::unit(-1.25, "line"),
      panel.grid = element_blank(),
      legend.key = element_blank()) %>% 
    coord_cartesian(ylim = c(-0.05, 1.05)) +
    theme(
      legend.position = alt.legend,
      legend.justification = jus,
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text.align = 0,
      plot.title = element_text(
        lineheight = 1.05,
        face = "bold",
        size = 14.3,
        hjust = 0.5))
}
