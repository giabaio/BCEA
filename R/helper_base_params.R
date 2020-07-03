
#
helper_base_params <- function(he,
                               graph_params) {
  
  if (he$n_comparisons == 1) {
    
    default_params <- list(plot =
                             list(labels = NULL,
                                  lwd = 1,
                                  line =
                                    list(types = 1)))
    
    graph_params <- modifyList(default_params, graph_params)
  }
  
  if (he$n_comparisons > 1) {
    
    default_params <-
      list(plot =
             list(labels = paste(he$interventions[he$ref], "vs",
                                 he$interventions[he$comp]),
                  lwd = ifelse(he$n_comparisons <= 6, 1, 1.5),
                  line =
                    list(types = rep_len(1:6, he$n_comparisons),
                         colors = colors()[floor(seq(262, 340,
                                                     length.out = he$n_comparisons))])
             ))
    
    graph_params <- modifyList(default_params, graph_params)
    
    types <- graph_params$plot$line$types
    cols <- graph_params$plot$line$colors
    
    is_enough_types <- length(types) >= he$n_comparisons
    is_enough_colours <- length(cols) >= he$n_comparisons
    
    if (!is_enough_types) {
      graph_params$plot$line$types <- rep_len(types, he$n_comparisons)}
    
    if (!is_enough_colours) {
      graph_params$plot$line$colors <- rep_len(cols, he$n_comparisons)}
  }
  
  list(type = "l",
       main = graph_params$annot$title,
       xlab = graph_params$annot$xlab,
       ylab = graph_params$annot$ylab,
       ylim = c(0, 1),
       lty = graph_params$plot$line$types,
       col = graph_params$plot$line$colors[1],
       lwd = graph_params$plot$lwd)
}

