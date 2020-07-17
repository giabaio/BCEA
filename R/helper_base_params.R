
#
helper_base_params <- function(he,
                               graph_params) {
  
  n_lines <- 
    if (inherits(he, "pairwise")) {
      he$n_comparators
    } else {
      he$n_comparisons}
  
  if (n_lines == 1) {
    
    default_params <- list(plot =
                             list(lwd = 1,
                                  line =
                                    list(types = 1)))
    
    graph_params <- modifyList(default_params, graph_params)
  }
  
  if (n_lines > 1) {
    
    default_params <-
      list(plot =
             list(lwd = ifelse(n_lines <= 6, 1, 1.5),
                  line =
                    list(types = rep_len(1:6, n_lines),
                         colors = colors()[floor(seq(262, 340,
                                                     length.out = n_lines))])
             ))
    
    graph_params <- modifyList(default_params, graph_params)
    
    types <- graph_params$plot$line$types
    cols <- graph_params$plot$line$colors
    
    is_enough_types <- length(types) >= n_lines
    is_enough_colours <- length(cols) >= n_lines
    
    if (!is_enough_types) {
      graph_params$plot$line$types <- rep_len(types, n_lines)
      message("Wrong number of line types provided. Falling back to default\n")}
    
    if (!is_enough_colours) {
      graph_params$plot$line$colors <- rep_len(cols, n_lines)
      message("Wrong number of colours provided. Falling back to default\n")}
  }
  
  list(type = "l",
       main = graph_params$annot$title,
       xlab = graph_params$annot$xlab,
       ylab = graph_params$annot$ylab,
       ylim = c(0, 1),
       lty = graph_params$plot$line$types,
       col = graph_params$plot$line$colors,
       lwd = graph_params$plot$lwd)
}

