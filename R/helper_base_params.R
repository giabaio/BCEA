
#' @importFrom grDevices colors
#' @keywords dplot
#' 
helper_base_params <- function(he,
                               graph_params) {
  
  n_lines <- num_lines(he)
  
  if (n_lines == 1) {
    default_params <-
      list(list(lwd = 1,
                line = list(type = 1)))
    
    graph_params <- modifyList(default_params, graph_params)
  }
  
  if (n_lines > 1) {
    default_params <-
      list(list(lwd = ifelse(n_lines <= 6, 1, 1.5),
                line =
                  list(type = rep_len(1:6, n_lines),
                       color = colors()[floor(seq(262, 340,
                                                  length.out = n_lines))])
             ))
    
    graph_params <- modifyList(default_params, graph_params)
    
    types <- graph_params$line$type
    cols <- graph_params$line$color
    
    is_enough_types <- length(types) >= n_lines || length(types) == 1
    is_enough_colours <- length(cols) >= n_lines || length(cols) == 1
    
    if (!is_enough_types) {
      graph_params$line$type <- rep_len(types, n_lines)
      message("Wrong number of line types provided. Falling back to default\n")}
    
    if (!is_enough_colours) {
      graph_params$line$color <- rep_len(cols, n_lines)
      message("Wrong number of colours provided. Falling back to default\n")}
  }
  
  list(type = "l",
       main = graph_params$annot$title,
       xlab = graph_params$annot$x,
       ylab = graph_params$annot$y,
       ylim = c(0, 1),
       lty = graph_params$line$type,
       col = graph_params$line$color,
       lwd = graph_params$lwd,
       pch = NULL)
}

