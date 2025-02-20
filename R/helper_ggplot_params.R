
#' @noRd
#' 
#' @keywords dplot
#' 
helper_ggplot_params <- function(he,
                                 graph_params) {
  
  n_lines <- num_lines(he)
  
  text_default <- list(
    size =
      if (is.rel(graph_params$text$size)) {
        11 * unclass(graph_params$text$size)  # theme_get()$text$size
      } else {
        graph_params$text$size
      })
  
  if (n_lines == 1) {
    default_params <-
      list(labels = NULL,
           line =
             list(type = 1,
                  color = 1,
                  size = 1),
           text = text_default)
    
    graph_params <-
      modifyList(default_params, graph_params)
  }
  
  if (n_lines > 1) {
    default_params <-
      list(labels = line_labels(he),
           line =
             list(type = rep_len(1:6, n_lines),
                  color = 1,
                  size = rep_len(1, n_lines)),
           text = text_default)
    
    graph_params <- modifyList(default_params, graph_params)
    
    types <- graph_params$line$type
    cols <- graph_params$line$color
    sizes <- graph_params$line$size
    
    is_enough_types <- length(types) >= n_lines
    is_enough_colours <- length(cols) >= n_lines
    is_enough_sizes <- length(sizes) == n_lines
    
    if (!is_enough_types) {
      graph_params$line$type <- rep_len(types, n_lines)}
    if (!is_enough_colours) {
      graph_params$line$color <- rep_len(cols, n_lines)}
    if (!is_enough_sizes) {
      graph_params$line$size <- rep_len(sizes, n_lines)}
  }
  
  graph_params
}
