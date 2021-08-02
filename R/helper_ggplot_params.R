
#' @noRd
#' 
#' @keywords dplot
#' 
helper_ggplot_params <- function(he,
                                 graph_params) {
  
  n_lines <- num_lines(he)
  
  if (n_lines == 1) {
    default_params <-
      list(plot =
             list(labels = NULL,
                  line =
                    list(types = 1)))
    graph_params <-
      modifyList(default_params, graph_params)
  }
  
  if (n_lines > 1) {
    default_params <-
      list(plot =
             list(labels = line_labels(he),
                  line =
                    list(types = rep_len(1:6, n_lines),
                         colors = 1)))
    
    graph_params <- modifyList(default_params, graph_params)
    
    types <- graph_params$plot$line$types
    cols <- graph_params$plot$line$colors
    
    is_enough_types <- length(types) >= n_lines
    is_enough_colours <- length(cols) >= n_lines
    
    if (!is_enough_types) {
      graph_params$plot$line$types <- rep_len(types, n_lines)}
    
    if (!is_enough_colours) {
      graph_params$plot$line$colors <- rep_len(cols, n_lines)}
  }
  
  graph_params
}
