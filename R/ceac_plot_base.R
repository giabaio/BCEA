
#' @noRd
#' 
.ceac_plot_base <- function(he,
                            pos_legend,
                            graph_params,
                            comparison, ...) {
  
  extra_params <- list(...)
  
  if (is.null(comparison)) comparison <- he$comp
  
  legend_params <- make_legend_base(he, pos_legend, graph_params)
  
  graph_params <- helper_base_params(he, graph_params)
  
  do.call("matplot", c(list(x = he$k,
                            y = he$ceac),
                       graph_params), quote = TRUE)
  
  if (length(he$comp) > 1)
    do.call(legend, legend_params)
}
