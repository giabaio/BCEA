
#' Expected Incremental Benefit Plot By Graph Device
#'
#' Choice of base R, ggplot2 or plotly.
#' @name eib_plot_graph
#' 
NULL


#' @rdname eib_plot_graph
#' EIB plot base R version
#' 
#' @template args-he
#' @param graph_params List of graph parameters
#' @param ...
#' @export
#' 
eib_plot_base <- function(he,
                          graph_params,
                          ...) {
  
  cri_params <- eib_params_cri(he, graph_params)
  plot_params <- eib_params_base(he, graph_params, cri_params)
  legend_params <- eib_legend_base(he, graph_params)
  
  do.call(matplot, c(list(x = he$k,
                          y = he$eib),
                     plot_params), quote = TRUE)
  abline(h = 0, col = "grey")      # x-axis
  plot_eib_cri(he, cri_params)     # credible intervals
  kstar_vlines(he, plot_params)
  do.call(legend, legend_params)
}

