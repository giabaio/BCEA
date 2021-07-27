
#' eib_plot_base
#' 
#' @template args-he
#' @template args-comparison
#' 
eib_plot_base <- function(he,
                          graph_params,
                          ...) {
  
  plot_params <- eib_params_base(he, graph_params)
  legend_params <- eib_legend_base(he, graph_params)
  
  do.call(matplot, c(list(x = he$k,
                          y = he$eib),
                     plot_params), quote = TRUE)
  abline(h = 0, col = "grey")      # x-axis
  plot_eib_cri(he, plot_params)    # credible intervals
  kstar_vlines(he, plot_params)
  do.call(legend, legend_params)
}

