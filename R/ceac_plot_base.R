
#
ceac_plot_base <- function(he, ...) UseMethod("ceac_plot_base", he)

#
ceac_plot_base.multi <- function(he,
                                 pos,
                                 legend,
                                 graph_params) {
  ceac_matplot(he,
               pos,
               legend,
               graph_params,
               "p_best_interv")
}

#
ceac_plot_base.default <- function(he,
                                   pos,
                                   legend,
                                   graph_params) {
  ceac_matplot(he,
               pos,
               legend,
               graph_params,
               "ceac")
}

#' @noRd
#' 
ceac_matplot <- function(he,
                         pos_legend,
                         graph_params,
                         ceac) {
  
  base_params <- helper_base_params(he, graph_params)
  
  legend_params <- make_legend_base(he, pos_legend, base_params)
  
  do.call("matplot", c(list(x = he$k,
                            y = he[[ceac]]),
                       base_params), quote = TRUE)
  
  do.call(legend, legend_params)
}

