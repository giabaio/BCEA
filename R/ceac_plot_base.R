
#' Cost-Effectiveness Acceptability Curve (CEAC) Plot By Graph Device
#'
#' Choice of base R, ggplot2 or plotly.
#' @name ceac_plot_graph
NULL


#' @rdname ceac_plot_graph
#' 
#' @template args-he 
#' @param pos_legend Legend position
#' @param graph_params Aesthetic ggplot parameters
#' @param ... Additional arguments
#' @keywords hplot
#' 
#' @export
#' 
ceac_plot_base <- function(he,
                           pos_legend,
                           graph_params,
                           ...)
  UseMethod("ceac_plot_base", he)


#' @rdname ceac_plot_graph
#' @inheritParams ceac_plot_base
#' @keywords hplot
#' 
#' @export
#' 
ceac_plot_base.pairwise <- function(he,
                                    pos_legend,
                                    graph_params,
                                    ...) {
  ceac_matplot(he,
               pos_legend,
               graph_params,
               "p_best_interv")
}

#' @rdname ceac_plot_graph
#' @inheritParams ceac_plot_base
#' @keywords hplot
#' 
#' @export
#'
ceac_plot_base.bcea <- function(he,
                                pos_legend,
                                graph_params,
                                ...) {
  ceac_matplot(he,
               pos_legend,
               graph_params,
               "ceac")
}

#' @noRd
#' @inheritParams ceac_plot_base
#' @param ceac
#' @keywords hplot
#' 
#' @export
#' 
ceac_matplot <- function(he,
                         pos_legend,
                         graph_params,
                         ceac) {
  
  base_params <- helper_base_params(he, graph_params)
  
  legend_params <- ceac_legend_base(he, pos_legend, base_params)
  
  do.call("matplot", c(list(x = he$k,
                            y = he[[ceac]]),
                       base_params), quote = TRUE)
  
  do.call(legend, legend_params)
}

