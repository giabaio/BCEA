
#' @keywords hplot
#' 
#' @export
#' 
ceac_plot_base <- function(he, ...) UseMethod("ceac_plot_base", he)


#' @keywords hplot
#' 
#' @export
#' 
ceac_plot_base.pairwise <- function(he,
                                    pos_legend,
                                    graph_params) {
  ceac_matplot(he,
               pos_legend,
               graph_params,
               "p_best_interv")
}

#' @keywords hplot
#' 
#' @export
#'
ceac_plot_base.bcea <- function(he,
                                pos_legend,
                                graph_params) {
  ceac_matplot(he,
               pos_legend,
               graph_params,
               "ceac")
}

#' @noRd
#' 
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

