
#' EIB Parameters CrI
#' @keywords internal aplot
#' 
eib_params_cri <- function(he, graph_params) {
  list(plot.cri = graph_params$plot.cri,
       data =
         compute_eib_cri(he,
                         graph_params$alpha_cri,
                         graph_params$cri.quantile),
       col = graph_params$line$cri_col,
       lty =  graph_params$line$cri_lty)
}

