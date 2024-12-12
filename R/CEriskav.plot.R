#' @rdname CEriskav.plot
#' 
#' @template args-he
#' @param pos Legend position
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the options `"base"`,
#' `"ggplot2"` or `"plotly"`. Default value is `"base"`.
#'
#' @return \item{plot}{ A ggplot or plot_ly object containing the plot. Returned only
#' if `graph_type="ggplot2"` or `graph_type="plotly"`.}
#' @seealso [bcea()]
#' 
#' @references
#' \insertRef{Baio2013}{BCEA}
#' 
#' @importFrom grDevices colours
#' 
#' @export
#' 
CEriskav.plot.CEriskav <- function(he,
                           pos = "topright",
                           graph = c("base", "ggplot2", "plotly")
                           ) {
  
  graph <- match.arg(graph)
  # extra_args <- list(...)
  
  if (is_baseplot(graph)) {
    CEriskav_plot_base(he,
                       pos_legend = pos)
  } else if (is_ggplot(graph)) {
    CEriskav_plot_ggplot(he,
                         pos_legend = pos)
  } else if (is_plotly(graph)) {
    CEriskav_plot_plotly(he,
                         pos_legend = pos)
  }
}

#' Cost-effectiveness Plot Including a Parameter of Risk Aversion (CEriskav)
#' 
#' @template args-he
#' @param pos Legend position
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the options `"base"`,
#' `"ggplot2"` or `"plotly"`. Default value is `"base"`.
#' @export
#' 
CEriskav.plot <- function(he, pos, graph) {
  UseMethod('CEriskav.plot', he, pos, graph)
}
