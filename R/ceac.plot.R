
#' Cost-Effectiveness Acceptability Curve (CEAC) plot
#' 
#' Produces a plot of the Cost-Effectiveness Acceptability Curve (CEAC) against
#' the willingness to pay threshold.
#' 
#' @rdname plot-bcea
#' 
#' @template args-he
#' @template args-comparison
#' @param pos Parameter to set the position of the legend (only relevant for
#'   multiple interventions, ie more than 2 interventions being compared). Can be
#'   given in form of a string \code{(bottom|top)(right|left)} for base graphics
#'   and \code{bottom}, \code{top}, \code{left} or \code{right} for *ggplot2*.
#'   It can be a two-elements vector, which specifies the relative position on the x
#'   and y axis respectively, or alternatively it can be in form of a logical
#'   variable, with \code{FALSE} indicating to use the default position and
#'   \code{TRUE} to place it on the bottom of the plot. Default value is
#'   \code{c(1,0)}, that is the bottom right corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#'   plotting. Should (partial-)match the three options \code{"base"},
#'   \code{"ggplot2"} or \code{"plotly"}. Default value is \code{"base"}.
#' @param ...  If \code{graph="ggplot2"} and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional arguments:
#'  \itemize{
#'   \item \code{line_colors}: specifies the line colour(s) - all graph types.
#'   \item \code{line_types}: specifies the line type(s) as lty numeric values - all graph types.
#'   \item \code{area_include}: logical, include area under the CEAC curves - plotly only.
#'   \item \code{area_color}: specifies the AUC colour - plotly only.}
#'   
#' @return \item{ceac} {If \code{graph="ggplot2"} a ggplot object, or if \code{graph="plotly"} 
#'   a plotly object containing the requested plot. Nothing is returned when \code{graph="base"}, 
#'   the default.} The function produces a plot of the
#'   cost-effectiveness acceptability curve against the discrete grid of possible
#'   values for the willingness to pay parameter. Values of the CEAC closer to 1
#'   indicate that uncertainty in the cost-effectiveness of the reference
#'   intervention is very low. Similarly, values of the CEAC closer to 0 indicate
#'   that uncertainty in the cost-effectiveness of the comparator is very low.
#'   
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#'   Analysis in Health Economics.  Statistical Methods in Medical Research
#'   doi:10.1177/0962280211419832.
#' 
#'   Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
#' @keywords Health economic evaluation Cost Effectiveness Acceptability Curve
#' @export
#' 
#' @importFrom ggplot2, ellipsis
#' 
#' @examples 
#' 
#' data("Vaccine")
#' he <- BCEA::bcea(e, c)
#' ceac.plot(he)
#' 
#' ceac.plot(he, graph = "base")
#' ceac.plot(he, graph = "ggplot2")
#' ceac.plot(he, graph = "plotly")
#' 
#' ceac.plot(he, graph = "ggplot2", title = "my title", line = list(colors = "green"), theme = theme_dark())
#
#' he2 <- BCEA::bcea(cbind(e,e - 0.0002), cbind(c,c + 5))
#' mypalette <- RColorBrewer::brewer.pal(3, "Accent")
#' ceac.plot(he2, graph = "ggplot2", title = "my title", theme = theme_dark(), pos = TRUE, line = mypalette)
#
#' ceac.plot(he, graph = "base", title = "my title", line = list(colors = "green"))
#
#' ceac.plot(he2, graph = "base")
#'
ceac.plot <- function(he,
                      comparison = NULL,
                      pos = c(1, 0),
                      graph = c("base", "ggplot2", "plotly"),
                      ...) {
  
  options(scipen = 10)
  graph <- match.arg(graph)
  
  # choose graphical engine
  if (is.null(graph) || is.na(graph)) graph <- "base"
  
  graph_type <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  
  is_pkg_avail <-
    requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)
  
  if (graph_type == 2 && !is_pkg_avail) {
    warning("Package ggplot2 and grid not found; ceac.plot will be rendered using base graphics.", call. = FALSE)
    graph_type <- 1}
  
  if (graph_type == 3 && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package plotly not found; ceac.plot will be rendered using base graphics.", call. = FALSE)
    graph_type <- 1}
  
  graph_params <- prepare_graph_params(...)
  
  if (graph_type == 1) {
    
    .ceac_plot_base(he,
                    pos_legend = pos,
                    graph_params,
                    comparison, ...)
    
  } else if (graph_type == 2) {
    
    .ceac_plot_ggplot(he,
                      pos_legend = pos,
                      graph_params,
                      comparison, ...)
    
  } else if (graph_type == 3) {
    
    ##TODO:
    # .ceac_plot_plotly()
  }
}
