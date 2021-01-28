
#' @rdname ceac.plot
#' 
#' @template args-he
#' @template args-comparison
#' @param pos Parameter to set the position of the legend (only relevant for
#'   multiple interventions, ie more than 2 interventions being compared). Can be
#'   given in form of a string \code{(bottom|top)(right|left)} for base graphics
#'   and \code{bottom}, \code{top}, \code{left} or \code{right} for *ggplot2*.
#'   It can be a two-elements vector, which specifies the relative position on the x
#'   and y axis respectively, or alternatively in form of a logical
#'   variable, with \code{FALSE} indicating to use the default position and
#'   \code{TRUE} to place it on the bottom of the plot. Default value is
#'   \code{c(1,0)}, that is the bottom right corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#'   plotting. Should (partial-)match the three options \code{"base"},
#'   \code{"ggplot2"} or \code{"plotly"}. Default value is \code{"base"}.
#' @param ...  If \code{graph = "ggplot2"} and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional arguments:
#'  \itemize{
#'   \item \code{line_colors}: specifies the line colour(s) - all graph types.
#'   \item \code{line_types}: specifies the line type(s) as lty numeric values - all graph types.
#'   \item \code{area_include}: logical, include area under the CEAC curves - plotly only.
#'   \item \code{area_color}: specifies the AUC colour - plotly only.}
#'   
#' @return \item{ceac}{If \code{graph = "ggplot2"} a ggplot object, or if \code{graph = "plotly"} 
#'   a plotly object containing the requested plot. Nothing is returned when \code{graph = "base"},
#'   the default.} The function produces a plot of the
#'   cost-effectiveness acceptability curve against the discrete grid of possible
#'   values for the willingness to pay parameter. Values of the CEAC closer to 1
#'   indicate that uncertainty in the cost-effectiveness of the reference
#'   intervention is very low. Similarly, values of the CEAC closer to 0 indicate
#'   that uncertainty in the cost-effectiveness of the comparator is very low.
#'   
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()],
#'          [plot.bcea()]
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#'   Analysis in Health Economics. Statistical Methods in Medical Research
#'   doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' @keywords hplot
#' @export
#' 
#' @import ggplot2
#' 
#' @examples 
#' data("Vaccine")
#' he <- BCEA::bcea(e, c)
#' ceac.plot(he)
#' 
#' ceac.plot(he, graph = "base")
#' ceac.plot(he, graph = "ggplot2")
#' ceac.plot(he, graph = "plotly")
#' 
#' ceac.plot(he, graph = "ggplot2",
#'           title = "my title",
#'           line = list(colors = "green"),
#'           theme = ggplot2::theme_dark())
#'
#' ## more interventions
#' he2 <- BCEA::bcea(cbind(e, e - 0.0002), cbind(c, c + 5))
#' mypalette <- RColorBrewer::brewer.pal(3, "Accent")
#' ceac.plot(he2, graph = "ggplot2",
#'           title = "my title",
#'           theme = ggplot2::theme_dark(),
#'           pos = TRUE,
#'           line = list(colors = mypalette))
#
#' ceac.plot(he, graph = "base", title = "my title", line = list(colors = "green"))
#
#' ceac.plot(he2, graph = "base")
#' 
#' ceac.plot(he2, graph = "plotly", pos = "bottom")
#'
ceac.plot.bcea <- function(he,
                           comparison = NULL,
                           pos = c(1, 0),
                           graph = c("base", "ggplot2", "plotly"),
                           ...) {
  
  graph <- match.arg(graph)
  
  he <- setComparisons(he, comparison)
  
  graph_params <- prepare_ceac_params(...)
  
  if (is_baseplot(graph)) {
    
    ceac_plot_base(he,
                   pos_legend = pos,
                   graph_params)
    
  } else if (is_ggplot(graph)) {
    
    ceac_plot_ggplot(he,
                     pos_legend = pos,
                     graph_params, ...)
    
  } else if (is_plotly(graph)) {
    
    ceac_plot_plotly(he,
                     pos_legend = pos,
                     graph_params)
  }
}


#' Cost-Effectiveness Acceptability Curve (CEAC) Plot
#' 
#' Produces a plot of the Cost-Effectiveness Acceptability Curve (CEAC) against
#' the willingness to pay threshold.
#' 
#' The CEAC estimates the probability of cost-effectiveness, with respect to a
#' given willingness to pay threshold. The CEAC is used mainly to evaluate the
#' uncertainty associated with the decision-making process, since it enables the
#' quantification of the preference of the compared interventions, defined in terms
#' of difference in utilities.
#' Formally, the CEAC is defined as:
#' 
#' \deqn{\text{CEAC} = P(\text{IB}(\theta) > 0)}
#'   
#' If the net benefit function is used as utility function, the definition can be
#' re-written as
#' 
#' \deqn{\text{CEAC} = P(k\cdot\Delta_e - \Delta_c > 0)}
#' 
#' effectively depending on the willingness to pay value \eqn{k}.
#' 
#' @template args-he
#' @param ...
#' 
#' @export
#' @seealso \code{\link{ceac.plot.bcea}}
#' 
ceac.plot <- function(he, ...) {
  UseMethod('ceac.plot', he)
}

