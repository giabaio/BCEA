
#' @rdname ceac.plot
#' 
#' @template args-he
#' @template args-comparison
#' @template args-pos
#' @template args-graph
#'    
#' @return \item{ceac}{If `graph = "ggplot2"` a ggplot object, or if `graph = "plotly"` 
#'   a plotly object containing the requested plot. Nothing is returned when `graph = "base"`,
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
#' 
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' @export
#' 
#' @importFrom Rdpack reprompt
#' 
#' @examples 
#' data("Vaccine")
#' he <- BCEA::bcea(eff, cost)
#' ceac.plot(he)
#' 
#' ceac.plot(he, graph = "base")
#' ceac.plot(he, graph = "ggplot2")
#' ceac.plot(he, graph = "plotly")
#' 
#' ceac.plot(he, graph = "ggplot2",
#'           title = "my title",
#'           line = list(color = "green"),
#'           theme = ggplot2::theme_dark())
#'
#' ## more interventions
#' he2 <- BCEA::bcea(cbind(eff, eff - 0.0002), cbind(cost, cost + 5))
#' ## could also construct a palette using RColorBrewer, e.g.
#' ## mypalette <- RColorBrewer::brewer.pal(3, "Accent")
#' ## and then call
#' ## ceac.plot(he2, graph = "ggplot2",
#' ##          title = "my title",
#' ##          theme = ggplot2::theme_dark(),
#' ##          pos = TRUE,
#' ##          line = list(color = mypalette))
#
#' ceac.plot(he, graph = "base", title = "my title", line = list(color = "green"))
#
#' ceac.plot(he2, graph = "base")
#' 
#' ceac.plot(he2, graph = "plotly", pos = "bottom")
#'
ceac.plot.bcea <- function(he,
                           comparison = NULL,
                           pos = "bottomright",
                           graph = c("base", "ggplot2", "plotly"),
                           ...) {
  graph <- match.arg(graph)
  
  he <- setComparisons(he, comparison)
  
  graph_params <- prepare_ceac_params(he, ...)
  
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
#' \deqn{\textrm{CEAC} = P(\textrm{IB}(\theta) > 0)}
#'   
#' If the net benefit function is used as utility function, the definition can be
#' re-written as
#' 
#' \deqn{\textrm{CEAC} = P(k \cdot \Delta_e - \Delta_c > 0)}
#' 
#' effectively depending on the willingness to pay value \eqn{k}.
#' 
#' @template args-he
#' @param ...  If `graph = "ggplot2"` and a named theme object is supplied,
#'   it will be passed to the ggplot2 object. The usual ggplot2 syntax is used.
#'   Additional arguments:
#'  \itemize{
#'   \item `line = list(color)`: specifies the line colour(s) - all graph types.
#'   \item `line = list(type)`: specifies the line type(s) as `lty` numeric values - all graph types.
#'   \item `line = list(size)`: specifies the line width(s) as numeric values - all graph types.
#'   \item `currency`: Currency prefix to willingness to pay values - ggplot2 only.
#'   \item `area_include`: logical, include area under the CEAC curves - plotly only.
#'   \item `area_color`: specifies the AUC colour - plotly only.}
#' @aliases ceac.plot
#' @export
#' 
ceac.plot <- function(he, ...) {
  UseMethod('ceac.plot', he)
}

