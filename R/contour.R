
#' @rdname contour
#' 
#' @template args-he
#' @template args-graph
#' @template args-pos
#' @template args-comparison
#' @param ...  Additional graphical arguments. The usual ggplot2 syntax is used regardless of graph type.
#'  \itemize{
#'   \item `xlim`: The range of the plot along the x-axis. If NULL (default) it is
#'             determined by the range of the simulated values for `delta_e`
#'   \item `ylim`: The range of the plot along the y-axis. If NULL (default) it is
#'             determined by the range of the simulated values for `delta_c`
#'   \item `scale`: Scales the plot as a function of the observed standard deviation.
#'   \item `levels`: Numeric vector of levels at which to draw contour lines. Quantiles 0<p<1.
#'   \item `nlevels`: Number of levels to be plotted in the contour.
#' }
#'   
#' @importFrom stats sd
#' @importFrom graphics par
#' 
#' @export
#' 
contour.bcea <- function(he,
                         pos = "topleft",
                         graph = c("base", "ggplot2", "plotly"),
                         comparison = NULL,
                         ...) {
  
  graph_type <- match.arg(graph)
  
  he <- setComparisons(he, comparison)
  
  params <- prep_contour_params(he, ...)
  
  if (is_baseplot(graph_type)) {
    contour_base(he, pos_legend = pos, params, ...)
  } else if (is_ggplot(graph_type)) {
    contour_ggplot(he, pos_legend = pos, params, ...)
  } else if (is_plotly(graph_type)) {
    contour_plotly(he, pos_legend = pos, params, ...)
  }
}


#' @title Contour Plots for the Cost-Effectiveness Plane
#'
#' @description Contour method for objects in the class `bcea`.
#' Produces a scatterplot of the cost-effectiveness plane, with a contour-plot
#' of the bivariate density of the differentials of cost (y-axis) and
#' effectiveness (x-axis).
#'
#' @template args-he
#' 
#' @return \item{ceplane}{ A ggplot object containing the plot. Returned only
#' if `graph="ggplot2"`. } Plots the cost-effectiveness plane with a
#' scatterplot of all the simulated values from the (posterior) bivariate
#' distribution of (\eqn{\Delta_e, \Delta_c}), the differentials of effectiveness and
#' costs; superimposes a contour of the distribution and prints the estimated
#' value of the probability of each quadrant (combination of positive/negative
#' values for both \eqn{\Delta_e} and \eqn{\Delta_c})
#' 
#' @author Gianluca Baio, Andrea Berardi
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @seealso [bcea()],
#'          [ceplane.plot()],
#'          [contour2()]
#' @keywords hplot
#' 
#' @importFrom Rdpack reprompt
#'  
#' @examples
#' data(Vaccine)
#'
#' # run the health economic evaluation using BCEA
#' m <- bcea(e=eff,
#'           c=cost,           # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e,c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0,Kmax)
#'       plot=TRUE             # plots the results
#' )
#' 
#' contour(m)
#' contour(m, graph = "ggplot2")
#' 
#' contour(m,          # uses the results of the economic evaluation 
#'                     #  (a "bcea" object)
#'       comparison=1, # if more than 2 interventions, selects the 
#'                     #  pairwise comparison 
#'       nlevels=10,   # selects the number of levels to be 
#'                     #  plotted (default=4)
#'       levels=NULL,  # specifies the actual levels to be plotted 
#'                     #  (default=NULL, so that R will decide)
#'       scale=1,      # scales the bandwidths for both x- and 
#'                     #  y-axis (default=0.5)
#'       graph="base"  # uses base graphics to produce the plot
#' )
#' 
#' # use the smoking cessation dataset
#' data(Smoking)
#' m <- bcea(eff, cost, ref = 4, intervention = treats, Kmax = 500, plot = FALSE)
#' contour(m)
#' contour(m, graph = "ggplot2")
#' 
#' @export
#' 
contour <- function(he, ...) {
  UseMethod('contour', he)
}

