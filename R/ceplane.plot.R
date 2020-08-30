
#' Cost-effectiveness Plane Plot
#' 
#' Produces a scatter plot of the cost-effectiveness plane, together with the
#' sustainability area, as a function of the selected willingness to pay
#' threshold.
#' 
#' @template args-he
#' 
#' @param comparison Selects the comparator, in case of more than two
#'   interventions being analysed. Default as \code{NULL} plots all the
#'   comparisons together. Any subset of the possible comparisons can be selected
#'   (e.g., \code{comparison = c(1,3)} or \code{comparison = 2}).
#' @param wtp The value of the willingness to pay parameter. Not used if
#'   \code{graph = "base"} for multiple comparisons.
#' @param pos Parameter to set the position of the legend; for a single
#'   comparison plot, the ICER legend position. Can be given in form of a string
#'   \code{(bottom|top)(right|left)} for base graphics and
#'   \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#'   which specifies the relative position on the x and y axis respectively, or
#'   alternatively it can be in form of a logical variable, with \code{FALSE}
#'   indicating to use the default position and \code{TRUE} to place it on the
#'   bottom of the plot. Default value is \code{c(1,1)}, that is the topright
#'   corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#'   plotting. Should (partial-)match the two options \code{"base"} or
#'   \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  If \code{graph = "ggplot2"} and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional graphical arguments:
#'  \itemize{
#'   \item \code{label.pos = FALSE}: will place the willingness to pay label in a different 
#'   position at the bottom of the graph - base and ggplot2 only (no label in plotly).
#'   \item \code{point_colors}: a vector of colours specifying the colour(s) associated to 
#'   the cloud of points. Should be of length 1 or equal to the number of comparisons.
#'   \item \code{point_sizes}: a vector of colours specifying the size(s) of the points.
#'   Should be of length 1 or equal to the number of comparisons.
#'   \item \code{ICER_colors}: a vector of colours specifying the colour(s) of the ICER points.
#'   Should be of length 1 or equal to the number of comparisons.
#'   \item \code{ICER_sizes}: a vector of colours specifying the size(s) of the ICER points.
#'   Should be of length 1 or equal to the number of comparisons.
#'   \item \code{area_include}: logical, include or exclude the cost-effectiveness 
#'   acceptability area (default is TRUE).
#'   \item \code{area_color}: a colour specifying the colour of the cost-effectiveness acceptability area.
#'  }
#'  
#' @return If \code{graph = "ggplot2"} a ggplot object, or if \code{graph = "plotly"} 
#'   a plotly object containing the requested plot. Nothing is returned when \code{graph = "base"}, 
#'   the default.
#'    
#'   Grey dots show the simulated values for the joint
#'   distribution of the effectiveness and cost differentials. The larger red
#'   dot shows the ICER and the grey area identifies the sustainability area,
#'   i.e. the part of the plan for which the simulated values are below the
#'   willingness to pay threshold. The proportion of points in the sustainability
#'   area effectively represents the CEAC for a given value of the willingness to
#'   pay. If the comparators are more than 2 and no pairwise comparison is
#'   specified, all scatterplots are graphed using different colours.
#'   
#' @details In the plotly version, point_colors, ICER_colors and area_color can also be specified
#'   as rgba colours using either the \code{\link[plotly]{toRGB}{plotly::toRGB}} function or
#'   a rgba colour string, e.g. \code{'rgba(1, 1, 1, 1)'}.
#'   
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics.
#' Statistical Methods in Medical Research. doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' 
#' @keywords hplot Health economic evaluation Cost Effectiveness Plane
#' 
#' @examples
#' 
#' ## create the bcea object for the smoking cessation example
#' data(Smoking)
#' 
#' m <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)
#' 
#' ## produce the base plot
#' ceplane.plot(m, wtp = 200, graph = "base")
#' 
#' ## select only one comparator
#' ceplane.plot(m, wtp = 200, graph = "base", comparison = 3)
#' 
#' ## or use ggplot2 instead
#' if (requireNamespace("ggplot2")) {
#'    ceplane.plot(m, wtp = 200, pos = "right", ICER_sizes = 2, graph = "ggplot2")
#' }
#' 
#' @export
#' 
ceplane.plot.bcea <- function(he,
                              comparison = NULL,
                              wtp = 25000,
                              pos = c(1, 1),
                              graph = c("base", "ggplot2"),
                              ...) {
  
  graph <- match.arg(graph)
  
  he <- subset_by_comparisons(he, comparison)
  
  graph_params <- prep_ceplane_params(he, wtp, ...)
  
  if (is_baseplot(graph)) {
    
    ceplane_plot_base(he,
                      wtp,
                      pos_legend = pos,
                      graph_params)
    
  } else if (is_ggplot(graph)) {
    
    ##TODO:...
    # ceplane_plot_ggplot(he,
    #                     pos_legend = pos,
    #                     graph_params, ...)
    
  } else if (is_plotly(graph)) {
    
    ##TODO:...
    # ceplane_plot_plotly(he, graph_params)
  }
}


#'
ceplane.plot <- function(he, ...) {
  UseMethod(ceplane.plot, he)
}
