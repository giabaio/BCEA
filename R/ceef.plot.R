
#' @rdname ceef.plot
#' 
#' @template args-he
#' @param comparators Vector specifying the comparators to be included in the
#' frontier analysis. It must have a length > 1. Default as \code{NULL} includes
#' all the available comparators.
#' @template args-pos
#' @param start.from.origins Logical. Should the frontier start from the
#'  origins of the axes? The argument is reset to \code{FALSE} if the average
#'  effectiveness and/or costs of at least one comparator are negative.
#' @param threshold Specifies if the efficiency should be defined based on a
#'  willingness-to-pay threshold value. If set to \code{NULL} (the default), no
#'  conditions are included on the slope increase. If a positive value is passed
#'  as argument, to be efficient an intervention also requires to have an ICER
#'  for the comparison versus the last efficient strategy not greater than the
#'  specified threshold value. A negative value will be ignored with a warning.
#' @param flip Logical. Should the axes of the plane be inverted?
#' @param dominance Logical. Should the dominance regions be included in the
#'  plot?
#' @param relative Logical. Should the plot display the absolute measures (the
#'  default as \code{FALSE}) or the differential outcomes versus the reference
#'  comparator?
#' @param print.summary Logical. Should the efficiency frontier summary be
#'  printed along with the graph?  See Details for additional information.
#' @param graph A string used to select the graphical engine to use for
#'  plotting. Should (partial-)match the two options \code{"base"} or
#'  \code{"ggplot2"}. Default value is \code{"base"}.
#' @param print.plot Logical. Should the efficiency frontier be plotted?
#' @param ... If \code{graph_type="ggplot2"} and a named theme object is supplied,
#'  it will be added to the ggplot object. Ignored if \code{graph_type="base"}.
#'  Setting the optional argument \code{include.ICER} to \code{TRUE} will print
#'  the ICERs in the summary tables, if produced.
#' 
#' @return \item{ceplane}{ A ggplot object containing the plot. Returned only
#' if \code{graph_type="ggplot2"}. } The function produces a plot of the
#' cost-effectiveness efficiency frontier. The dots show the simulated values
#' for the intervention-specific distributions of the effectiveness and costs.
#' The circles indicate the average of each bivariate distribution, with the
#' numbers referring to each included intervention. The numbers inside the
#' circles are black if the intervention is included in the frontier and grey
#' otherwise. If the option \code{dominance} is set to \code{TRUE}, the
#' dominance regions are plotted, indicating the areas of dominance.
#' Interventions in the areas between the dominance region and the frontier are
#' in a situation of extended dominance.
#' @author Andrea Berardi, Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' 
#' IQWIG (2009). General methods for the Assessment of the Relation of Benefits
#' to Cost, Version 1.0. IQWIG, November 2009.
#' @importFrom graphics rect abline points legend box
#' @importFrom grDevices colours
#' 
#' @examples
#' 
#' ## create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)
#' 
#' ## produce plot
#' ceef.plot(m, graph = "base")
#' 
#' \donttest{
#' ## tweak the options
#' ## flip axis
#' ceef.plot(m,
#'           flip = TRUE,
#'           dominance = FALSE,
#'           start.from.origins = FALSE,
#'           print.summary = FALSE,
#'           graph = "base")
#'           
#' ## or use ggplot2 instead
#' if(require(ggplot2)){
#' ceef.plot(m,
#'           dominance = TRUE,
#'           start.from.origins = FALSE,
#'           pos = TRUE,
#'           print.summary = FALSE,
#'           graph = "ggplot2")
#'  }
#' }
#' 
#' @export
#' 
ceef.plot.bcea <- function(he,
                           comparators = NULL,
                           pos = c(1, 1),
                           start.from.origins = TRUE,
                           threshold = NULL,
                           flip = FALSE,
                           dominance = TRUE,
                           relative = FALSE,
                           print.summary = TRUE,
                           graph = c("base", "ggplot2"),
                           print.plot = TRUE,
                           ...) {
   
   graph <- match.arg(graph)
   
   # extra_args <- list(...)
   
   ##TODO: this function uses comparators not comparisons
   ##      thing is he$e,c have all interventions because can modify
   ##      
   if (!is.null(comparators)) {
      he$ref <- rank(unique(he$ref, comparators))[1]
      he$comp <- rank(unique(he$ref, comparators))[-1]
      he$n_comparators <- length(comparators)
      he$n_comparisons <- length(comparators) - 1
      he$interventions <- he$interventions[comparators]
   }
   
   # if incremental analysis (relative to the reference) required
   # needs to modify the bcea object
   if (relative) {
      temp <- he
      temp$e <- temp$c <- matrix(NA, he$n_sim, he$n_comparators)
      temp$e[, he$ref] <- temp$c[, he$ref] <- rep(0, he$n_sim)
      temp$e[, -he$ref] <- -he$delta.e
      temp$c[, -he$ref] <- -he$delta.c
      he <- temp
   }
  
   frontier_data <-
      prep_frontier_data(he,
                         threshold,
                         start.from.origins)
   frontier_params <-
      list(colour =
              colours()[
                 floor(seq(262, 340, length.out = he$n_comparators))], # grey scale
           pos = pos,
           flip = flip,
           relative = relative,
           dominance = dominance)
           
   if (print.summary)
      ceef.summary(he,
                   frontier_data,
                   frontier_params,
                   ...)
   
   if (is_baseplot(graph)) {
      if (print.plot) {
         ceef_plot_base(he,
                        frontier_data,
                        frontier_params)
      }
   } else if (is_ggplot(graph)) {
      if (print.plot) {
         ceef_plot_ggplot(he,
                          frontier_data,
                          frontier_params,
                          ...)
      }
   } else if (is_plotly(graph)) {
      ##TODO:
      # ceef_plot_plotly(he,
      #                  frontier_data,
      #                  frontier_params,
      #                  ...)
   }
}


#' Cost-Effectiveness Efficiency Frontier (CEEF) Plot
#' 
#' The line connecting successive points on a cost-effectiveness plane which each
#' represent the effect and cost associated with different treatment alternatives.
#' The gradient of a line segment represents the ICER of the treatment comparison
#' between the two alternatives represented by that segment.
#' The cost-effectiveness frontier consists of the set of points corresponding to
#' treatment alternatives that are considered to be cost-effective at different values
#' of the cost-effectiveness threshold. The steeper the gradient between successive
#' points on the frontier, the higher is the ICER between these treatment alternatives
#' and the more expensive alternative would be considered cost-effective only when a
#' high value of the cost-effectiveness threshold is assumed.
#' Points not lying on the cost-effectiveness frontier represent treatment alternatives
#' that are not considered cost-effective at any value of the cost-effectiveness threshold.
#' 
#' Back compatibility with BCEA previous versions:
#' The \code{bcea} objects did not include the generating \code{e} and \code{c}
#' matrices in BCEA versions <2.1-0. This function is not compatible with
#' objects created with previous versions. The matrices can be appended to
#' \code{bcea} objects obtained using previous versions, making sure that the
#' class of the object remains unaltered.
#' 
#' The argument \code{print.summary} allows for printing a brief summary of the
#' efficiency frontier, with default to \code{TRUE}. Two tables are plotted,
#' one for the interventions included in the frontier and one for the dominated
#' interventions. The average costs and clinical benefits are included for each
#' intervention. The frontier table includes the slope for the increase in the
#' frontier and the non-frontier table displays the dominance type of each
#' dominated intervention. Please note that the slopes are defined as the
#' increment in the costs for a unit increment in the benefits even if
#' \code{flip = TRUE} for consistency with the ICER definition. The angle of
#' increase is in radians and depends on the definition of the axes, i.e. on
#' the value given to the \code{flip} argument.
#' 
#' If the argument \code{relative} is set to \code{TRUE}, the graph will not
#' display the absolute measures of costs and benefits. Instead the axes will
#' represent differential costs and benefits compared to the reference
#' intervention (indexed by \code{ref} in the \code{\link{bcea}} function).
#' 
#' @template args-he
#' @export
#' 
ceef.plot <- function(he, ...) {
   UseMethod('ceef.plot', he)
}

