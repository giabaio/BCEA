
#' @rdname contour
#' @importFrom stats sd
#' @importFrom graphics par
#' 
#' @export
#' 
contour.bcea <- function(he,
                         scale = 0.5,
                         nlevels = 4,
                         levels = NULL,
                         pos = c(1, 0),
                         xlim = NULL,
                         ylim = NULL,
                         graph = c("base", "ggplot2"),
                         ...) {

  # comparison selects which plot should be made
  # by default it is the first possible
  
  # Additional/optional arguments
  extra_args <- list(...)
  
  params <- list()
  
  if (!exists("xlab", where = extra_args)) {
    params$xlab <- "Effectiveness differential"
  } else {
    params$xlab <- extra_args$xlab
  }
  
  if (!exists("ylab", where = extra_args)) {
    params$ylab <- "Cost differential"
  } else {
    params$ylab <- extra_args$ylab
  }
  
  if (!exists("title", where = extra_args)) {
    params$title <-
      paste(
        "Cost effectiveness plane contour plot\n",
        he$interventions[he$ref],
        " vs ",
        he$interventions[he$comp],
        sep = "")
  } else {
    params$title <- extra_args$title
  }
  
  params$alt.legend <- pos
  
  graph <- match.arg(graph)
  
  if (is_baseplot(graph)) {
    contour_base(he, params, scale, nlevels, levels, xlim, ylim, extra_args)
  }
  else{
    if (!requireNamespace("ggplot2", quietly = TRUE) &
        requireNamespace("grid", quietly = TRUE)) {
      message("Falling back to base graphics\n")
      contour(
        he,
        comparison = comparison,
        scale = scale,
        nlevels = nlevels,
        pos = alt.legend,
        levels = levels,
        graph = "base",
        ...)
      return(invisible(NULL))
    }
    
    if (!is.null(levels))
      message("Option level will be ignored using ggplot2 graphics")
    
   contour_ggplot(he, params, scale, nlevels, levels, xlim, ylim, extra_args)
  }
}


#' @name contour
#' @title Contour Plots for the Cost-Effectiveness Plane
#'
#' Contour method for objects in the class \code{bcea}.
#' Produces a scatterplot of the cost-effectiveness plane, with a contour-plot
#' of the bivariate density of the differentials of cost (y-axis) and
#' effectiveness (x-axis).
#'
#' @template args-he
#' @param scale Scales the plot as a function of the observed standard
#' deviation.
#' @param levels Numeric vector of levels at which to draw contour lines. Will
#' be ignored using \code{graph="ggplot2"}.
#' @param nlevels Number of levels to be plotted in the contour.
#' @template args-pos
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-) match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param xlim The range of the plot along the x-axis. If NULL (default) it is
#' determined by the range of the simulated values for \code{delta_e}
#' @param ylim The range of the plot along the y-axis. If NULL (default) it is
#' determined by the range of the simulated values for \code{delta_c}
#' @param ...  Additional arguments to 'plot.window', 'title', 'Axis' and
#' 'box', typically graphical parameters such as 'cex.axis'. Will be ignored if
#' \code{graph="ggplot2"}.
#' 
#' @return \item{ceplane}{ A ggplot object containing the plot. Returned only
#' if \code{graph="ggplot2"}. } Plots the cost-effectiveness plane with a
#' scatterplot of all the simulated values from the (posterior) bivariate
#' distribution of (\eqn{\Delta_e, \Delta_c}), the differentials of effectiveness and
#' costs; superimposes a contour of the distribution and prints the estimated
#' value of the probability of each quadrant (combination of positive/negative
#' values for both \eqn{\Delta_e} and \eqn{\Delta_c})
#' 
#' @author Gianluca Baio, Andrea Berardi
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics. Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#'
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' @seealso \code{\link{bcea}},
#'          \code{\link{ceplane.plot}},
#'          \code{\link{contour2}}
#' @keywords "Health economic evaluation" "Bayesian model"
#' 
#' @import ggplot2
#' @importFrom MASS kde2d
#' @importFrom grid unit
#' 
#' @examples
#' data(Vaccine)
#'
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(e=e,
#'           c=c,              # defines the variables of 
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
#' # Plots the contour and scatterplot of the bivariate 
#' # distribution of (Delta_e, Delta_c)
#' contour(m,          # uses the results of the economic evaluation 
#'                     #  (a "bcea" object)
#'       comparison=1, # if more than 2 interventions, selects the 
#'                     #  pairwise comparison 
#'       nlevels=4,    # selects the number of levels to be 
#'                     #  plotted (default=4)
#'       levels=NULL,  # specifies the actual levels to be plotted 
#'                     #  (default=NULL, so that R will decide)
#'       scale=0.5,    # scales the bandwidths for both x- and 
#'                     #  y-axis (default=0.5)
#'       graph="base"  # uses base graphics to produce the plot
#' )
#' 
#' @export
#' 
contour <- function(he, ...) {
  UseMethod('contour', he)
}

