
#' @rdname BCEA-deprecated
#' @section `plot.mixedAn`:
#' For `plot.mixedAn`, use [evi.plot()].
#' 
#' Summary plot of the health economic analysis when the mixed analysis is
#' considered
#' 
#' Compares the optimal scenario to the mixed case in terms of the EVPI.
#' 
#' @param x An object of class `mixedAn`, given as output of the call to
#' the function [mixedAn()].
#' @param y.limits Range of the y-axis for the graph. The default value is
#' `NULL`, in which case the maximum range between the optimal and the
#' mixed analysis scenarios is considered.
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string `(bottom|top)(right|left)` for base graphics and
#' `bottom|top|left|right` for ggplot2. It can be a two-elements vector,
#' which specifies the relative position on the x and y axis respectively, or
#' alternatively it can be in form of a logical variable, with `FALSE`
#' indicating to use the default position and `TRUE` to place it on the
#' bottom of the plot. Default value is `c(0,1)`, that is in the topleft
#' corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options `"base"` or
#' `"ggplot2"`. Default value is `"base"`.
#' @param ...  Arguments to be passed to methods, such as graphical parameters
#' (see [par()]).
#' @return \item{evi}{ A ggplot object containing the plot. Returned only if
#' `graph="ggplot2"`. } The function produces a graph showing the
#' difference between the ''optimal'' version of the EVPI (when only the most
#' cost-effective intervention is included in the market) and the mixed
#' strategy one (when more than one intervention is considered in the market).
#' @author Gianluca Baio, Andrea Berardi
#' @usage plot.mixedAn(x, y.limits=NULL, pos=c(0,1), graph=c("base","ggplot2"),...)
#'
#' @export
#' 
plot.mixedAn <- function(x, y.limits = NULL, pos = c(0,1),
                         graph = c("base","ggplot2"), ...) {
  .Deprecated(new = "evi.plot")
}

