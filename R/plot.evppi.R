
#' Plot Expected Value of Partial Information With Respect to a
#' Set of Parameters
#' 
#' @param x An object in the class \code{evppi}, obtained by the call to the
#' function \code{\link{evppi}}.
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#' which specifies the relative position on the x and y axis respectively, or
#' alternatively it can be in form of a logical variable, with \code{FALSE}
#' indicating to use the default position and \code{TRUE} to place it on the
#' bottom of the plot. Default value is \code{c(0,1)}, that is in the topleft
#' corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-) match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param col Sets the colour for the lines depicted in the graph.
#' @param ...  Arguments to be passed to methods, such as graphical parameters
#' (see \code{\link{par}}).
#' @return Plot with base R or ggplot 2.
#' @import grid ggplot2
#' 
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{evppi}}
#' @references
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' 
#' @keywords "Health economic evaluation" "Expected value of information"
#' 
#' @export
#' @examples
#' 
#' data(Vaccine)
#' 
#' # Run the health economic evaluation using BCEA
#' m <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#'
#' # Compute the EVPPI for a bunch of parameters
#' inp <- createInputs(vaccine)
#' 
#' # Compute the EVPPI using INLA/SPDE
#' x0 <- evppi(he = m, 39:40, input = inp$mat)
#' x1 <- evppi(he = m, c(32,48,49), input = inp$mat)
#' 
#' plot(x0, pos = c(0,1))
#' plot(x1, pos = "topright")
#' 
#' plot(x0, col = c("black", red"), pos = "topright")
#' plot(x0, col = c(1,3), pos = "topright")
#' 
#' if (FALSE)
#'  plot(x0, col = 3, pos = "topright")
#' # The vector 'col' must have the number of elements for an EVPI
#' # colour and each of the EVPPI parameters. Forced to black
#' 
plot.evppi <- function (evppi_obj,
                        pos = c(0, 0.8),
                        graph = c("base", "ggplot2"),
                        col = NULL,
                        ...) {
  
  graph <- match.arg(graph)
  
  # graph_params <- prepare_evppi_params(...)
  
  if (is_baseplot(graph)) {
    
    evppi_plot_base(evppi_obj,
                    pos_legend = pos,
                    # graph_params,
                    col)
    
  } else if (is_ggplot(graph)) {
    
    evppi_plot_ggplot(evppi_obj,
                      pos_legend = pos,
                      # graph_params,
                      ...)
  }
  
  # } else if (is_plotly(graph)) {
  #   
  #   evppi_plot_plotly(evppi_obj,
  #                     pos_legend = pos,
  #                     graph_params)
  # }
}


