
#' Plot Expected Value of Partial Information With Respect to a
#' Set of Parameters
#' 
#' @param evppi_obj An object in the class \code{evppi},
#' obtained by the call to the function \code{\link{evppi}}.
#' @template args-pos
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-) match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param col Sets the colour for the lines depicted in the graph.
#' @param ...  Arguments to be passed to methods, such as graphical parameters
#' (see \code{\link{par}}).
#' @return Plot with base R or ggplot2.
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
#' treats <- c("Status quo", "Vaccination")
#'  
#' # Run the health economic evaluation using BCEA
#' m <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#'
#' # Compute the EVPPI for a bunch of parameters
#' inp <- createInputs(vaccine)
#' 
#' # Compute the EVPPI using INLA/SPDE
#' x0 <- evppi(m, c("beta.1." , "beta.2."), input = inp$mat)
#' x1 <- evppi(m, c(32,48,49), input = inp$mat)
#' 
#' plot(x0, pos = c(0,1))
#' plot(x1, pos = "topright")
#' 
#' plot(x0, col = c("black", "red"), pos = "topright")
#' plot(x0, col = c(2,3), pos = "bottomright")
#' 
#' plot(x0, pos = c(0,1), graph = "ggplot2")
#' plot(x1, pos = "top", graph = "ggplot2")
#' 
#' plot(x0, col = c("black", "red"), pos = "right", graph = "ggplot2")
#' plot(x0, col = c(2,3), size = c(1,2), pos = "bottom", graph = "ggplot2")
#' 
#' plot(x0, graph = "ggplot2", theme = ggplot2::theme_linedraw())
#' 
#' if (FALSE)
#'  plot(x0, col = 3, pos = "topright")
#' # The vector 'col' must have the number of elements for an EVPI
#' # colour and each of the EVPPI parameters. Forced to black
#' 
plot.evppi <- function (evppi_obj,
                        pos = c(0, 0.8),
                        graph = c("base", "ggplot2"),
                        col = c(1,1),
                        ...) {
  
  graph <- match.arg(graph)
  
  if (is_baseplot(graph)) {
    
    evppi_plot_base(evppi_obj,
                    pos_legend = pos,
                    col = col,
                    ...)
    
  } else if (is_ggplot(graph)) {
    
    evppi_plot_ggplot(evppi_obj,
                      pos_legend = pos,
                      col = col,
                      ...)
  }
  
  ##TODO
  # } else if (is_plotly(graph)) {
  #   
  #   evppi_plot_plotly(evppi_obj,
  #                     pos_legend = pos,
  #                     graph_params)
  # }
}


#' @name evppi_plot_graph
#' @title Plot Expected Value of Partial Information With Respect to a
#' Set of Parameters
#' 
#' @param evppi_obj Result of call to \code{createInputs()}
#' @param pos_legend Legend position
#' @param col Line colours; vector
#' 
NULL

