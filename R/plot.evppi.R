
#' Plot Expected Value of Partial Information With Respect to a
#' Set of Parameters
#' 
#' @param x An object in the class `evppi`,
#' obtained by the call to the function [evppi()].
#' @template args-pos
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-) match the two options `"base"` or
#' `"ggplot2"`. Default value is `"base"`.
#' @param col Sets the colour for the lines depicted in the graph.
#' @param ...  Arguments to be passed to methods, such as graphical parameters
#' (see [par()]).
#' @return Plot with base R or \pkg{ggplot2}.
#' 
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()], [evppi()]
#' @importFrom Rdpack reprompt
#' @import voi
#' 
#' @references
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' 
#' @export
#' @examples
#' 
#' \dontrun{
#' data(Vaccine, package = "BCEA")
#' treats <- c("Status quo", "Vaccination")
#'
#' # Run the health economic evaluation using BCEA
#' m <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#'
#' # Compute the EVPPI for a bunch of parameters
#' inp <- createInputs(vaccine_mat)
#' 
#' # Compute the EVPPI using INLA/SPDE
#' if (require("INLA")) {
#'   x0 <- BCEA::evppi(m, c("beta.1." , "beta.2."), input = inp$mat)
#'   
#'   plot(x0, pos = c(0,1))
#' 
#'   x1 <- BCEA::evppi(m, c(32,48,49), input = inp$mat)
#'   plot(x1, pos = "topright")
#' 
#'   plot(x0, col = c("black", "red"), pos = "topright")
#'   plot(x0, col = c(2,3), pos = "bottomright")
#' 
#'   plot(x0, pos = c(0,1), graph = "ggplot2")
#'   plot(x1, pos = "top", graph = "ggplot2")
#' 
#'   plot(x0, col = c("black", "red"), pos = "right", graph = "ggplot2")
#'   plot(x0, col = c(2,3), size = c(1,2), pos = "bottom", graph = "ggplot2")
#' 
#'   plot(x0, graph = "ggplot2", theme = ggplot2::theme_linedraw())
#' }
#' 
#' if (FALSE)
#'  plot(x0, col = 3, pos = "topright")
#' # The vector 'col' must have the number of elements for an EVPI
#' # colour and each of the EVPPI parameters. Forced to black
#' }
#' 
plot.evppi <- function (x,
                        pos = c(0, 0.8),
                        graph = c("base", "ggplot2"),
                        col = c(1, 1),
                        ...) {
  
  graph <- match.arg(graph)
  
  if (is_baseplot(graph)) {
    
    evppi_plot_base(x,
                    pos_legend = pos,
                    col = col,
                    ...)
    
  } else if (is_ggplot(graph)) {
    
    evppi_plot_ggplot(x,
                      pos_legend = pos,
                      col = col,
                      ...)
  
  } else if (is_plotly(graph)) {
  
  ##TODO
  #  evppi_plot_plotly(x,
  #                    pos_legend = pos,
  #                    graph_params)
  }
}

