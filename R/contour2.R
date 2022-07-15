
#' @rdname contour2
#' @importFrom stats sd
#' @importFrom graphics par contour
#' 
#' @export
#' 
contour2.bcea <- function(he,
                          comparison = NULL,
                          wtp = 25000,
                          graph = c("base", "ggplot2"),
                          pos = c(0, 1),
                          ...) {
  
  graph_type <- match.arg(graph)
  
  he <- setComparisons(he, comparison)
  
  params <- prep_contour_params(he, ...)
  
  if (is_baseplot(graph_type)) {
    
    # encode characters so that the graph can
    # be saved as postscript or pdf
    ps.options(encoding = "CP1250")
    pdf.options(encoding = "CP1250")
    
    plot_params <-
      contour_base_params(he, params)
    
    ceplane.plot(he, comparison = NULL, wtp = wtp, pos = pos, graph = "base", ...)
    add_contours(he, plot_params)
    
  } else if (is_ggplot(graph_type)) {
    
    plot_params <-
      contour_ggplot_params(he, params, ...)
    
    ceplane.plot(he, comparison = NULL, wtp = wtp, pos = pos, graph = "ggplot2", ...) +
      do.call(geom_density_2d, plot_params$contour)     
  }
}


#' Specialised CE-plane Contour Plot
#' 
#' Produces a scatterplot of the cost-effectiveness plane, with a contour-plot
#' of the bivariate density of the differentials of cost (y-axis) and
#' effectiveness (x-axis).  Also adds the sustainability area (i.e. below the
#' selected value of the willingness-to-pay threshold).
#' 
#' @template args-he
#' @param comparison The comparison being plotted. Default to \code{NULL}
#' If \code{graph_type="ggplot2"} the default value will choose all the possible
#' comparisons. Any subset of the possible comparisons can be selected (e.g.,
#' \code{comparison=c(1,3)}).
#' @param wtp The selected value of the willingness-to-pay. Default is
#' \code{25000}.
#' @template args-graph
#' @template args-pos
#' @param ...  Arguments to be passed to \code{\link{ceplane.plot}}. See the
#' relative manual page for more details.
#' 
#' @return \item{contour}{ A ggplot item containing the requested plot.
#' Returned only if \code{graph_type="ggplot2"}. } Plots the cost-effectiveness
#' plane with a scatterplot of all the simulated values from the (posterior)
#' bivariate distribution of (\eqn{\Delta_e, \Delta_c}), the differentials of
#' effectiveness and costs; superimposes a contour of the distribution and
#' prints the value of the ICER, together with the sustainability area.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}},
#'          \code{\link{ceplane.plot}},
#'          \code{\link{contour}}
#'          
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' @import ggplot2
#' @importFrom grDevices ps.options pdf.options
#' @importFrom MASS kde2d
#' @importFrom Rdpack reprompt
#'  
#' @examples
#' ## create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)
#' 
#' ## produce the plot
#' contour2(m,
#'          wtp = 200,
#'          graph_type = "base")
#' 
#' \donttest{
#' ## or use ggplot2 to plot multiple comparisons
#' contour2(m,
#'          wtp = 200,
#'          ICER_size = 2,
#'          graph_type = "ggplot2")
#' }
#' 
#' ## vaccination example
#' data(Vaccine)
#' treats = c("Status quo", "Vaccination")
#' m <- bcea(e=e, c=c, ref = 2, interventions = treats, Kmax = 50000)
#' contour2(m)
#' contour2(m, wtp = 100)
#' 
#' @rdname contour2
#' @export
#' 
contour2 <- function(he, ...) {
  UseMethod('contour2', he)
}

