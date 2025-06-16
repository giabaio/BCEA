
#' @rdname contour2
#' @importFrom stats sd
#' @importFrom graphics par contour
#' @importFrom grDevices ps.options pdf.options
#' 
#' @export
#' 
contour2.bcea <- function(he,
                          comparison = NULL,
                          wtp = 25000,
                          graph = c("base", "ggplot2", "plotly"),
                          pos = "topleft",
                          ...) {
  
  graph_type <- match.arg(graph)
  
  he <- setComparisons(he, comparison)
  
  graph_params <- prep_contour_params(he, ...)
  
  if (is_baseplot(graph_type)) {
    
    # encode characters so that the graph can
    # be saved as postscript or pdf
    grDevices::ps.options(encoding = "CP1250")
    grDevices::pdf.options(encoding = "CP1250")
    
    plot_params <-
      contour_base_params(he, graph_params)
    
    ceplane.plot(he, comparison = NULL, wtp = wtp, pos = pos, graph = "base", ...)
    add_contours(he, plot_params)
    
  } else if (is_ggplot(graph_type)) {
    
    plot_params <-
      contour_ggplot_params(he, pos, graph_params, ...)
    
    ceplane.plot(he, comparison = NULL, wtp = wtp, pos = pos, graph = "ggplot2", ...) +
      do.call(geom_density_2d, plot_params$contour)
    
  } else if (is_plotly(graph_type)) {
    
    ##TODO: move all this to a prep function so its outside of the plotting function
    delta_ce <- prep_delta_ce(he)
    
    comp_label <- paste(he$interventions[he$ref], "vs", he$interventions[he$comp])
    
    if (length(graph_params$point$colors) != length(comp_label)) {
      graph_params$point$colors <-
        rep_len(graph_params$point$color, length(comp_label))
    }
    
    pt_cols <-
      ifelse(test = grepl(pattern = "^rgba\\(",
                          x = graph_params$point$colors),
             yes = plotly::toRGB(graph_params$point$colors),
             no = graph_params$point$colors)
    
    ceplane.plot(he, wtp = wtp, pos = pos,
                 graph = "plotly",
                 graph_params = graph_params, ...) |>
      contour_plotly_lines(he, delta_ce, graph_params, pt_cols)
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
#' @param comparison The comparison being plotted. Default to `NULL`
#' If `graph_type="ggplot2"` the default value will choose all the possible
#' comparisons. Any subset of the possible comparisons can be selected (e.g.,
#' `comparison=c(1,3)`).
#' @param wtp The selected value of the willingness-to-pay. Default is
#' `25000`.
#' @template args-graph
#' @template args-pos
#' @param ...  Arguments to be passed to [ceplane.plot()]. See the
#' relative manual page for more details.
#' 
#' @return \item{contour}{ A ggplot item containing the requested plot.
#' Returned only if `graph_type="ggplot2"`. } Plots the cost-effectiveness
#' plane with a scatterplot of all the simulated values from the (posterior)
#' bivariate distribution of (\eqn{\Delta_e, \Delta_c}), the differentials of
#' effectiveness and costs; superimposes a contour of the distribution and
#' prints the value of the ICER, together with the sustainability area.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()],
#'          [ceplane.plot()],
#'          [contour()]
#'          
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' @importFrom Rdpack reprompt
#'  
#' @examples
#' ## create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)
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
#' m <- bcea(eff, cost, ref = 2, interventions = treats, Kmax = 50000)
#' contour2(m)
#' contour2(m, wtp = 100)
#' 
#' @rdname contour2
#' @export
#' 
contour2 <- function(he, ...) {
  UseMethod('contour2', he)
}

