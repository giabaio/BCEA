
#' @rdname eib.plot
#' 
#' @importFrom graphics lines abline text legend
#' @import ggplot2
#' 
#' @template args-he
#' @template args-comparison
#' @template args-pos
#' @param size Value (in millimetres) of the size of the willingness to pay
#' label. Used only if `graph="ggplot2"`, otherwise it will be ignored
#' with a message. If set to `NA`, the break-even point line(s) and
#' label(s) are suppressed, with both base graphics and ggplot2.
#' @param plot.cri Logical value. Should the credible intervals be plotted
#' along with the expected incremental benefit? Default as `NULL` draws
#' the 95\% credible intervals if only one comparison is selected, and does not
#' include them for multiple comparisons.  Setting `plot.cri=TRUE` or
#' `plot.cri=FALSE` forces the function to add the intervals or not. The
#' level of the intervals can be also set, see \ldots{} for more details.
#' @template args-graph
#' @param ...  If `graph="ggplot2"` and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional arguments:
#'   
#' - `alpha_cri`: Can be used to set the CrI level when `plot.cri=TRUE`, 
#'   with a default value of `alpha=0.05`.
#' - `cri.quantile`: Controls the method of calculation of the credible 
#'   intervals. The default value, `cri.quantile=TRUE`, defines the CrI as the 
#'   interval between the `alpha/2`-th and `1-alpha/2`-th quantiles of 
#'   the IB distribution. Setting `cri.quantile=FALSE` will use a normal 
#'   approximation on the IB distribution to calculate the intervals.
#' - `currency`: Currency prefix to willingness-to-pay values (applies to 
#'   \pkg{ggplot2} only).
#' - `line_colors`: Specifies the line colour(s) (applies to all graph types).
#' - `line_types`: Specifies the line type(s) as `lty` numeric values 
#'   (applies to all graph types).
#' - `area_include`: Includes the area under the EIB curve (applies to 
#'   \pkg{plotly} only).
#' - `area_color`: Specifies the AUC curve colour (applies to \pkg{plotly} only).
#'   
#' @export
#' 
eib.plot.bcea <- function(he,
                          comparison = NULL,
                          pos = "bottomright",
                          size = NULL,
                          plot.cri = NULL,
                          graph = c("base", "ggplot2", "plotly"),
                          ...) {

  graph <- match.arg(graph)
  
  he <- setComparisons(he, comparison)
  
  graph_params <- c(prep_eib_params(he, plot.cri, ...),
                    list(pos = pos,
                         size = size,
                         plot.cri = plot.cri))
  
  if (is_baseplot(graph)) {
    
    eib_plot_base(he,
                  graph_params,
                  ...)
    
  } else if (is_ggplot(graph)) {
    
    eib_plot_ggplot(he,
                    graph_params,
                    ...)
    
  } else if (is_plotly(graph)) {
    
    eib_plot_plotly(he,
                    graph_params,
                    ...)
  }
}


#' @title Expected Incremental Benefit (EIB) Plot
#' 
#' @description Produces a plot of the Expected Incremental Benefit (EIB) as a function of
#' the willingness to pay.
#' 
#' @template args-he
#' @param ...  If `graph="ggplot2"` and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional arguments:
#'  \itemize{
#'   \item `alpha` can be used to set the CrI level when `plot.cri=TRUE`,
#'   with a default value of `alpha=0.05`.
#'   \item `cri.quantile` controls the the method of calculation of the credible
#'   intervals. The default value `cri.quantile=TRUE` defines the CrI as the
#'   interval between the `alpha/2`-th and `1-alpha/2`-th quantiles of
#'   the IB distribution. Setting `cri.quantile=FALSE` will use a normal
#'   approximation on the IB distribution to calculate the intervals.
#'   \item `line = list(color)`: specifies the line colour(s) - all graph types.
#'   \item `line = list(type)`: specifies the line type(s) as lty numeric values - all graph types.
#'   \item `area_include`: include area under the EIB curve - plotly only.
#'   \item `area_color`: specifies the AUC curve - plotly only.}
#'    
#' @return \item{eib}{ If `graph="ggplot2"` a ggplot object, or if `graph="plotly"` 
#' a plotly object containing the requested plot. Nothing is returned when `graph="base"`, 
#' the default.} The function produces a plot of the
#' Expected Incremental Benefit as a function of the discrete grid
#' approximation of the willingness to pay parameter. The break even point
#' (i.e. the point in which the EIB = 0, i.e. when the optimal decision changes
#' from one intervention to another) is also showed by default. The value `k*` is
#' the discrete grid approximation of the ICER.
#' 
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()],
#'          [ib.plot()],
#'          [ceplane.plot()]
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom Rdpack reprompt
#' 
#' @export
#' 
#' @examples
#' data(Vaccine)
#'  
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(
#'       e=eff,
#'       c=cost,               # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e, c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0, Kmax)
#'       plot=FALSE            # plots the results
#' )
#' eib.plot(m)
#' eib.plot(m, graph = "ggplot2") + ggplot2::theme_linedraw()
#' 
#' data(Smoking)
#' treats <- c("No intervention", "Self-help",
#'             "Individual counselling", "Group counselling")
#' m <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)
#' eib.plot(m)
#' 
eib.plot <- function(he, ...) {
  UseMethod('eib.plot', he)
}

