
#' @rdname eib.plot
#' @importFrom graphics lines abline text legend
#' @import ggplot2
#' @export
#' 
eib.plot.bcea <- function(he,
                          comparison = NULL,
                          pos = c(1, 0),
                          size = NULL,
                          plot.cri = FALSE,
                          graph = c("base", "ggplot2", "plotly"),
                          ...) {
  
  graph <- match.arg(graph)
  
  he <- setComparisons(he, comparison)
  
  graph_params <- c(prep_eib_params(he, ...),
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
                    alt.legend,
                    plot_aes,
                    plot_annotations,
                    plot.cri,
                    cri.quantile,
                    comparison,
                    alpha,
                    cri,
                    size,
                    ...)
  }
}


#' Expected Incremental Benefit (EIB) Plot
#' 
#' Produces a plot of the Expected Incremental Benefit (EIB) as a function of
#' the willingness to pay.
#' 
#' @template args-he
#' @template args-comparison
#' @param pos Parameter to set the position of the legend; for a single
#' comparison plot, the ICER legend position. Can be given in form of a string
#' \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#' which specifies the relative position on the x and y axis respectively, or
#' alternatively it can be in form of a logical variable, with \code{FALSE}
#' indicating to use the default position and \code{TRUE} to place it on the
#' bottom of the plot. Default value is \code{c(1,0)}, that is the bottomright
#' corner inside the plot area.
#' @param size Value (in millimetres) of the size of the willingness to pay
#' label. Used only if \code{graph="ggplot2"}, otherwise it will be ignored
#' with a message. If set to \code{NA}, the break-even point line(s) and
#' label(s) are suppressed, with both base graphics and ggplot2.
#' @param plot.cri Logical value. Should the credible intervals be plotted
#' along with the expected incremental benefit? Default as \code{NULL} draws
#' the 95\% credible intervals if only one comparison is selected, and does not
#' include them for multiple comparisons.  Setting \code{plot.cri=TRUE} or
#' \code{plot.cri=FALSE} forces the function to add the intervals or not. The
#' level of the intervals can be also set, see \ldots{} for more details.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the three options \code{"base"},
#' \code{"ggplot2"} or \code{"plotly"}. Default value is \code{"base"}.
#' @param ...  If \code{graph="ggplot2"} and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional arguments:
#'  \itemize{
#'   \item \code{alpha} can be used to set the CrI level when \code{plot.cri=TRUE},
#'   with a default value of \code{alpha=0.05}.
#'   \item \code{cri.quantile} controls the the method of calculation of the credible
#'   intervals. The default value \code{cri.quantile=TRUE} defines the CrI as the
#'   interval between the \code{alpha/2}-th and \code{1-alpha/2}-th quantiles of
#'   the IB distribution. Setting \code{cri.quantile=FALSE} will use a normal
#'   approximation on the IB distribution to calculate the intervals.
#'   \item \code{line_colors}: specifies the line colour(s) - all graph types.
#'   \item \code{line_types}: specifies the line type(s) as lty numeric values - all graph types.
#'   \item \code{area_include}: include area under the EIB curve - plotly only.
#'   \item \code{area_color}: specifies the AUC curve - plotly only.}
#'   
#' @return \item{eib}{ If \code{graph="ggplot2"} a ggplot object, or if \code{graph="plotly"} 
#' a plotly object containing the requested plot. Nothing is returned when \code{graph="base"}, 
#' the default.} The function produces a plot of the
#' Expected Incremental Benefit as a function of the discrete grid
#' approximation of the willingness to pay parameter. The break even point
#' (i.e. the point in which the EIB = 0, i.e. when the optimal decision changes
#' from one intervention to another) is also showed by default. The value `k*` is
#' the discrete grid approximation of the ICER.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}},
#'          \code{\link{ib.plot}},
#'          \code{\link{ceplane.plot}}
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics. Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' 
#' @keywords "Health economic evaluation" "Expected Incremental Benefit"
#' @import ggplot2
#' @importFrom grid unit
#' @export
#' 
#' @examples
#' data(Vaccine)
#'  
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(
#'       e=e,
#'       c=c,                  # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e, c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0, Kmax)
#'       plot=TRUE             # plots the results
#' )
#' eib.plot(m)
#' eib.plot(m, graph = "ggplot2") + ggplot2::theme_linedraw()
#' 
#' data(Smoking)
#' treats <- c("No intervention", "Self-help",
#'             "Individual counselling", "Group counselling")
#' m <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)
#' eib.plot(m)
#' 
eib.plot <- function(he, ...) {
  UseMethod('eib.plot', he)
}

