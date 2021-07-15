
#' @rdname eib.plot
#' @importFrom graphics lines abline text legend
#' @import ggplot2
#' @export
#' 
eib.plot.bcea <- function(he,
                          comparison = NULL,
                          pos = c(1, 0),
                          size = NULL,
                          plot.cri = NULL,
                          graph = c("base", "ggplot2", "plotly"),
                          ...) {
  
  ##TODO: this is needed because its not defined in bcea()
  he$change_comp <- FALSE
  
  alt.legend <- pos
  # choose graphical engine
  if (any(is.null(graph)) || any(is.na(graph))) graph <- "base"
  
  graph_choice <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  
  if (graph_choice == 2 &&
      !requireNamespace("ggplot2", quietly = TRUE) &
      requireNamespace("grid", quietly = TRUE)) {
    warning("Package ggplot2 and grid not found;
            eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  if (graph_choice == 3 &&
      !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package plotly not found;
            eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  
  ## evaluate arguments
  ## possibility to include different values of confidence as "alpha"
  exArgs <- list(...)
  alpha <- 0.05
  plot_annotations <-
    list("exist" = list(
      "title" = FALSE,
      "xlab" = FALSE,
      "ylab" = FALSE))
  plot_aes <-
    list(
      "area" = list("include" = FALSE, "color" = "grey"),
      "line" = list(
        "colors" = "black",
        "types" = NULL,
        "cri_colors" = "grey50"))
  
  plot_aes_args <- c("area_include",
                     "area_color",
                     "line_colors",
                     "line_types",
                     "line_cri_colors")
  
  cri.quantile <- TRUE
  
  if (length(exArgs) >= 1) {
    if (exists("cri.quantile", where = exArgs))
      cri.quantile <- exArgs$cri.quantile
    if (exists("alpha", where = exArgs)) {
      alpha <- exArgs$alpha
      if (alpha < 0 | alpha > 1) {
        warning("Argument alpha must be between 0 and 1. Reset to default value 0.95.")
        alpha <- 0.05
      }
      if (alpha > 0.80 & cri.quantile) {
        warning(
          "It is recommended adopting the normal approximation of the credible interval for high values of alpha.
            Please set the argument cri.quantile = FALSE to use the normal approximation.")
      }
    }
    # if existing, read and store title, xlab and ylab
    for (annotation in names(plot_annotations$exist)) {
      if (exists(annotation, where = exArgs)) {
        plot_annotations$exist[[annotation]] <- TRUE
        plot_annotations[[annotation]] <- exArgs[[annotation]]
      }
    }
    # if existing, read and store graphical options
    for (aes_arg in plot_aes_args) {
      if (exists(aes_arg, where = exArgs)) {
        aes_cat <- strsplit(aes_arg, "_")[[1]][1]
        aes_name <- paste0(strsplit(aes_arg, "_")[[1]][-1], collapse = "_")
        plot_aes[[aes_cat]][[aes_name]] <- exArgs[[aes_arg]]
      }
    }
  }
  
  ## if plot.cri is null, if comp=1 plot them otherwise do not (clutter!)
  if (is.null(plot.cri) & isTRUE(he$n_comparisons == 1 | is.null(comparison)))
    plot.cri <- he$n_comparisons == 1
  
  ## calculate credible intervals if necessary
  if (plot.cri)
    cri <- eib.plot.cri(he, alpha, cri.quantile)
  
  ## calculate plot vertical limits
  yl <- ifelse(rep(!isTRUE(plot.cri), 2),
               yes = range(c(he$eib)),
               no = range(c(he$eib), c(cri[, 1:2])))
  
  if (graph_choice == 1) {

    eib_plot_base(he,
                  alt.legend,
                  plot_aes,
                  plot_annotations,
                  size,
                  yl,
                  alpha,
                  cri,
                  plot.cri)
    
  } else if (graph_choice == 2) {
    
    eib_plot_ggplot(he,
                    alt.legend,
                    plot_aes,
                    data.psa,
                    plot.cri,
                    plot_annotations,
                    alpha,
                    size,
                    exArgs,
                    yl,
                    cri)
    
  } else if (graph_choice == 3) {
    
    eib_plot_plotly(he,
                    alt.legend,
                    plot_aes,
                    plot_annotations,
                    plot.cri,
                    cri.quantile,
                    comparison,
                    alpha,
                    cri,
                    size)
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

