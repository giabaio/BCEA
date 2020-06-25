
#' Cost-Effectiveness Acceptability Curve (CEAC) plot
#' 
#' Produces a plot of the Cost-Effectiveness Acceptability Curve (CEAC) against
#' the willingness to pay threshold.
#' 
#' @rdname plot-bcea
#' 
#' @template args-he
#' @template args-comparison
#' @param pos Parameter to set the position of the legend (only relevant for
#'   multiple interventions, ie more than 2 interventions being compared). Can be
#'   given in form of a string \code{(bottom|top)(right|left)} for base graphics
#'   and \code{bottom}, \code{top}, \code{left} or \code{right} for *ggplot2*.
#'   It can be a two-elements vector, which specifies the relative position on the x
#'   and y axis respectively, or alternatively it can be in form of a logical
#'   variable, with \code{FALSE} indicating to use the default position and
#'   \code{TRUE} to place it on the bottom of the plot. Default value is
#'   \code{c(1,0)}, that is the bottom right corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#'   plotting. Should (partial-)match the three options \code{"base"},
#'   \code{"ggplot2"} or \code{"plotly"}. Default value is \code{"base"}.
#' @param ...  If \code{graph="ggplot2"} and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional arguments:
#'  \itemize{
#'   \item \code{line_colors}: specifies the line colour(s) - all graph types.
#'   \item \code{line_types}: specifies the line type(s) as lty numeric values - all graph types.
#'   \item \code{area_include}: logical, include area under the CEAC curves - plotly only.
#'   \item \code{area_color}: specifies the AUC colour - plotly only.}
#'   
#' @return \item{ceac} {If \code{graph="ggplot2"} a ggplot object, or if \code{graph="plotly"} 
#'   a plotly object containing the requested plot. Nothing is returned when \code{graph="base"}, 
#'   the default.} The function produces a plot of the
#'   cost-effectiveness acceptability curve against the discrete grid of possible
#'   values for the willingness to pay parameter. Values of the CEAC closer to 1
#'   indicate that uncertainty in the cost-effectiveness of the reference
#'   intervention is very low. Similarly, values of the CEAC closer to 0 indicate
#'   that uncertainty in the cost-effectiveness of the comparator is very low.
#'   
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#'   Analysis in Health Economics.  Statistical Methods in Medical Research
#'   doi:10.1177/0962280211419832.
#' 
#'   Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
#' @keywords Health economic evaluation Cost Effectiveness Acceptability Curve
#' @export
#' 
#' @examples 
#' 
ceac.plot <- function(he,
                      comparison = NULL,
                      pos = c(1, 0),
                      graph = c("base", "ggplot2", "plotly"),
                      ...) {
  
  options(scipen = 10)
  graph <- match.arg(graph)
  exArgs <- list(...)
  
  alt.legend <- pos
  # choose graphical engine
  if (is.null(graph) || is.na(graph)) graph <- "base"
  
  graph_choice <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  
  is_pkg_avail <- requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)
  
  # check feasibility
  if (graph_choice == 2 && !is_pkg_avail) {
    warning("Package ggplot2 and grid not found; ceac.plot will be rendered using base graphics.", call. = FALSE)
    graph_choice <- 1
  }
  if (graph_choice == 3 && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package plotly not found; ceac.plot will be rendered using base graphics.", call. = FALSE)
    graph_choice <- 1
  }
  
  # evaluate additional arguments -----
  plot_annotations <- list("exist" = list("title" = FALSE, "xlab" = FALSE, "ylab" = FALSE))
  plot_aes <- list("area" = list("include" = TRUE, "color" = NULL),
                   "line" = list("colors" = "black", "types" = NULL))
  plot_aes_args = c("area_include", "area_color", "line_colors", "line_types")
  
  if (length(exArgs) >= 1) {
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
  
  # default plot annotations -----
  if (!plot_annotations$exist$title)
    plot_annotations$title = "Cost Effectiveness Acceptability Curve"
  if (!plot_annotations$exist$xlab)
    plot_annotations$xlab = "Willingness to pay"
  if (!plot_annotations$exist$ylab)
    plot_annotations$ylab = "Probability of cost effectiveness"
  
  if (graph_choice == 1) {
    
    ##TODO:
    # .ceac_plot_base()
    
  } else if (graph_choice == 2) {
    
    ##TODO:    
    # .ceac_plot_ggplot()
    
  } else if (graph_choice == 3) {
    
    ##TODO:
    # .ceac_plot_plotly()
  }
}
