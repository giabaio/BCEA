
#' @rdname evi.plot
#' 
#' @template args-he
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the three options `"base"`,
#' `"ggplot2"` or `"plotly"`. Default value is `"base"`.
#' @param ... Additional parameters
#' 
#' @return \item{eib}{ If `graph="ggplot2"` a ggplot object, or if `graph="plotly"` 
#'   a plotly object containing the requested plot. Nothing is returned when `graph="base"`, 
#'   the default.} The function produces a plot of the
#'   Expected Value of Information as a function of the discrete grid
#'   approximation of the willingness to pay parameter. The break even point(s)
#'   (i.e. the point in which the EIB=0, ie when the optimal decision changes
#'   from one intervention to another) is(are) also showed.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()],
#'          [ceac.plot()],
#'          [ceplane.plot()]
#' @importFrom Rdpack reprompt
#' 
#' @references
#' 
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' @export
#' 
#' @examples
#' data(Vaccine)
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
#' evi.plot(m)
#' 
#' data(Smoking)
#' treats <- c("No intervention", "Self-help",
#'             "Individual counselling", "Group counselling")
#' m <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)
#' evi.plot(m)
#' 
evi.plot.bcea <- function(he,
                          graph = c("base", "ggplot2", "plotly"),
                          ...) {
  
  graph <- match.arg(graph)
  
  extra_args <- list(...)
  
  plot_annotations <-
    list("exist" = list("title" = FALSE,
                        "xlab" = FALSE,
                        "ylab" = FALSE))
  
  plot_aes <- list("area" = list("include" = TRUE,
                                 "color" = "grey50"),
                   "line" = list("colors" = "black",
                                 "types" = NULL))
  
  plot_aes_args <- c("area_include", "area_color", "line_colors", "line_types")
  
  ##TODO: should we be using this?
  plot_aes$cri.quantile <- TRUE
  
  if (length(extra_args) >= 1) {
    # if existing, read and store title, xlab and ylab
    for (annotation in names(plot_annotations$exist)) {
      if (exists(annotation, where = extra_args)) {
        plot_annotations$exist[[annotation]] <- TRUE
        plot_annotations[[annotation]] <- extra_args[[annotation]]
      }
    }
    # if existing, read and store graphical options
    for (aes_arg in plot_aes_args) {
      if (exists(aes_arg, where = extra_args)) {
        aes_cat <- strsplit(aes_arg, "_")[[1]][1]
        aes_name <- paste0(strsplit(aes_arg, "_")[[1]][-1], collapse = "_")
        plot_aes[[aes_cat]][[aes_name]] <- extra_args[[aes_arg]]
      }
    }
  }

  if (!plot_annotations$exist$title)
    plot_annotations$title <- "Expected Value of Information"
  
  if (!plot_annotations$exist$xlab)
    plot_annotations$xlab <- "Willingness to pay"
  
  if (!plot_annotations$exist$ylab)
    plot_annotations$ylab <- "EVPI"
  
  # fonts
  default_params <- list(text = list(size = 11))
  graph_params <- modifyList(default_params, extra_args)
  plot_annotations$text <- graph_params$text
  
  data.psa <- data.frame(k = c(he$k),
                         evi = c(he$evi))
  
  if (is_baseplot(graph)) {
    
    evi_plot_base(he,
                  data.psa,
                  plot_aes,
                  plot_annotations)
    
  } else if (is_ggplot(graph)) {
    
    evi_plot_ggplot(he,
                    data.psa,
                    plot_aes,
                    plot_annotations)
    
  } else if (is_plotly(graph)) {
    
    evi_plot_plotly(data.psa,
                    plot_aes,
                    plot_annotations)
  }
}


#' Expected Value of Information (EVI) Plot
#' 
#' Plots the Expected Value of Information (EVI) against the willingness to pay.
#' 
#' @template args-he
#' @param ... Additional graphical arguments:
#'   \itemize{
#'     \item `line_colors` to specify the EVPI line colour - all graph types.
#'     \item `line_types` to specify the line type (lty) - all graph types.
#'     \item `area_include` to specify whether to include the area under the
#'     EVPI curve - plotly only.
#'     \item `area_color` to specify the area under the colour curve - plotly only.}
#'     
#' @export
#' 
evi.plot <- function(he, ...) {
  UseMethod('evi.plot', he)
}

