
#' Cost-effectiveness plane plot
#' 
#' Produces a scatter plot of the cost-effectiveness plane, together with the
#' sustainability area, as a function of the selected willingness to pay
#' threshold
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#'   modelling and the economic evaluation.
#' @param comparison Selects the comparator, in case of more than two
#'   interventions being analysed. Default as \code{NULL} plots all the
#'   comparisons together. Any subset of the possible comparisons can be selected
#'   (e.g., \code{comparison=c(1,3)} or \code{comparison=2}).
#' @param wtp The value of the willingness to pay parameter. Not used iff
#'   \code{graph="base"} for multiple comparisons.
#' @param pos Parameter to set the position of the legend; for a single
#'   comparison plot, the ICER legend position. Can be given in form of a string
#'   \code{(bottom|top)(right|left)} for base graphics and
#'   \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#'   which specifies the relative position on the x and y axis respectively, or
#'   alternatively it can be in form of a logical variable, with \code{FALSE}
#'   indicating to use the default position and \code{TRUE} to place it on the
#'   bottom of the plot. Default value is \code{c(1,1)}, that is the topright
#'   corner inside the plot area.
#' @param size Value (in millimetres) of the size of the willingness to pay
#'   label. Used only if \code{graph="ggplot2"}, otherwise is ignored with a
#'   message.
#' @param graph A string used to select the graphical engine to use for
#'   plotting. Should (partial-)match the two options \code{"base"} or
#'   \code{"ggplot2"}. Default value is \code{"base"}.
#' @param xlim The range of the plot along the x-axis. If NULL (default) it is
#'   determined by the range of the simulated values for \code{delta.e}
#' @param ylim The range of the plot along the y-axis. If NULL (default) it is
#'   determined by the range of the simulated values for \code{delta.c}
#' @param ...  If \code{graph="ggplot2"} and a named theme object is supplied,
#'   it will be added to the ggplot object. Additional graphical arguments:
#'  \itemize{
#'   \item \code{label.pos=FALSE} will place the willingness to pay label in a different 
#'   position at the bottom of the graph - base and ggplot2 only (no label in plotly);
#'   \item \code{point_colors}: a vector of colours specifying the colour(s) associated to 
#'   the cloud of points. Should be of length 1 or equal to the number of comparisons.
#'   \item \code{point_sizes}: a vector of colours specifying the size(s) of the points.
#'   Should be of length 1 or equal to the number of comparisons.
#'   \item \code{ICER_colors}: a vector of colours specifying the colour(s) of the ICER points.
#'   Should be of length 1 or equal to the number of comparisons.
#'   \item \code{ICER_sizes}:  a vector of colours specifying the size(s) of the ICER points.
#'   Should be of length 1 or equal to the number of comparisons.
#'   \item \code{area_include}: logical, include or exclude the cost-effectiveness 
#'   acceptability area (default is TRUE).
#'   \item \code{area_color}: a color specifying the colour of the cost-effectiveness acceptability area
#'  }
#' @return \item{ceplane}{ If \code{graph="ggplot2"} a ggplot object, or if \code{graph="plotly"} 
#'   a plotly object containing the requested plot. Nothing is returned when \code{graph="base"}, 
#'   the default.} 
#'   The function produces a plot of the
#'   cost-effectiveness plane. Grey dots show the simulated values for the joint
#'   distribution of the effectiveness and cost differentials.  The larger red
#'   dot shows the ICER and the grey area identifies the sustainability area,
#'   i.e. the part of the plan for which the simulated values are below the
#'   willingness to pay threshold. The proportion of points in the sustainability
#'   area effectively represents the CEAC for a given value of the willingness to
#'   pay. If the comparators are more than 2 and no pairwise comparison is
#'   specified, all scatterplots are graphed using different colors.
#' @details In the plotly version, point_colors, ICER_colors and area_color can also be specified
#'   as rgba colours using either the \code{\link[plotly]{toRGB}{plotly::toRGB}} function or
#'   a rgba colour string, e.g. \code{'rgba(1, 1, 1, 1)'}.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Cost Effectiveness Plane
#' @examples
#' 
#' ### create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m <- bcea(e,c,ref=4,Kmax=500,interventions=treats)
#' ### produce the plot
#' ceplane.plot(m,wtp=200,graph="base")
#' ### select only one comparator
#' ceplane.plot(m,wtp=200,graph="base",comparator=3)
#' ### or use ggplot2 instead
#' if(requireNamespace("ggplot2")){
#' ceplane.plot(m,wtp=200,pos="right",ICER_sizes=2,graph="ggplot2")
#' }
#' 
#' @export
#' 
ceplane.plot <- function(he,
                         comparison = NULL,
                         wtp = 25000,
                         pos = c(1, 1),
                         size = NULL,
                         graph = c("base", "ggplot2"),
                         xlim = NULL,
                         ylim = NULL,
                         ...) {
  # avoid scientific format for graphs labels
  options(scipen = 10)
  exArgs <- list(...)
  
  
  ### hidden options for ggplot2 ###
  # ICER.size =                    # changes ICER point size
  # label.pos = FALSE              # uses alternate position for wtp label (old specification)
  alt.legend <- pos
  # choose graphical engine
  if (is.null(graph) || is.na(graph)) graph = "base"
  graph_choice <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  
  is_pkg_avail <- requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)
  
  # check feasibility
  if (graph_choice == 2 && !is_pkg_avail) {
    warning("Package ggplot2 and grid not found; ceplane.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  if (graph_choice == 3 && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package plotly not found; ceplane.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  
  # evaluate additional arguments -----
  plot_annotations <- list("exist" = list("title" = FALSE, "xlab" = FALSE, "ylab" = FALSE))
  plot_aes <- list("area" = list("include" = TRUE, "color" = "light gray", "line_color" = "black"),
                   "point" = list("colors" = "black", "sizes" = 4),
                   "ICER" = list("colors" = "red", "sizes" = 8),
                   "exist" = list("area" = list("include" = FALSE, "color" = FALSE, "line_color" = FALSE),
                                  "point" = list("colors" = FALSE, "sizes" = FALSE),
                                  "ICER" = list("colors" = FALSE, "sizes" = FALSE)))
  plot_aes_args = c("area_include", "area_color", "area_line_color",
                    "point_colors", "point_sizes",
                    "ICER_colors", "ICER_sizes")
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
        plot_aes$exist[[aes_cat]][[aes_name]] <- TRUE
      }
    }
  }
  # Args compatibility
  if (exists("ICER.size", where = exArgs)) {
    if (plot_aes$exist$ICER$sizes) {
      warning("Both ICER.size and ICER_sizes arguments specified. ICER_sizes will be used.")
    } else {
      warning("ICER.size is softly deprecated. Please use ICER_sizes instead.")
      plot_aes$exist$ICER$sizes <- TRUE
      plot_aes$ICER$sizes <- exArgs$ICER.size
    }
  }
  if (exists("ICER.col", where = exArgs)) {
    if (plot_aes$exist$ICER$colors) {
      warning("Both ICER.col and ICER_col arguments specified. ICER_col will be used.")
    } else {
      warning("ICER.col is softly deprecated. Please use ICER_col instead.")
      plot_aes$exist$ICER$colors <- TRUE
      plot_aes$ICER$colors <- exArgs$ICER.col
    }
  }
  if (exists("col", where = exArgs)) {
    if (plot_aes$exist$point$colors) {
      warning("Both col and point_colors arguments specified. point_colors will be used.")
    } else {
      warning("col argument is softly deprecated. Please use point_colors instead.")
      plot_aes$exist$point$colors <- TRUE
      plot_aes$point$colors <- exArgs$col
    }
  }
  # set default colour scheme
  if (!plot_aes$exist$point$colors) {
    if (he$n.comparisons > 1 & (is.null(comparison) || length(comparison) > 1)) {
      plot_aes$point$colors <- colors()[floor(seq(262, 340, length.out = he$n.comparisons))]
    } else {
      plot_aes$point$colors <- "grey55"
    }
  }
  # default plot annotations -----
  if (!plot_annotations$exist$title)
    plot_annotations$title <- with(he, paste0(
      "Cost-Effectiveness Plane",
      ifelse(
        n.comparisons == 1 | (n.comparisons > 1 & (!is.null(comparison) && length(comparison) == 1)),
        paste0("\n", interventions[ref], " vs ", interventions[-ref]),
        paste0(ifelse(
          isTRUE(he$mod),
          paste0(
            "\n",
            interventions[ref],
            " vs ",
            paste0(interventions[comp], collapse = ", ")
          ),
          ""
        ))
      )
    ))
  if (!plot_annotations$exist$xlab)
    plot_annotations$xlab = "Effectiveness differential"
  if (!plot_annotations$exist$ylab)
    plot_annotations$ylab = "Cost differential"
  
  if (graph_choice == 1) {

    ##TODO:...
    # ceplane_plot_base()
    
  } else if (graph_choice == 2) {
    
    ##TODO:...
    # ceplane_plot_ggplot()
    
  } else if (graph_choice == 3) {
    
    ##TODO:...
    # ceplane_plot_plotly()
  }
}

