# ceac.plot -----

#' Cost-Effectiveness Acceptability Curve (CEAC) plot
#' 
#' Produces a plot of the Cost-Effectiveness Acceptability Curve (CEAC) against
#' the willingness to pay threshold
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#'   modelling and the economic evaluation.
#' @param comparison Selects the comparator, in case of more than two
#'   interventions being analysed. Default as NULL plots all the comparisons
#'   together. Any subset of the possible comparisons can be selected (e.g.,
#'   \code{comparison=c(1,3)} or \code{comparison=2}).
#' @param pos Parameter to set the position of the legend (only relevant for
#'   multiple interventions, ie more than 2 interventions being compared). Can be
#'   given in form of a string \code{(bottom|top)(right|left)} for base graphics
#'   and \code{bottom}, \code{top}, \code{left} or \code{right} for ggplot2. It
#'   can be a two-elements vector, which specifies the relative position on the x
#'   and y axis respectively, or alternatively it can be in form of a logical
#'   variable, with \code{FALSE} indicating to use the default position and
#'   \code{TRUE} to place it on the bottom of the plot. Default value is
#'   \code{c(1,0)}, that is the bottomright corner inside the plot area.
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
#' @return \item{ceac}{ If \code{graph="ggplot2"} a ggplot object, or if \code{graph="plotly"} 
#'   a plotly object containing the requested plot. Nothing is returned when \code{graph="base"}, 
#'   the default.} The function produces a plot of the
#'   cost-effectiveness acceptability curve against the discrete grid of possible
#'   values for the willingness to pay parameter. Values of the CEAC closer to 1
#'   indicate that uncertainty in the cost-effectiveness of the reference
#'   intervention is very low. Similarly, values of the CEAC closer to 0 indicate
#'   that uncertainty in the cost-effectiveness of the comparator is very low.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#'   Analysis in Health Economics.  Statistical Methods in Medical Research
#'   doi:10.1177/0962280211419832.
#' 
#'   Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#'   London
#' @keywords Health economic evaluation Cost Effectiveness Acceptability Curve
#' @export ceac.plot
ceac.plot <- function(he, comparison = NULL, pos = c(1, 0), graph = c("base", "ggplot2", "plotly"), ...) {
  options(scipen = 10)
  
  alt.legend <- pos
  # choose graphical engine
  if (is.null(graph) || is.na(graph)) graph = "base"
  graph_choice <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  # check feasibility
  if (graph_choice == 2 && !requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)) {
    warning("Package ggplot2 and grid not found; eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  if (graph_choice == 3 && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package plotly not found; eib.plot will be rendered using base graphics.")
    graph_choice <- 1
  }
  
  # evaluate additional arguments -----
  exArgs <- list(...)
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
    # base graphics version -----
    if (is.numeric(alt.legend) & length(alt.legend) == 2) {
      temp <- ""
      if (alt.legend[2] == 1)
        temp <- paste0(temp, "top")
      else
        temp <- paste0(temp, "bottom")
      if (alt.legend[1] == 0)
        temp <- paste0(temp, "left")
      else
        temp <- paste0(temp, "right")
      alt.legend <- temp
      if (length(grep("^(bottom|top)(left|right)$", temp)) == 0)
        alt.legend <- FALSE
    }
    if (is.logical(alt.legend)) {
      if (!alt.legend)
        alt.legend = "bottomright"
      else
        alt.legend = "bottomleft"
    }
    
    if (he$n.comparisons == 1) {
      plot(
        he$k, he$ceac, t = "l",
        xlab = plot_annotations$xlab, ylab = plot_annotations$ylab,
        ylim = c(0, 1), main = plot_annotations$title,
        lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types[1]),
        col = plot_aes$line$colors[1])
    }
    if (he$n.comparisons > 1 & is.null(comparison)) {
      lwd = ifelse(he$n.comparisons <= 6, 1, 1.5)
      # linetype is the indicator
      if (is.null(plot_aes$line$types))
        plot_aes$line$types = rep_len(1:6, he$n.comparisons)
      # adjust provided aes lengths
      if (length(plot_aes$line$types) < he$n.comparisons)
        plot_aes$line$types <- rep_len(plot_aes$line$types, he$n.comparisons)
      if (!exists("line_colors", where = exArgs)) {
        plot_aes$line$colors <- 
          if (he$n.comparisons <= 6) rep(1,he$n.comparisons) else
            colors()[floor(seq(262, 340, length.out = he$n.comparisons))]	# gray scale
      } else {
        if (length(plot_aes$line$colors) < he$n.comparisons)
          plot_aes$line$colors <- rep_len(plot_aes$line$colors, he$n.comparisons)
      }
      plot(
        he$k, he$ceac[,1], t = "l", 
        main = plot_annotations$title,
        xlab = plot_annotations$xlab, ylab = plot_annotations$ylab,
        ylim = c(0, 1), lwd = lwd,
        lty = plot_aes$line$types[1], col = plot_aes$line$colors[1])
      for (j in 2:he$n.comparisons)
        points(he$k, he$ceac[,j], t = "l", lwd = lwd,
               col = plot_aes$line$colors[j], lty = plot_aes$line$types[j])
      text <- paste(he$interventions[he$ref]," vs ",he$interventions[he$comp])
      legend(
        alt.legend, text, cex = .7, bty = "n", 
        lty = plot_aes$line$types, col = plot_aes$line$colors)
    }
    if (he$n.comparisons > 1 & !is.null(comparison)) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[, comparison]
      he$delta.c <- he$delta.c[, comparison]
      he$n.comparators <- length(comparison) + 1
      he$n.comparisons <- length(comparison)
      he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
      he$ICER <- he$ICER[comparison]
      he$ib <- he$ib[, , comparison]
      he$eib <- he$eib[, comparison]
      he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
      he$ceac <- he$ceac[, comparison]
      he$ref <- rank(c(he$ref, he$comp))[1]
      he$comp <- rank(c(he$ref, he$comp))[-1]
      he$mod <- TRUE #
      
      ceac.plot(he, pos = alt.legend, graph = "base", ...)
    }
  } else if (graph_choice == 2) {
    # ggplot2 version -----
    if (!isTRUE(
      requireNamespace("ggplot2", quietly = TRUE) &
      requireNamespace("grid", quietly = TRUE)
    )) {
      message("falling back to base graphics\n")
      ceac.plot(he, pos = alt.legend, graph = "base", ...)
      return(invisible(NULL))
    }
    
    if (he$n.comparisons > 1 & is.null(comparison) == FALSE) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[, comparison]
      he$delta.c <- he$delta.c[, comparison]
      he$n.comparators <- length(comparison) + 1
      he$n.comparisons <- length(comparison)
      he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
      he$ICER <- he$ICER[comparison]
      he$ib <- he$ib[, , comparison]
      he$eib <- he$eib[, comparison]
      he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
      he$ceac <- he$ceac[, comparison]
      he$ref <- rank(c(he$ref, he$comp))[1]
      he$comp <- rank(c(he$ref, he$comp))[-1]
      he$mod <- TRUE #
      return(ceac.plot(he, pos = alt.legend, graph = "ggplot2", ...))
    }
    # no visible binding note
    k = NA_real_
    if (he$n.comparisons == 1) {
      data.psa <- data.frame("k" = he$k, "ceac" = he$ceac)
      if (is.null(plot_aes$line$types)) 
        plot_aes$line$types <- 1
      ceac <- ggplot2::ggplot(data.psa, ggplot2::aes(k,ceac)) + 
        ggplot2::geom_line(
          linetype = plot_aes$line$types[1], 
          colour = plot_aes$line$colors[1])
    }
    if (he$n.comparisons > 1 & is.null(comparison) == TRUE) {
      data.psa <- with(
        he, data.frame(
          "k" = c(k), "ceac" = c(ceac),
          "comparison" = as.factor(c(
            sapply(1:he$n.comparisons, function(x) rep(x, length(he$k)))
          ))))
      # labels for legend
      comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
      # linetype is the indicator
      if (is.null(plot_aes$line$types))
        plot_aes$line$types = rep_len(1:6, he$n.comparisons)
      # adjust provided aes lengths
      if (length(plot_aes$line$types) < length(comparisons.label))
        plot_aes$line$types <- rep_len(plot_aes$line$types, length(comparisons.label))
      if (length(plot_aes$line$colors) < length(comparisons.label))
        plot_aes$line$colors <- rep_len(plot_aes$line$colors, length(comparisons.label))
      ceac <- ggplot2::ggplot(
        data.psa,
        ggplot2::aes(k, ceac, linetype = comparison, colour = comparison)) +
        ggplot2::geom_line() +
        ggplot2::scale_linetype_manual(
          "", labels = comparisons.label, values = plot_aes$line$types) +
        ggplot2::scale_colour_manual(
          "", labels = comparisons.label, values = plot_aes$line$colors)
    }
    ceac <- ceac + ggplot2::theme_bw() + 
      ggplot2::scale_y_continuous(limits = c(0,1)) +
      ggplot2::labs(
        title = plot_annotations$title,
        x = plot_annotations$xlab, y = plot_annotations$ylab) 
    jus <- NULL
    if (isTRUE(alt.legend)) {
      alt.legend = "bottom"
      ceac <- ceac + ggplot2::theme(legend.direction = "vertical")
    }
    else{
      if (is.character(alt.legend)) {
        choices <- c("left", "right", "bottom", "top")
        alt.legend <- choices[pmatch(alt.legend, choices)]
        jus = "center"
        if (is.na(alt.legend)) alt.legend = FALSE
      }
      if (length(alt.legend) > 1) jus <- alt.legend
      if (length(alt.legend) == 1 & !is.character(alt.legend)) {
        alt.legend <- c(1, 0); jus <- alt.legend
      }
    }
    # opt theme retrieval, if any
    opt.theme <- ggplot2::theme()
    for (obj in exArgs)
      if (ggplot2::is.theme(obj))
        opt.theme <- opt.theme + obj
    # theme refinement
    ceac <- ceac +
      ggplot2::theme(
        legend.position = alt.legend,
        legend.justification = jus,
        legend.title = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        text = ggplot2::element_text(size = 11),
        legend.key.size = grid::unit(.66, "lines"),
        legend.spacing = grid::unit(-1.25, "line"),
        panel.grid = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text.align = 0,
        plot.title = ggplot2::element_text(
          lineheight = 1.05,
          face = "bold",
          size = 14.3,
          hjust = 0.5
        )) + 
      opt.theme
    return(ceac)
  } else if (graph_choice == 3) {
    # plotly version -----
    if (he$n.comparisons > 1 & is.null(comparison) == FALSE) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[, comparison]
      he$delta.c <- he$delta.c[, comparison]
      he$n.comparators <- length(comparison) + 1
      he$n.comparisons <- length(comparison)
      he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
      he$ICER <- he$ICER[comparison]
      he$ib <- he$ib[, , comparison]
      he$eib <- he$eib[, comparison]
      he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
      he$ceac <- he$ceac[, comparison]
      he$ref <- rank(c(he$ref, he$comp))[1]
      he$comp <- rank(c(he$ref, he$comp))[-1]
      he$mod <- TRUE #
      return(ceac.plot(he, pos = alt.legend, graph = "plotly", ...))
    }
    # plot labels
    comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
    # data frame
    data.psa <- data.frame(
      "k" = c(he$k), "ceac" = c(he$ceac),
      "comparison" = as.factor(c(
        sapply(1:he$n.comparisons, function(x) rep(x, length(he$k)))
      )),
      "label" = as.factor(c(
        sapply(comparisons.label, function(x) rep(x, length(he$k)))
      )))
    # aes management
    if (is.null(plot_aes$line$types))
      plot_aes$line$types = rep_len(1:6, he$n.comparisons)
    # opacities
    if (!is.null(plot_aes$area$color))
      plot_aes$area$color <- sapply(plot_aes$area$color, function(x)
        ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
    # adjust provided aes lengths
    if (length(plot_aes$line$types) < length(comparisons.label))
      plot_aes$line$types <- rep_len(plot_aes$line$types, length(comparisons.label))
    if (length(plot_aes$line$colors) < length(comparisons.label))
      plot_aes$line$colors <- rep_len(plot_aes$line$colors, length(comparisons.label))
    
    ceac <- plotly::plot_ly(data.psa, x = ~k)
    ceac <- plotly::add_trace(
      ceac,
      y = ~ceac, type = "scatter", mode = "lines",
      fill = ifelse(plot_aes$area$include, "tozeroy", "none"),
      name = ~label,
      fillcolor = plot_aes$area$color,
      color = ~comparison,
      colors = plot_aes$line$colors,
      linetype = ~comparison,
      linetypes = plot_aes$line$types)
    
    # legend positioning not great - must be customized case by case
    legend_list = list(orientation = "h", xanchor = "center", x = 0.5)
    if (is.character(alt.legend))
      legend_list = switch(
        alt.legend,
        "left" = list(orientation = "v", x = 0, y = 0.5),
        "right" = list(orientation = "v", x = 0, y = 0.5),
        "bottom" = list(orienation = "h", x = .5, y = 0, xanchor = "center"),
        "top" = list(orientation = "h", x = .5, y = 100, xanchor = "center"))
    
    ceac <- plotly::layout(
      ceac,
      title = plot_annotations$title,
      xaxis = list(
        hoverformat = ".2f",
        title = plot_annotations$xlab),
      yaxis = list(
        title = plot_annotations$ylab,
        range = c(0,1.005)),
      showlegend = TRUE, 
      legend = legend_list)
    ceac <- plotly::config(ceac, displayModeBar = FALSE)
    return(ceac)
  }
}
