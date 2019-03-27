# evi.plot -----

#' Expected Value of Information (EVI) plot
#' 
#' Plots the Expected Value of Information (EVI) against the willingness to pay
#' 
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the three options \code{"base"},
#' \code{"ggplot2"} or \code{"plotly"}. Default value is \code{"base"}.
#' @param ... Additional graphical arguments:
#'   \itemize{
#'     \item \code{line_colors} to specify the EVPI line colour - all graph types.
#'     \item \code{line_types} to specify the line type (lty) - all graph types.
#'     \item \code{area_include} to specify whether to include the area under the EVPI curve - plotly only.
#'     \item \code{area_color} to specify the area under the colour curve - plotly only.}
#' @return \item{eib}{ If \code{graph="ggplot2"} a ggplot object, or if \code{graph="plotly"} 
#'   a plotly object containing the requested plot. Nothing is returned when \code{graph="base"}, 
#'   the default.} The function produces a plot of the
#'   Expected Value of Information as a function of the discrete grid
#'   approximation of the willingness to pay parameter. The break even point(s)
#'   (i.e. the point in which the EIB=0, ie when the optimal decision changes
#'   from one intervention to another) is(are) also showed.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{ceac.plot}},
#' \code{\link{ceplane.plot}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Expected value of information
#' @export evi.plot
evi.plot <- function(he, graph = c("base","ggplot2","plotly"), ...) {
  options(scipen = 10)
  # choose graphical engine -----
  if (is.null(graph) || is.na(graph)) 
    graph = "base"
  graph_choice <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  if (graph_choice == 2 &&
      !requireNamespace("ggplot2", quietly = TRUE) &
      requireNamespace("grid", quietly = TRUE)) {
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
  plot_aes <- list("area" = list("include" = TRUE, "color" = "grey50"),
                   "line" = list("colors" = "black", "types" = NULL))
  plot_aes_args = c("area_include", "area_color", "line_colors", "line_types")
  cri.quantile <- TRUE
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
    plot_annotations$title = "Expected Value of Information"
  if (!plot_annotations$exist$xlab)
    plot_annotations$xlab = "Willingness to pay"
  if (!plot_annotations$exist$ylab)
    plot_annotations$ylab = "EVPI"
  # dataset
  data.psa <- with(he,data.frame("k" = c(k), "evi" = c(evi)))
  if (graph_choice == 1) {
    # base graphics version -----
    plot(
      data.psa$k, data.psa$evi, t = "l",
      xlab = plot_annotations$xlab,
      ylab = plot_annotations$ylab,
      main = plot_annotations$title,
      col = plot_aes$line$colors,
      lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$type)
    )
    if (length(he$kstar) == 1) {
      points(
        rep(he$kstar, 3), c(-10000, he$evi[he$k == he$kstar] / 2, he$evi[he$k == he$kstar]),
        t = "l", lty = 2, col = "dark grey")
      points(c(-10000, he$kstar / 2, he$kstar), rep(he$evi[he$k == he$kstar], 3),
        t = "l", lty = 2, col = "dark grey")
    }
    if (length(he$kstar) > 1) {
      for (i in 1:length(he$kstar)) {
        points(
          rep(he$kstar[i], 3), c(-10000, he$evi[he$k == he$kstar[i]] / 2, he$evi[he$k == he$kstar[i]]),
          t = "l", lty = 2, col = "dark grey")
        points(
          c(-10000, he$kstar[i] / 2, he$kstar[i]), rep(he$evi[he$k == he$kstar[i]], 3),
          t = "l", lty = 2, col = "dark grey")
      }
    }
  } else if (graph_choice == 2) {
    # ggplot2 version -----
    if (!isTRUE(
      requireNamespace("ggplot2", quietly = TRUE) &
      requireNamespace("grid", quietly = TRUE))) {
      message("falling back to base graphics\n")
      evi.plot(he, graph = "base", ...)
      return(invisible(NULL))
    }
    ### no visible binding note
    k <- NA_real_
    evi <- ggplot2::ggplot(data.psa, ggplot2::aes(k, evi)) +
      ggplot2::geom_line(
        colour = plot_aes$line$colors,
        lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$type)
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = plot_annotations$title,
                    x = plot_annotations$xlab,
                    y = plot_annotations$ylab)
    if (length(he$kstar) != 0) {
      kstars = length(he$kstar)
      evi.at.kstar <- numeric(kstars)
      for (i in 1:kstars) {
        evi.at.kstar[i] <- with(he, evi[which.min(abs(k - kstar[i]))])
      }
      for (i in 1:kstars) {
        evi <- evi + 
          ggplot2::annotate(
            "segment",
            x = he$kstar[i],
            xend = he$kstar[i],
            y = evi.at.kstar[i],
            yend = -Inf,
            linetype = 2,
            colour = "grey50") +
          ggplot2::annotate(
            "segment",
            x = he$kstar[i],
            xend = -Inf,
            y = evi.at.kstar[i],
            yend = evi.at.kstar[i],
            linetype = 2,
            colour = "grey50")
      }
    }
    evi <- evi +
      ggplot2::theme(
        text = ggplot2::element_text(size = 11),
        legend.key.size = grid::unit(.66, "lines"),
        legend.spacing = grid::unit(-1.25, "line"),
        panel.grid = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          lineheight = 1.05,
          face = "bold",
          size = 14.3,
          hjust = 0.5)
      )
    return(evi)
  } else if (graph_choice == 3) {
    # plotly version -----
    # opacities
    plot_aes$area$color = sapply(plot_aes$area$color, function(x)
      ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
    # legend
    legend_list = list(orientation = "h", xanchor = "center", x = .5)
    # actual plot
    evi <- plotly::plot_ly(data.psa, x = ~k)
    evi <- plotly::add_trace(
      evi,
      y = ~evi,
      type = "scatter", mode = "lines",
      name = "EVPI",
      fill = ifelse(plot_aes$area$include, "tozeroy", "none"),
      fillcolor = plot_aes$area$color,
      line = list(
        color = plot_aes$line$colors[1],
        dash = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot")[
          ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types)]))
    evi <- plotly::layout(
      evi,
      title = plot_annotations$title,
      xaxis = list(
        hoverformat = ".2f",
        title = plot_annotations$xlab),
      yaxis = list(
        hoverformat = ".2f",
        title = plot_annotations$ylab),
      # legend hidden by default (single series)
      showlegend = FALSE,
      legend = legend_list)
    evi <- plotly::config(evi, displayModeBar = FALSE)
    return(evi)
  }
}
