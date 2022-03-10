
#' Expected Incremental Benefit Plot By Graph Device
#'
#' Choice of base R, ggplot2 or plotly.
#' @name eib_plot_graph
#' 
NULL


#' EIB plot base R version
#' @rdname eib_plot_graph
#' 
#' @template args-he
#' @param graph_params List of graph parameters
#' @param ... Additional arguments
#' 
eib_plot_base <- function(he,
                          graph_params,
                          ...) {
  
  cri_params <- eib_params_cri(he, graph_params)
  plot_params <- eib_params_base(he, graph_params, cri_params)
  legend_params <- eib_legend_base(he, graph_params)
  
  do.call(matplot, c(list(x = he$k,
                          y = he$eib),
                     plot_params), quote = TRUE)
  abline(h = 0, col = "grey")      # x-axis
  plot_eib_cri(he, cri_params)     # credible intervals
  kstar_vlines(he, plot_params)
  do.call(legend, legend_params)
}



#' EIB plot ggplot2 version
#' @rdname eib_plot_graph
#' 
#' @template args-he
#' @param graph_params Graph parameters
#' @param ... Additional parameters
#' 
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom purrr keep
#' 
eib_plot_ggplot <- function(he,
                            graph_params,
                            ...) {
  extra_params <- list(...)
  
  ##TODO: can we move this up a level?
  cri_params <- eib_params_cri(he, graph_params)
  
  theme_add <- purrr::keep(extra_params, is.theme)
  legend_params <- make_legend_ggplot(he, graph_params$pos)
  graph_params <- eib_params_ggplot(he, graph_params, cri_params)
  
  data_psa <-
    data.frame(
      k = c(he$k),
      eib = c(he$eib), 
      comparison =
        as.factor(rep(1:he$n_comparison,
                      each = length(he$k))))
  
  ggplot(data_psa,
         aes(x = .data$k, y = .data$eib,
             group = .data$comparison)) + 
    geom_line(aes(colour = .data$comparison,
                  linetype = .data$comparison)) +
    theme_eib() +
    theme_add +
    do.call(theme, legend_params) +
    do.call(labs,
            list(title = graph_params$main,
                 x = graph_params$xlab,
                 y = graph_params$ylab)) +
    geom_hline(aes(yintercept = 0),
               colour = "grey",
               linetype = 1) + 
    geom_cri(graph_params$plot.cri, cri_params) +
    do.call(annotate, graph_params$kstar) +
    geom_vline(
      aes(xintercept = .data$kstar),
      data = data.frame("kstar" = he$kstar),
      colour = "grey50",
      linetype = 2,
      size = 0.5) +
    scale_linetype_manual(
      "",
      labels = graph_params$plot$labels,
      values = graph_params$plot$line$types) +
    scale_colour_manual(
      "",
      labels = graph_params$plot$labels,
      values = graph_params$plot$line$colors)
}



#' EIB plot plotly version
#' @rdname eib_plot_graph
#' 
#' @template args-he
#' @param graph_params Graph parameters
#' @param ... Additional parameters
#' 
eib_plot_plotly <- function(he,
                            graph_params, ...) {
  
  cri_params <- eib_params_cri(he, graph_params)
  
  alt.legend <- graph_params$alt.legend
  plot_aes <- graph_params$plot_aes
  plot_annotations <- graph_params$plot_annotations
  plot.cri <- graph_params$plot.cri
  cri.quantile <- graph_params$cri.quantile
  comparison <- graph_params$comparison
  alpha <- graph_params$alpha_cri
  cri <- graph_params$cri
  size <- graph_params$size
  main <- graph_params$main
  xlab <- graph_params$xlab
  ylab <- graph_params$ylab
  low <- cri_params$data$low
  upp <- cri_params$data$upp
  
  
  if (!is.null(size) && !is.na(size)) {
    message("Option size will be ignored using plotly.")
    size <- NULL
  }
  
  if (he$n_comparisons > 1 & !is.null(comparison)) {
    # adjusts bcea object for the correct number of dimensions and comparators
    he$comp <- he$comp[comparison]
    he$delta_e <- he$delta_e[, comparison]
    he$delta_c <- he$delta_c[, comparison]
    he$n_comparators <- length(comparison) + 1
    he$n_comparisons <- length(comparison)
    he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
    he$ICER <- he$ICER[comparison]
    he$ib <- he$ib[, , comparison]
    he$eib <- he$eib[, comparison]
    he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
    he$ceac <- he$ceac[, comparison]
    he$ref <- rank(c(he$ref, he$comp))[1]
    he$comp <- rank(c(he$ref, he$comp))[-1]
    he$change_comp <- TRUE
    
    return(
      eib.plot(
        he,
        pos = alt.legend,
        graph = "plotly",
        size = size,
        comparison = NULL,
        plot.cri = plot.cri,
        alpha = alpha,
        cri.quantile = cri.quantile,
        ...))
  }
  
  n_comp <- length(comparison)
  
  if (is.null(plot_aes$line$types))
    plot_aes$line$types <- rep(c(1:6), ceiling(he$n_comparisons/6))[1:he$n_comparisons]
  
  comparisons.label <-
    paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp])
  
  if (length(plot_aes$line$types) < n_comp)
    plot_aes$line$types <- rep_len(plot_aes$line$types, n_comp)
  
  if (length(plot_aes$line$colors) < n_comp)
    plot_aes$line$colors <- rep_len(plot_aes$line$colors, n_comp)
  
  # opacities
  plot_aes$line$cri_colors <-
    sapply(plot_aes$line$cri_colors,
           function(x) 
             ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
  
  plot_aes$area$color <-
    sapply(plot_aes$area$color,
           function(x)
             ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
  
  data.psa <-
    data.frame(
      k = he$k,
      eib = c(he$eib),
      comparison = as.factor(c(
        sapply(1:he$n_comparisons, function(x) rep(x, length(he$k)))
      )),
      label = as.factor(c(
        sapply(comparisons.label, function(x) rep(x, length(he$k)))
      )))
  
  if (plot.cri)
    data.psa <- cbind(data.psa, cri)
  
  eib <- plotly::plot_ly(data.psa, x = ~k)
  eib <-
    plotly::add_trace(
      eib,
      y = ~eib,
      type = "scatter",
      mode = "lines",
      fill = ifelse(plot_aes$area$include, "tozeroy", "none"),
      name = ~label,
      fillcolor = plot_aes$area$color,
      color = ~comparison,
      colors = plot_aes$line$colors,
      linetype = ~comparison,
      linetypes = plot_aes$line$types,
      legendgroup = ~comparison)
  
  # decision change points not included
  # hover functionality is sufficient
  if (plot.cri) {
    if (he$n_comparisons == 1) {
      eib <- plotly::add_ribbons(
        eib,
        name = paste0(100 * (1 - alpha), "% CrI"),
        ymin = ~low,
        ymax = ~upp,
        color = NA,
        fillcolor = ~plot_aes$line$cri_colors[comparison])
    } else {
      eib <- plotly::add_ribbons(
        eib,
        name = ~label,
        ymin = ~low,
        ymax = ~upp,
        line = list(color = plot_aes$line$cri_colors[1]),
        # for transparency, use plotly::toRGB("blue", alpha = 0.5)
        legendgroup = ~comparison,
        fillcolor = "rgba(1, 1, 1, 0)",
        linetype = ~comparison,
        linetypes = plot_aes$line$types,
        showlegend = FALSE)
    }
  }
  
  # legend positioning not great
  # must be customized case by case
  legend_list <- list(orientation = "h", xanchor = "center", x = 0.5)
  
  if (is.character(alt.legend))
    legend_list <- switch(
      alt.legend,
      "left" = list(orientation = "v", x = 0, y = 0.5),
      "right" = list(orientation = "v", x = 0, y = 0.5),
      "bottom" = list(orienation = "h", x = 0.5, y = 0, xanchor = "center"),
      "top" = list(orientation = "h", x = 0.5, y = 100, xanchor = "center"))
  
  xaxis <- 
    list(
      hoverformat = ".2f",
      title = xlab)
  
  yaxis <- 
    list(
      hoverformat = ".2f",
      title = ylab)
  
  eib <-
    plotly::layout(
      eib,
      title = main,
      xaxis = xaxis,
      yaxis = yaxis,
      showlegend = TRUE, 
      legend = legend_list)
  
  plotly::config(eib, displayModeBar = FALSE)
}

