
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
#' @importFrom scales label_dollar
#' 
eib_plot_ggplot <- function(he,
                            graph_params,
                            ...) {
  extra_params <- list(...)
  
  ##TODO: can we move this up a level?
  cri_params <- eib_params_cri(he, graph_params)
  
  theme_add <- Filter(f = \(val) ggplot2::is_theme(val), x = extra_params)
  
  legend_params <- make_legend_ggplot(he, graph_params$pos)
  graph_params <- eib_params_ggplot(he, graph_params, cri_params)
  
  data_psa <-
    data.frame(
      k = c(he$k),
      eib = c(he$eib), 
      comparison =
        as.factor(rep(1:he$n_comparisons,
                      each = length(he$k))))
  
  ggplot(data_psa,
         aes(x = .data$k, y = .data$eib,
             group = .data$comparison)) + 
    geom_line(aes(color = .data$comparison,
                  linetype = .data$comparison)) +
    theme_eib() +
    theme_add +
    do.call(theme, legend_params) +
    do.call(theme, list(
      axis.text = element_text(size = graph_params$text$size),
      axis.title.x = element_text(size = graph_params$text$size),
      axis.title.y = element_text(size = graph_params$text$size))) +
    do.call(labs,
            list(title = graph_params$main,
                 x = graph_params$xlab,
                 y = graph_params$ylab)) +
    geom_hline(aes(yintercept = 0),
               color = "grey",
               linetype = 1) + 
    geom_cri(graph_params$plot.cri, cri_params) +
    do.call(annotate, graph_params$kstar) +
    scale_x_continuous(
      labels = scales::label_dollar(prefix = graph_params$currency)) +
    geom_vline(
      aes(xintercept = .data$kstar),
      data = data.frame("kstar" = he$kstar),
      color = "grey50",
      linetype = 2,
      linewidth = 0.5) +
    scale_linetype_manual(
      "",
      labels = graph_params$labels,
      values = graph_params$line$type) +
    scale_color_manual(
      "",
      labels = graph_params$labels,
      values = graph_params$line$color)
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
  
  alt.legend <- graph_params$pos
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
  
  if (he$n_comparisons > 1 && !is.null(comparison)) {
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
  
  graph_params$line$type <- graph_params$line$type %||% rep(1:6, ceiling(he$n_comparisons/6))[1:he$n_comparisons]
  
  comparisons.label <-
    paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp])
  
  if (length(graph_params$line$type) < n_comp)
    graph_params$line$type <- rep_len(graph_params$line$type[1], n_comp)
  
  if (length(graph_params$line$color) < n_comp)
    graph_params$line$color <- rep_len(graph_params$line$color[1], n_comp)
  
  # opacities
  graph_params$line$cri_colors <-
    sapply(graph_params$line$cri_col,
           function(x) 
             ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
  
  graph_params$area$color <-
    sapply(graph_params$area$color,
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
      fill = ifelse(graph_params$area$include, "tozeroy", "none"),
      name = ~label,
      fillcolor = graph_params$area$color,
      color = ~comparison,
      colors = graph_params$line$color,
      linetype = ~comparison,
      linetypes = graph_params$line$type,
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
        fillcolor = ~graph_params$line$cri_colors[comparison])
    } else {
      eib <- plotly::add_ribbons(
        eib,
        name = ~label,
        ymin = ~low,
        ymax = ~upp,
        line = list(color = graph_params$line$cri_colors[1]),
        # for transparency, use plotly::toRGB("blue", alpha = 0.5)
        legendgroup = ~comparison,
        fillcolor = "rgba(1, 1, 1, 0)",
        linetype = ~comparison,
        linetypes = graph_params$line$type,
        showlegend = FALSE)
    }
  }
  
  legend_list = make_legend_plotly(alt.legend)
  
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

