
#' Cost-Effectiveness Acceptability Curve (CEAC) Plot By Graph Device
#'
#' Choice of base R, ggplot2 or plotly.
#' @name ceac_plot_graph
#' 
#' @template args-he
#' @param pos_legend Legend position
#' @param graph_params Aesthetic ggplot parameters
#' @param ... Additional arguments
NULL


#' @rdname ceac_plot_graph
#' @keywords hplot
#' 
ceac_plot_base <- function(he,
                           pos_legend,
                           graph_params,
                           ...)
  UseMethod("ceac_plot_base", he)


#' @rdname ceac_plot_graph
#' @keywords hplot
#' 
ceac_plot_base.pairwise <- function(he,
                                    pos_legend,
                                    graph_params,
                                    ...) {
  ceac_matplot(he,
               pos_legend,
               graph_params,
               "p_best_interv")
}

#' @rdname ceac_plot_graph
#' @keywords hplot
#'
ceac_plot_base.bcea <- function(he,
                                pos_legend,
                                graph_params,
                                ...) {
  ceac_matplot(he,
               pos_legend,
               graph_params,
               "ceac")
}


#' CEAC Matrix Plot
#' 
#' CEAC plot using `matplot` in Base R.
#' 
#' @inheritParams ceac_plot_graph
#' @param ceac `ceac` index in `he`
#' @keywords hplot
#' @importFrom graphics matplot legend
#' 
ceac_matplot <- function(he,
                         pos_legend,
                         graph_params,
                         ceac) {
  
  base_params <- helper_base_params(he, graph_params)
  
  legend_params <- ceac_legend_base(he, pos_legend, base_params)
  
  do.call("matplot", c(list(x = he$k,
                            y = he[[ceac]]),
                       base_params), quote = TRUE)
  
  do.call(legend, legend_params)
}


#' @rdname ceac_plot_graph
#' 
#' @keywords hplot
#' 
ceac_plot_ggplot <- function(he,
                             pos_legend,
                             graph_params, ...)
  UseMethod("ceac_plot_ggplot", he)


#' @rdname ceac_plot_graph
#' @keywords hplot
#' 
ceac_plot_ggplot.pairwise <- function(he,
                                      pos_legend,
                                      graph_params, ...) {
  ceac_ggplot(he,
              pos_legend,
              graph_params,
              "p_best_interv", ...)
}

#' @rdname ceac_plot_graph
#' @keywords hplot
#' 
ceac_plot_ggplot.bcea <- function(he,
                                  pos_legend,
                                  graph_params, ...) {
  ceac_ggplot(he,
              pos_legend,
              graph_params,
              "ceac", ...)
}

#' @rdname ceac_plot_graph
#' @param ceac ceac index in he
#' @keywords hplot
#' 
ceac_ggplot <- function(he,
                        pos_legend,
                        graph_params,
                        ceac, ...) {
  
  extra_params <- list(...)
  
  ceac_dat <- he[[ceac]]
  n_lines <- ncol(ceac_dat)
  
  data_psa <-
    tibble(k = rep(he$k,
                   times = n_lines),
           ceac = c(ceac_dat),
           comparison = as.factor(rep(1:n_lines,
                                      each = length(he$k))))
  
  graph_params <- helper_ggplot_params(he, graph_params)
  legend_params <- make_legend_ggplot(he, pos_legend)
  theme_add <- purrr::keep(extra_params, is.theme)
  
  ggplot(data_psa, aes(.data$k, .data$ceac)) +
    geom_line(aes(linetype = .data$comparison,
                  size = .data$comparison,
                  colour = factor(.data$comparison))) +
    theme_ceac() + 
    theme_add +                                            # theme
    scale_y_continuous(limits = c(0, 1)) +
    do.call(labs, graph_params$annot) +                    # text
    do.call(theme, legend_params) +                        # legend
    scale_linetype_manual("",                              # lines
                          labels = graph_params$labels,
                          values = graph_params$line$type) +
    scale_color_manual("",
                       labels = graph_params$labels,
                       values = graph_params$line$color) +
    scale_size_manual("",
                      labels = graph_params$labels,
                      values = graph_params$line$size)
}


#' @rdname ceac_plot_graph
#' 
ceac_plot_plotly <- function(he,
                             pos_legend = "left",
                             graph_params) {
  
  comparisons_label <-
    paste0(he$interventions[he$ref]," vs ",he$interventions[he$comp])
  
  data.psa <- data.frame(
    k = c(he$k),
    ceac = c(he$ceac),
    comparison = as.factor(c(
      sapply(1:he$n_comparisons, function(x) rep(x, length(he$k)))
    )),
    label = as.factor(c(
      sapply(comparisons_label, function(x) rep(x, length(he$k)))
    )))
  
  if (is.null(graph_params$line$types))
    graph_params$line$type <- rep_len(1:6, he$n_comparisons)
  
  # opacities
  if (!is.null(graph_params$area$color))
    graph_params$area$color <-
    sapply(graph_params$area$color, function(x)
      ifelse(grepl(pattern = "^rgba\\(", x = x),
             yes = x,
             no = plotly::toRGB(x, 0.4)))
  
  ceac <- plotly::plot_ly(data.psa, x = ~k)
  ceac <-
    plotly::add_trace(
      ceac,
      y = ~ceac,
      type = "scatter",
      mode = "lines",
      fill = ifelse(graph_params$area$include, "tozeroy", "none"),
      name = ~label,
      fillcolor = graph_params$area$color,
      color = ~comparison,
      colors = graph_params$line$color,
      linetype = ~comparison,
      linetypes = graph_params$line$type)
  
  legend_params <- make_legend_plotly(pos_legend)
  
  ceac <-
    plotly::layout(
      ceac,
      title = graph_params$annot$title,
      xaxis = list(
        hoverformat = ".2f",
        title = graph_params$annot$x),
      yaxis = list(
        title = graph_params$annot$y,
        range = c(0, 1.005)),
      showlegend = he$n_comparisons > 1, 
      legend = legend_params)
  
  plotly::config(ceac, displayModeBar = FALSE)
}

