
#' @noRd
#' 
.ceac_plot_plotly <- function() {
  
  if (he$n_comparisons > 1 & is.null(comparison) == FALSE) {
    # adjusts bcea object for the correct number of dimensions and comparators
    he$comp <- he$comp[comparison]
    he$delta.e <- he$delta.e[, comparison]
    he$delta.c <- he$delta.c[, comparison]
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
    he$mod <- TRUE #
    return(ceac.plot(he, pos = alt.legend, graph = "plotly", ...))
  }
  # plot labels
  comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
  # data frame
  data.psa <- data.frame(
    "k" = c(he$k), "ceac" = c(he$ceac),
    "comparison" = as.factor(c(
      sapply(1:he$n_comparisons, function(x) rep(x, length(he$k)))
    )),
    "label" = as.factor(c(
      sapply(comparisons.label, function(x) rep(x, length(he$k)))
    )))
  # aes management
  if (is.null(plot_aes$line$types))
    plot_aes$line$types = rep_len(1:6, he$n_comparisons)
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
  
  plotly::config(ceac, displayModeBar = FALSE)
}