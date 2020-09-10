
#' @noRd
#' 
#' @import plotly
#' @export
#' 
ceac_plot_plotly <- function(he,
                             pos_legend = pos,
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
    graph_params$line$types <- rep_len(1:6, he$n_comparisons)
  
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
      colors = graph_params$line$colors,
      linetype = ~comparison,
      linetypes = graph_params$line$types)
  
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

