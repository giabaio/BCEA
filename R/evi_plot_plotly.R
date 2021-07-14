
#' evi_plot_plotly
#' 
#' @import plotly
#' 
evi_plot_plotly <- function(data.psa,
                            plot_aes,
                            plot_annotations) {
  
  plot_aes$area$color = sapply(plot_aes$area$color, function(x)
    ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
  
  # legend
  legend_list = list(orientation = "h",
                     xanchor = "center",
                     x = 0.5)
  # actual plot
  evi <- plotly::plot_ly(data.psa, x = ~k)
  
  evi <- plotly::add_trace(
    evi,
    y = ~evi,
    type = "scatter",
    mode = "lines",
    name = "EVPI",
    fill = ifelse(plot_aes$area$include, "tozeroy", "none"),
    fillcolor = plot_aes$area$color,
    line = list(
      color = plot_aes$line$colors[1],
      dash = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot")[
        ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types)]))
  
  evi <-
    plotly::layout(
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
  
  plotly::config(evi, displayModeBar = FALSE)
}

