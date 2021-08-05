
#' EIB plot plotly version
#' 
#' @importFrom plotly plot_ly add_trace add_ribbons layout config
#' 
eib_plot_plotly <- function(he,
                            alt.legend,
                            plot_aes,
                            plot_annotations,
                            plot.cri,
                            cri.quantile,
                            comparison,
                            alpha,
                            cri,
                            size = NULL) {
  
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
  
  if (is.null(plot_aes$line$types))
    plot_aes$line$types <- rep(c(1:6), ceiling(he$n_comparisons/6))[1:he$n_comparisons]
  comparisons.label <- with(he,paste0(interventions[ref], " vs ", interventions[comp]))
  
  if (length(plot_aes$line$types) < length(comparisons.label))
    plot_aes$line$types <- rep_len(plot_aes$line$types, length(comparisons.label))
  
  if (length(plot_aes$line$colors) < length(comparisons.label))
    plot_aes$line$colors <- rep_len(plot_aes$line$colors, length(comparisons.label))
  
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
  # NB: decision change points not included - hover functionality is sufficient
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
  
  # legend positioning not great - must be customized case by case
  legend_list <- list(orientation = "h", xanchor = "center", x = 0.5)
  
  if (is.character(alt.legend))
    legend_list = switch(
      alt.legend,
      "left" = list(orientation = "v", x = 0, y = 0.5),
      "right" = list(orientation = "v", x = 0, y = 0.5),
      "bottom" = list(orienation = "h", x = 0.5, y = 0, xanchor = "center"),
      "top" = list(orientation = "h", x = 0.5, y = 100, xanchor = "center"))
  
  eib <-
    plotly::layout(
      eib,
      title = switch(
        as.numeric(plot_annotations$exist$title) + 1, 
        paste0("Expected Incremental Benefit", ifelse(
          plot.cri,
          paste0("\nand ", format((1 - alpha)*100, digits = 4), "% credible intervals"),
          "")),
        plot_annotations$title),
      xaxis = list(
        hoverformat = ".2f",
        title = switch(
          as.numeric(plot_annotations$exist$xlab) + 1,
          "Willingness to pay",
          plot_annotations$xlab)),
      yaxis = list(
        hoverformat = ".2f",
        title = switch(
          as.numeric(plot_annotations$exist$xlab) + 1,
          "EIB",
          plot_annotations$ylab)),
      showlegend = TRUE, 
      legend = legend_list)
  
  plotly::config(eib, displayModeBar = FALSE)
}

