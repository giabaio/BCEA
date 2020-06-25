
ceplane_plot_plotly <- function() {
  
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
    return(ceplane.plot(he, wtp = wtp, pos = alt.legend, graph = "plotly", ...))
  }
  if (exists("ICER.size", where = exArgs)) {
    ICER.size <- exArgs$ICER.size
  } else {
    ICER.size <- ifelse(he$n.comparisons == 1, 8, 0)
  }
  # plot labels
  comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
  kd <- data.frame(
    "delta.e" = c(he$delta.e), "delta.c" = c(he$delta.c),
    "comparison" = as.factor(c(
      sapply(1:he$n.comparisons, function(x) rep(x, nrow(as.matrix(he$delta.e))))
    )),
    "label" = as.factor(c(
      sapply(comparisons.label, function(x) rep(x, nrow(as.matrix(he$delta.e))))
    )))
  if (length(plot_aes$point$colors) != length(comparisons.label))
    plot_aes$point$colors <- rep_len(plot_aes$point$colors, length(comparisons.label))
  if (length(plot_aes$point$sizes) != length(comparisons.label))
    plot_aes$point$sizes <- rep_len(plot_aes$point$sizes, length(comparisons.label))
  if (length(plot_aes$ICER$colors) != length(comparisons.label))
    plot_aes$ICER$colors <- rep_len(plot_aes$ICER$colors, length(comparisons.label))
  if (length(plot_aes$ICER$sizes) != length(comparisons.label))
    plot_aes$ICER$sizes <- rep_len(plot_aes$ICER$sizes, length(comparisons.label))
  # plot limits
  range.e <- range(kd$delta.e)
  range.c <- range(kd$delta.c)
  range.e[1] <- ifelse(range.e[1] < 0, range.e[1], -range.e[1])
  range.c[1] <- ifelse(range.c[1] < 0, range.c[1], -range.c[1])
  # ce plane data
  x1 <- range.e[1] - 2*abs(diff(range.e))
  x2 <- range.e[2] + 2*abs(diff(range.e))
  x = c(x1, x2, x2)
  y = c(x1*wtp, x2*wtp, x1*wtp)
  plane <- data.frame(x = x, y = y)
  # build a trapezoidal plane instead of a triangle if
  # the y value is less than the minimum difference on costs
  if (y[1] > 1.2*range.c[1])
    plane <- rbind(plane,
                   c(x2,2*range.c[1]), #new bottom-right vertex
                   c(x1,2*range.c[1])) #new bottom-left vertex
  xrng = c(ifelse(prod(range.e) < 0,
                  range.e[1]*1.1,
                  ifelse(range.e[1] < 0,
                         range.e[1]*1.1,
                         -(range.e[2] - range.e[1])*0.1)),
           ifelse(prod(range.e) < 0, range.e[2]*1.1,
                  ifelse(range.e[2] > 0,
                         range.e[2]*1.1,
                         (range.e[2] - range.e[1])*0.1)))
  yrng = c(ifelse(prod(range.c) < 0,
                  range.c[1]*1.1,
                  ifelse(range.c[1] < 0,
                         range.c[1]*1.1,
                         -(range.c[2] - range.c[1])*0.1)),
           ifelse(prod(range.c) < 0,
                  range.c[2]*1.1,
                  ifelse(range.c[2] > 0,
                         range.c[2]*1.1,
                         (range.c[2] - range.c[1])*0.1)))
  # Calculates dataset for ICERs from bcea object
  # @param he A BCEA object
  # @param comparisons.label Optional vector of strings with comparison labels
  # @return A data.frame object including mean outcomes, comparison identifier,
  #   comparison label and associated ICER
  tabulate_means = function(he, comparisons.label = NULL) {
    if (is.null(comparisons.label))
      comparisons.label <- 1:he$n.comparisons
    data.frame(
      "lambda.e" = sapply(1:he$n.comparisons, function(x) mean(as.matrix(he$delta.e)[,x])),
      "lambda.c" = sapply(1:he$n.comparisons, function(x) mean(as.matrix(he$delta.c)[,x])),
      "comparison" = as.factor(1:he$n.comparisons),
      "label" = comparisons.label,
      "ICER" = he$ICER
    )
  }
  # actual plot
  ceplane <- plotly::plot_ly()
  # CEA area
  if (plot_aes$area$include)
    ceplane <- plotly::add_trace(
      ceplane,
      type = "scatter", mode = "lines",
      data = plane,
      x = ~x, y = ~y,
      fill = "tonext",
      fillcolor = ifelse(
        grepl(pattern = "^rgba\\(", x = plot_aes$area$color),
        plot_aes$area$color,
        plotly::toRGB(plot_aes$area$color, 0.5)),
      line = list(color = ifelse(
        grepl(pattern = "^rgba\\(", x = plot_aes$area$line_color),
        plot_aes$area$line_color,
        plotly::toRGB(plot_aes$area$line_color, 1))),
      name = "CEA area")
  # cloud
  for (comp in 1:he$n.comparisons) {
    ceplane <- plotly::add_trace(
      ceplane,
      type = "scatter", mode = "markers",
      data = kd[kd$comparison == levels(kd$comparison)[comp],],
      y = ~delta.c,
      x = ~delta.e,
      marker = list(
        color = ifelse(
          grepl(pattern = "^rgba\\(", x = plot_aes$point$colors[comp]),
          plot_aes$point$colors[comp],
          plotly::toRGB(plot_aes$point$colors[comp])),
        size = plot_aes$point$sizes[comp]
      ),
      hoverinfo = "name+x+y",
      name = ~label)
  }
  # ICER
  if (!all(plot_aes$ICER$sizes <= 0)) {
    means_table = tabulate_means(he, comparisons.label)
    for (comp in 1:he$n.comparisons) {
      ceplane <- plotly::add_trace(
        ceplane,
        type = "scatter", mode = "markers",
        data = means_table[comp,],
        x = ~lambda.e,
        y = ~lambda.c,
        marker = list(
          color = plot_aes$ICER$colors[comp],
          size = plot_aes$ICER$sizes[comp]
        ),
        name = ~paste(
          ifelse(he$n.comparisons > 1, as.character(label), ""),
          "ICER:",
          prettyNum(round(ICER,2), big.mark = ","))
      )
    }
  }
  # layout
  legend_list = list(orientation = "h", xanchor = "center", x = 0.5)
  ceplane <- plotly::layout(
    ceplane,
    title = plot_annotations$title,
    xaxis = list(
      hoverformat = ".2f", range = xrng,
      title = plot_annotations$xlab
    ),
    yaxis = list(
      hoverformat = ".2f", range = yrng,
      title = plot_annotations$ylab
    ),
    showlegend = TRUE,
    legend = legend_list
  )
  
  plotly::config(ceplane, displayModeBar = FALSE)
}
