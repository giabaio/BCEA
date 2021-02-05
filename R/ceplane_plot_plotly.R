
#' Calculate Dataset For ICERs From bcea Object
#'
#' @template args-he
#' @param comp_label Optional vector of strings with comparison labels
#' @param ... Additional arguments
#' 
#' @return A data.frame object including mean outcomes, comparison identifier,
#'   comparison label and associated ICER
#' 
#' @export
#' 
tabulate_means <- function(he,
                           comp_label = NULL,
                           ...) {
  
  if (is.null(comp_label))
    comp_label <- 1:he$n_comparisons
  
  data.frame(
    lambda.e = sapply(1:he$n_comparisons,
                      function(x) mean(as.matrix(he$delta_e)[, x])),
    lambda.c = sapply(1:he$n_comparisons,
                      function(x) mean(as.matrix(he$delta_c)[, x])),
    comparison = as.factor(1:he$n_comparisons),
    label = comp_label,
    ICER = he$ICER)
}


#' @rdname ceplane_plot_graph
#'  
#' @return For plotly returns a plot in the Viewer
#' @importFrom plotly toRGB plot_ly add_trace layout config
#' @export
#' 
ceplane_plot_plotly.bcea <- function(he,
                                     wtp = 25000,
                                     pos_legend,
                                     graph_params, ...) {
  
  comp_label <-
    paste(he$interventions[he$ref], "vs", he$interventions[he$comp])
  
  # single long format for plotting data
  delta_ce <-
    merge(
      melt(
        cbind(sim = seq_len(nrow(he$delta_c)),
              he$delta_c),
        variable.name = "comparison",
        value.name = "delta_c",
        id.vars = "sim"),
      melt(
        cbind(sim = seq_len(nrow(he$delta_e)),
              he$delta_e),
        variable.name = "comparison",
        value.name = "delta_e",
        id.vars = "sim"),
      by = c("sim", "comparison"))
  
  ICER_size <- ifelse(he$n_comparisons == 1, 8, 0)
  
  if (length(graph_params$point$colors) != length(comp_label))
    graph_params$point$colors <- rep_len(graph_params$point$colors, length(comp_label))
  
  if (length(graph_params$point$sizes) != length(comp_label))
    graph_params$point$sizes <- rep_len(graph_params$point$sizes, length(comp_label))
  
  if (!"colors" %in% names(graph_params$ICER) ||
      length(graph_params$ICER$colors) != length(comp_label))
    graph_params$ICER <- c(graph_params$ICER,
                           list(colors = rep_len("red", length(comp_label))))
  
  if (!"sizes" %in% names(graph_params$ICER) ||
      length(graph_params$ICER$sizes) != length(comp_label))
    graph_params$ICER$sizes <- rep_len(graph_params$ICER_size, length(comp_label))
  
  # plot limits
  range.e <- range(delta_ce$delta_e)
  range.c <- range(delta_ce$delta_c)
  range.e[1] <- ifelse(range.e[1] < 0,
                       yes = range.e[1],
                       no = -range.e[1])
  range.c[1] <- ifelse(range.c[1] < 0,
                       yes = range.c[1],
                       no = -range.c[1])
  
  # ce plane data
  x1 <- range.e[1] - 2*abs(diff(range.e))
  x2 <- range.e[2] + 2*abs(diff(range.e))
  x <- c(x1, x2, x2)
  y <- c(x1*wtp, x2*wtp, x1*wtp)
  plane <- data.frame(x = x, y = y)
  
  # build a trapezoidal plane instead of a triangle if
  # the y value is less than the minimum difference on costs
  if (y[1] > 1.2*range.c[1])
    plane <- rbind(plane,
                   c(x2, 2*range.c[1]), #new bottom-right vertex
                   c(x1, 2*range.c[1])) #new bottom-left vertex
  
  xrng <- c(ifelse(prod(range.e) < 0,
                   range.e[1]*1.1,
                   ifelse(range.e[1] < 0,
                          range.e[1]*1.1,
                          -(range.e[2] - range.e[1])*0.1)),
            ifelse(prod(range.e) < 0, range.e[2]*1.1,
                   ifelse(range.e[2] > 0,
                          range.e[2]*1.1,
                          (range.e[2] - range.e[1])*0.1)))
  yrng <- c(ifelse(prod(range.c) < 0,
                   range.c[1]*1.1,
                   ifelse(range.c[1] < 0,
                          range.c[1]*1.1,
                          -(range.c[2] - range.c[1])*0.1)),
            ifelse(prod(range.c) < 0,
                   range.c[2]*1.1,
                   ifelse(range.c[2] > 0,
                          range.c[2]*1.1,
                          (range.c[2] - range.c[1])*0.1)))
  
  pt_cols <-
    ifelse(test = grepl(pattern = "^rgba\\(",
                        x = graph_params$point$colors),
           yes = plotly::toRGB(graph_params$point$colors),
           no = graph_params$point$colors)
  
  # plot set-up
  ceplane <- plotly::plot_ly(colors = pt_cols)
  
  ceplane <-
    ceplane %>% 
    plotly::add_trace(
      type = "scatter",
      mode = "markers",
      data = delta_ce,
      y = ~delta_c,
      x = ~delta_e,
      color = ~comparison,
      hoverinfo = "name+x+y")
  
  if (graph_params$area_include) {
    
    ceplane <-
      ceplane %>% 
      plotly::add_trace(
        type = "scatter",
        mode = "lines",
        data = plane,
        x = ~x,
        y = ~y,
        line = list(color = ifelse(
          grepl(pattern = "^rgba\\(",
                x = graph_params$area$line_color),
          graph_params$area$line_color,
          plotly::toRGB(graph_params$area$line_color, 1))),
        showlegend = FALSE,
        inherit = FALSE)
    
    graph_params$area$col <- "grey"
    
    poly_col <-
    ifelse(
      grepl(pattern = "^rgba\\(",
            x = graph_params$area$col),
      graph_params$area$color,
      plotly::toRGB(graph_params$area$col, 0.5))
    
    ceplane <-
      ceplane %>% 
      plotly::add_polygons(
        data = plane,
        x = ~x,
        y = ~y,
        fillcolor = poly_col,
        line = list(color = 'transparent'),
        opacity = 0.3,
        name = "CEA area",
        hoveron = "points",
        showlegend = FALSE,
        inherit = FALSE)
  }
  
  # ICER
  if (!all(graph_params$ICER$sizes <= 0)) {
    means_table <- tabulate_means(he, comp_label)
    
    for (comp in seq_len(he$n_comparisons)) {
      ceplane <- plotly::add_trace(
        ceplane,
        type = "scatter",
        mode = "markers",
        data = means_table[comp, ],
        x = ~lambda.e,
        y = ~lambda.c,
        marker = list(
          color = graph_params$ICER$colors[comp],
          size = graph_params$ICER$sizes[comp]),
        name = ~paste(
          ifelse(he$n_comparisons > 1,
                 yes = "",#as.character(label),
                 no = ""),
          "ICER:",
          prettyNum(round(ICER, 2), big.mark = ",")))
    }
  }
  
  legend_params <- make_legend_plotly(pos_legend)
  
  ceplane <- plotly::layout(
    ceplane,
    title = graph_params$title,
    xaxis = list(
      hoverformat = ".2f",
      range = xrng,
      title = graph_params$xlab),
    yaxis = list(
      hoverformat = ".2f",
      range = yrng,
      title = graph_params$ylab),
    showlegend = TRUE,
    legend = legend_params)
  
  plotly::config(ceplane, displayModeBar = FALSE)
}


#' @rdname ceplane_plot_graph
#' @template args-he
#' @param ... Additional arguments
#' @export
#' 
ceplane_plot_plotly <- function(he, ...) {
  UseMethod('ceplane_plot_plotly', he)
}

