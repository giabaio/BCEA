
#' Cost-Effectiveness Plane Plot By Graph Device
#'
#' Choice of base R, \pkg{ggplot2} or \pkg{plotly}.
#' 
#' @template args-he
#' @param wtp Willingness to pay threshold; default 25,000
#' @param pos_legend Legend position
#' @param graph_params Graph parameters in \pkg{ggplot2} format
#' @param ... Additional arguments
#' 
#' @examples
#' # single comparator
#' data(Vaccine, package = "BCEA")
#' 
#' he <- bcea(eff, cost)
#' ceplane.plot(he, graph = "base")
#' 
#' \dontrun{
#' # need to provide all the defaults because thats what
#' # ceplane.plot() does
#' 
#' graph_params <-  list(xlab = "x-axis label",
#'                       ylab = "y-axis label",
#'                       title = "my title",
#'                       xlim = c(-0.002, 0.001),
#'                       ylim = c(-13, 5),
#'                       point = list(sizes = 1,
#'                                    colors = "darkgrey"),
#'                       area = list(color = "lightgrey"))
#'                       
#' he$delta_e <- as.matrix(he$delta_e)
#' he$delta_c <- as.matrix(he$delta_c)
#' 
#' BCEA::ceplane_plot_base(he, graph_params = graph_params)
#' 
#' ## single non-default comparator
#' 
#' 
#' ## multiple comparators
#' data(Smoking)
#' 
#' graph_params <-  list(xlab = "x-axis label",
#'                       ylab = "y-axis label",
#'                       title = "my title",
#'                       xlim = c(-1, 2.5),
#'                       ylim = c(-1, 160),
#'                       point = list(sizes = 0.5,
#'                                    colors = grey.colors(3, start = 0.1, end = 0.7)),
#'                       area = list(color = "lightgrey"))
#'                                    
#' he <- bcea(eff, cost, ref = 4, Kmax = 500, interventions = treats)
#' 
#' BCEA::ceplane_plot_base(he,
#'                         wtp = 200,
#'                         pos_legend = FALSE,
#'                         graph_params = graph_params)
#' }
#' 
#' @name ceplane_plot_graph
NULL


#' @rdname ceplane_plot_graph
#' 
#' @return For base R returns a plot
#' @keywords hplot
#' 
ceplane_plot_base.bcea <- function(he,
                                   pos_legend,
                                   graph_params, ...) {
  plot_params <-
    ceplane_base_params(he, graph_params)
  
  legend_params <-
    ceplane_legend_base(he, pos_legend, plot_params)
  add_ceplane_setup(plot_params)
  add_ceplane_polygon(plot_params)
  add_ceplane_points(he, plot_params)
  add_axes()
  add_ceplane_icer(he, plot_params)
  add_ceplane_k_txt(plot_params)
  add_ceplane_legend(legend_params)
}

#' @rdname ceplane_plot_graph
#' 
ceplane_plot_base <- function(he, ...) {
  UseMethod('ceplane_plot_base', he)
}

#' @rdname ceplane_plot_graph
#'
#' @return For \pkg{ggplot2} returns \pkg{ggplot2} object
#' 
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom purrr keep
#' @importFrom scales label_dollar
#' 
#' @keywords hplot
#' 
#' @examples
#' 
#' data(Vaccine)
#' he <- bcea(eff, cost)
#' 
#' ceplane.plot(he, graph = "ggplot2")
#' ceplane.plot(he, wtp=10000, graph = "ggplot2",
#'              point = list(colors = "blue", sizes = 2),
#'              area = list(col = "springgreen3"))
#' 
#' data(Smoking)
#' he <- bcea(eff, cost, ref = 4, Kmax = 500, interventions = treats)
#' 
#' ceplane.plot(he, graph = "ggplot2")
#' 
#' ceplane.plot(he,
#'              wtp = 200,
#'              pos = "right",
#'              ICER_size = 2,
#'              graph = "ggplot2")
#'    
#' ceplane.plot(he,
#'              wtp = 200,
#'              pos = TRUE,
#'              graph = "ggplot2")
#'
#' ceplane.plot(he,
#'              graph = "ggplot2",
#'              wtp=200,
#'              theme = ggplot2::theme_linedraw())
#'              
ceplane_plot_ggplot.bcea <- function(he,
                                     pos_legend,
                                     graph_params, ...) {
  comp_label <- paste(he$interventions[he$ref], "vs", he$interventions[he$comp])
  if (length(graph_params$point$size) != length(comp_label)) {
    graph_params$point$size <- rep(graph_params$point$size[1], length(comp_label))
  }
  if (length(graph_params$point$color) != length(comp_label)) {
    graph_params$point$color <- rep(graph_params$point$color[1], length(comp_label))
  }
  
  # single long format for ggplot data
  delta_c <- he$delta_c |>
    as_tibble() |>
    mutate(sim = row_number()) |>
    pivot_longer(
      cols = -"sim",
      names_to = "comparison",
      values_to = "delta_c"
    )
  
  delta_e <- he$delta_e |>
    as_tibble() |>
    mutate(sim = row_number()) |>
    pivot_longer(
      cols = -"sim",
      names_to = "comparison",
      values_to = "delta_e"
    )
  
  delta_ce <- left_join(delta_c, delta_e, by = c("sim", "comparison"))
  
  plot_params <-
    ceplane_ggplot_params(he, pos_legend = pos_legend, graph_params = graph_params, ...)
  plot_params$area_include <- plot_params$area$include
  plot_params$area$include <- NULL
  
  theme_add <- Filter(f = \(val) ggplot2::is_theme(val), x = list(...))
  
  return_plot <- ggplot(delta_ce,
                        aes(x = .data$delta_e, y = .data$delta_c,
                            group = factor(.data$comparison),
                            col = factor(.data$comparison),
                            shape = factor(.data$comparison))
  )
  
  if (plot_params$area_include) {
    return_plot <- return_plot + do.call(geom_polygon, plot_params$area)
  }
  
  return_plot <- return_plot +
    theme_ceplane() +
    theme_add +
    geom_point(size = plot_params$point$size[delta_ce$comparison]) +
    ceplane_legend_manual(he, plot_params) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_vline(xintercept = 0, color = "grey") +
    geom_text(
      data = data.frame(x = colMeans(he$delta_e),
                        y = colMeans(he$delta_c)),
      aes(x = .data$x, y = .data$y,
          label = if (plot_params$icer_annot) {
            line_labels.default(he, ref_first = graph_params$ref_first)
          } else {""}),
      inherit.aes = FALSE, show.legend = FALSE,
      hjust = 0, vjust = 0
    ) +
    scale_y_continuous(labels = scales::label_dollar(prefix = plot_params$currency)) +
    coord_cartesian(xlim = plot_params$xlim,
                    ylim = plot_params$ylim,
                    expand = FALSE) +
    do.call(labs,
            list(title = plot_params$title,
                 x = plot_params$xlab,
                 y = plot_params$ylab)) +
    do.call(geom_abline, c(slope = plot_params$wtp_value, plot_params$line)) +
    do.call(geom_point, plot_params$icer) +
    do.call(annotate, plot_params$wtp) +
    do.call(annotate, plot_params$icer_txt) +
    do.call(theme, list(
      axis.text = element_text(size = graph_params$text$size),
      axis.title.x = element_text(size = graph_params$text$size),
      axis.title.y = element_text(size = graph_params$text$size))) +
    do.call(theme, plot_params$legend)
  
  return(return_plot)
}

#' @rdname ceplane_plot_graph
#' 
ceplane_plot_ggplot <- function(he, ...) {
  UseMethod('ceplane_plot_ggplot', he)
}

#' @rdname ceplane_plot_graph
#'  
#' @return For \pkg{plotly} returns a plot in the Viewer
#' 
ceplane_plot_plotly.bcea <- function(he,
                                     wtp = 25000,
                                     pos_legend,
                                     graph_params, ...) {
  
  comp_label <- paste(he$interventions[he$ref], "vs", he$interventions[he$comp])
  
  # Set wtp from graph_params if supplied
  wtp <- graph_params$wtp_value
  
  # single long format for plotting data
  delta_c <- he$delta_c |>
    as_tibble() |>
    mutate(sim = row_number()) |>
    pivot_longer(
      cols = -"sim",
      names_to = "comparison",
      values_to = "delta_c"
    )
  
  delta_e <- he$delta_e |>
    as_tibble() |>
    mutate(sim = row_number()) |>
    pivot_longer(
      cols = -"sim",
      names_to = "comparison",
      values_to = "delta_e"
    )
  
  delta_ce <- left_join(delta_c, delta_e, by = c("sim", "comparison"))
  
  # Adjust graph parameters
  if (length(graph_params$point$color) != length(comp_label)) {
    graph_params$point$color <- rep_len(graph_params$point$color[1], length(comp_label))
  }
  if (length(graph_params$point$size) != length(comp_label)) {
    graph_params$point$size <- rep_len(graph_params$point$size[1], length(comp_label))
  }
  if (!exists("icer", graph_params)) graph_params$icer <- list()
  if (!"color" %in% names(graph_params$icer)) graph_params$icer$color <- "red"
  if (!"size" %in% names(graph_params$icer)) {
    graph_params$icer$size <- ifelse(he$n_comparisons == 1, 8, 0)
  }
  if (length(graph_params$icer$color) != length(comp_label)) {
    graph_params$icer$color <- rep_len(graph_params$icer$color[1], length(comp_label))
  }
  if (length(graph_params$icer$size) != length(comp_label)) {
    graph_params$icer$size <- rep_len(graph_params$icer$size[1], length(comp_label))
  }
  
  # plot limits
  range.e <- range(delta_ce$delta_e)
  range.c <- range(delta_ce$delta_c)
  range.e[1] <- ifelse(range.e[1] < 0, range.e[1], -range.e[1])
  range.c[1] <- ifelse(range.c[1] < 0, range.c[1], -range.c[1])
  
  # ce plane data
  x1 <- range.e[1] - 2 * abs(diff(range.e))
  x2 <- range.e[2] + 2 * abs(diff(range.e))
  x <- c(x1, x2, x2)
  y <- c(x1 * wtp, x2 * wtp, x1 * wtp)
  plane <- data.frame(x = x, y = y)
  
  # build a trapezoidal plane if needed
  if (y[1] > 1.2 * range.c[1]) {
    plane <- rbind(
      plane,
      c(x2, 2 * range.c[1]), # new bottom-right vertex
      c(x1, 2 * range.c[1])  # new bottom-left vertex
    )
  }
  
  xrng <- c(
    ifelse(prod(range.e) < 0,
           range.e[1] * 1.1,
           ifelse(range.e[1] < 0,
                  range.e[1] * 1.1,
                  -(range.e[2] - range.e[1]) * 0.1)),
    ifelse(prod(range.e) < 0, range.e[2] * 1.1,
           ifelse(range.e[2] > 0,
                  range.e[2] * 1.1,
                  (range.e[2] - range.e[1]) * 0.1))
  )
  
  yrng <- c(
    ifelse(prod(range.c) < 0,
           range.c[1] * 1.1,
           ifelse(range.c[1] < 0,
                  range.c[1] * 1.1,
                  -(range.c[2] - range.c[1]) * 0.1)),
    ifelse(prod(range.c) < 0,
           range.c[2] * 1.1,
           ifelse(range.c[2] > 0,
                  range.c[2] * 1.1,
                  (range.c[2] - range.c[1]) * 0.1))
  )
  
  pt_cols <- ifelse(
    grepl("^rgba\\(", graph_params$point$color),
    plotly::toRGB(graph_params$point$color),
    graph_params$point$color
  )
  
  # define point size variable in delta_ce
  delta_ce$pt_size <- graph_params$point$size[as.numeric(delta_ce$comparison)]
  
  # plot set-up
  ceplane <- plotly::plot_ly(colors = pt_cols) %>%
    plotly::add_trace(
      type = "scatter",
      mode = "markers",
      data = delta_ce,
      y = ~delta_c,
      x = ~delta_e,
      color = ~comparison,
      size = delta_ce$pt_size,
      hoverinfo = "name+x+y"
    )
  
  if (graph_params$area_include) {
    if (is.null(graph_params$line$color)) {
      if (is.null(graph_params$area$line_color))
        graph_params$area$line_color <- "grey20"
    } else {
      graph_params$area$line_color <- graph_params$line$color
    }
    
    ceplane <- ceplane %>%
      plotly::add_trace(
        type = "scatter",
        mode = "lines",
        data = plane,
        x = ~x,
        y = ~y,
        line = list(color = ifelse(
          grepl("^rgba\\(", graph_params$area$line_color),
          graph_params$area$line_color,
          plotly::toRGB(graph_params$area$line_color, 1))),
        showlegend = FALSE,
        inherit = FALSE
      )
    
    poly_col <- ifelse(
      grepl("^rgba\\(", graph_params$area$color),
      graph_params$area$color,
      plotly::toRGB(graph_params$area$color)
    )
    
    ceplane <- ceplane %>%
      plotly::add_polygons(
        data = plane,
        x = ~x,
        y = ~y,
        fillcolor = poly_col,
        line = list(color = "transparent"),
        opacity = 0.3,
        name = "CEA area",
        hoveron = "points",
        showlegend = FALSE,
        inherit = FALSE
      )
  }
  
  # ICER
  if (!all(graph_params$icer$size <= 0)) {
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
          color = graph_params$icer$color[comp],
          size = graph_params$icer$size[comp]
        ),
        name = ~paste0(
          ifelse(he$n_comparisons > 1, "", ""),
          "ICER ", label, ": ",
          prettyNum(round(ICER, 2), big.mark = ",")
        )
      )
    }
  }
  
  legend_params <- make_legend_plotly(pos_legend)
  
  ceplane <- plotly::layout(
    ceplane,
    title = graph_params$title,
    xaxis = list(
      hoverformat = ".2f",
      range = xrng,
      title = graph_params$xlab
    ),
    yaxis = list(
      hoverformat = ".2f",
      range = yrng,
      title = graph_params$ylab
    ),
    showlegend = TRUE,
    legend = legend_params
  )
  
  plotly::config(ceplane, displayModeBar = FALSE)
}

#' @rdname ceplane_plot_graph
#' @template args-he
#' @param ... Additional arguments
#' 
ceplane_plot_plotly <- function(he, ...) {
  UseMethod('ceplane_plot_plotly', he)
}
