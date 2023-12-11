
#' Contour Cost-Effectiveness Plane
#'
#' Choice of base R, \pkg{ggplot2}.
#' @name contour_graph
#' @seealso [contour()]
NULL


#' Contour Plot Base R Version
#' @rdname contour_graph
#' 
#' @template args-he
#' @param pos_legend Legend position
#' @param graph_params Plot parameters; list
#' @param ... Additional arguments
#' 
contour_base <- function(he,
                         pos_legend,
                         graph_params,
                         ...) {
  extra_args <- list(...)
  
  plot_params <-
    contour_base_params(he, graph_params)
  
  legend_params <-
    ceplane_legend_base(he, pos_legend, plot_params)
  
  add_ceplane_setup(plot_params)
  add_ceplane_points(he, plot_params)
  add_axes()
  add_ceplane_legend(legend_params)
  add_contour_quadrants(he, plot_params)
  add_contours(he, plot_params)
}


#' Contour Plot ggplot2 Version
#' @rdname contour_graph
#' 
#' @template args-he
#' @param pos_legend Legend position
#' @param graph_params Plot parameters; list
#' @param ... Additional arguments
#' 
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom dplyr mutate
#' @importFrom reshape2 melt
#' 
contour_ggplot <- function(he,
                           pos_legend,
                           graph_params,
                           ...) {
  extra_args <- list(...)

  plot_params <-
    contour_ggplot_params(he, pos_legend, graph_params, ...)
  
  th_contour = theme_contour()
  if (!is.null(plot_params$legend$legend.position))
    th_contour = th_contour + ggplot2::theme(legend.position = plot_params$legend$legend.position)
  if (!is.null(plot_params$legend$legend.justification))
    th_contour = th_contour + ggplot2::theme(legend.justification = plot_params$legend$legend.justification)
  if (!is.null(plot_params$legend$legend.direction))
    th_contour = th_contour + ggplot2::theme(legend.direction = plot_params$legend$legend.direction)
  
  theme_add <- purrr::keep(list(...), is.theme)
  
  # single long format for ggplot data
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
      by = c("sim", "comparison")) %>% 
    mutate(comparison = factor(.data$comparison))
  
  ggplot(delta_ce,
         aes(x = .data$delta_e, y = .data$delta_c, group = .data$comparison,
             col = .data$comparison, shape = .data$comparison)) +
    geom_point(size = plot_params$point$size) +
    do.call(geom_density_2d, plot_params$contour) +
    geom_quad_txt(he, plot_params) +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0, colour = "grey") +
    ceplane_legend_manual(he, plot_params) +
    coord_cartesian(xlim = plot_params$xlim,
                    ylim = plot_params$ylim,
                    expand = TRUE) +
    do.call(labs,
            list(title = plot_params$title,
                 x = plot_params$xlab,
                 y = plot_params$ylab)) +
    do.call(theme, plot_params$legend) +
    th_contour +
    theme_add
}

#' @rdname contour_graph
#'  
#' @return For \pkg{plotly} returns a plot in the Viewer
#' 
contour_plotly <- function(he,
                           pos_legend,
                           graph_params, 
                           ...) {
  extra_args <- list(...)
  comp_label <-
    paste(he$interventions[he$ref], "vs", he$interventions[he$comp])
  
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
  
  if (length(graph_params$point$colors) != length(comp_label))
    graph_params$point$colors <- rep_len(graph_params$point$color, length(comp_label))
  
  pt_cols <-
    ifelse(test = grepl(pattern = "^rgba\\(",
                        x = graph_params$point$colors),
           yes = plotly::toRGB(graph_params$point$colors),
           no = graph_params$point$colors)
  
  density <- lapply(
    1:ncol(he$delta_e),
    function(ndx) {
      MASS::kde2d(
        delta_ce$delta_e[delta_ce$comparison |> as.numeric() == ndx],
        delta_ce$delta_c[delta_ce$comparison |> as.numeric() == ndx],
        n = 300,
        h = c(sd(he$delta_e[, ndx] |> c())/graph_params$scale,
              sd(he$delta_c[, ndx] |> c())/graph_params$scale))
    })
  for (ii in 1:length(density)) {
    density[[ii]]$z = density[[ii]]$z/max(density[[ii]]$z)
    density[[ii]]$comparison = factor(ii, levels = 1:length(density), labels = levels(delta_ce$comparison))
  }
  
  contour_plot = plotly::plot_ly() |>
    plotly::add_trace(
      type = "scatter",
      mode = "markers",
      data = delta_ce,
      x = ~delta_e,
      y = ~delta_c,
      color = ~comparison,
      colors = pt_cols
    )
  
  for (ndx in 1:length(density)) {
    # work-around for uneven contour levels: add one trace per contour line
    for (contour_line in graph_params$levels) {
      contour_plot = contour_plot |>
        plotly::add_contour(
          z = density[[ndx]]$z,
          x = density[[ndx]]$x,
          y = density[[ndx]]$y,
          transpose = TRUE,
          showlegend = FALSE,
          showscale = FALSE,
          autocolorscale = FALSE,
          contours = list(
            type = "constraint",
            showlabels = TRUE,
            operation = "=",
            value = contour_line
          ),
          line = list(
            color = pt_cols[ndx]
          )
        )
    }
  }
  
  legend_params <- make_legend_plotly(pos_legend)
  
  contour_plot <- contour_plot |>
    plotly::layout(
    title = graph_params$title,
    xaxis = list(
      hoverformat = ".2f",
      title = graph_params$xlab),
    yaxis = list(
      hoverformat = ".2f",
      title = graph_params$ylab),
    showlegend = TRUE,
    legend = legend_params)
  
  plotly::config(contour_plot, displayModeBar = FALSE)
}