
#' ceplane_plot_ggplot
#'
#' @template args-he
#' @param wtp 
#' @param pos_legend 
#' @param graph_params 
#' @param ... 
#'
#' @import ggplot2
#' @importFrom grid unit
#' 
#' @keywords hplot
#'
ceplane_plot_ggplot <- function(he,
                                wtp,
                                pos_legend,
                                graph_params, ...) {

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
      by = c("sim", "comparison"))
  
  graph_params <- ceplane_ggplot_params(he, graph_params, ...)
  theme_add <- purrr::keep(extra_params, is.theme)
  
  ggplot(delta_ce, aes(x = delta_e, y = delta_c, col = comparison)) +
    do.call(geom_polygon, polygon_params) +
    geom_point() +
    theme_ceplane() +
    # theme_add +
    scale_color_manual(
      labels = graph_params$legend$comparisons.label,
      values = graph_params$point$colors,
      na.value = "black") +
    scale_size_manual(
      values = graph_params$point$size,
      na.value = 1) +
    scale_color_manual(labels = line_labels.default(he),
                       values = graph_params$point$colors) +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0, colour = "grey") +
    coord_cartesian(xlim = graph_params$xlim,
                    ylim = graph_params$ylim) +
    do.call(labs,
            list(title = graph_params$title,
                 x = graph_params$xlab,
                 y = graph_params$ylab)) +
    do.call(geom_abline, list(slope = wtp,
                              col = "black")) +
    do.call(geom_point, icer_point_params) +
    do.call(theme, legend_params) +
    do.call(annotate, k_text_params)
  
  # subset_by_comparisons()
}
