
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
  extra_params <- list(...)
  
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
  
  graph_params <- ceplane_ggplot_params(he, graph_params)
  
  # legend_params <- make_legend_ggplot(he, pos_legend)
  
  theme_add <- purrr::keep(extra_params, is.theme)
  
  ggplot(delta_ce, aes(x = delta_e, y = delta_c, col = comparison)) +
    geom_polygon(data = data.frame(x = polygon_params$x,
                                   y = polygon_params$y),
                 mapping = aes(x=x, y=y), fill = "lightgrey", #polygon_params$col,
                 inherit.aes = FALSE) +
    # do.call(annotate, polygon_params) +
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
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0, colour = "grey") +
    coord_cartesian(ylim = graph_params$xlim) +
    coord_cartesian(ylim = graph_params$ylim) +
    do.call(labs,
            list(title = graph_params$title,
                 x = graph_params$xlab,
                 y = graph_params$ylab)) +
    do.call(geom_abline, list(slope = wtp, col = "black")) +

    do.call(annotate, annot_wtp_params) +
    do.call(theme,
            legend_params <- list(
              legend.position = alt.legend,
              legend.justification = jus))
  
  ##TODO:
  ## why is this decided by sizes?
  if (!all(plot_aes$ICER$sizes <= 0)) {
    do.call(geom_point,
            list(
              data = means,
              aes(x = lambda.e, y = lambda.c),
              colour = plot_aes$ICER$colors,
              size = plot_aes$ICER$sizes))
  }
  
  # subset_by_comparisons()
  
  # if n_comparisons == 1
  + theme(legend.key.size = grid::unit(0.1, "lines"))
}
