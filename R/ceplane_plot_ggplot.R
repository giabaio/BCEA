
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
  
  delta_ce <-
    data.frame(
      delta_e = c(he$delta_e),
      delta_c = c(he$delta_c),
      ##TODO:
      comparison = as.factor(sort(rep(
        1:he$n_comparisons, dim(he$delta_e)[1]))))
  
  graph_params <- ceplane_ggplot_params(he, graph_params)
  
  # legend_params <- make_legend_ggplot(he, pos_legend)

  theme_add <- purrr::keep(extra_params, is.theme)
  
  ggplot(delta_ce, aes(x = delta_e, y = delta_c, col = comparison)) +
    geom_point(aes(size = comparison)) +
    theme_ceplane() + 
    theme_add +           #opt.theme
    scale_color_manual(
      labels = graph_params$plot$comparisons.label,
      values = graph_params$plot$line$colors,
      # values = plot_aes$point$colors,
      na.value = "black") +
    scale_size_manual(
      labels = graph_params$plot$comparisons.label,
      values = graph_params$plot$line$size,
      na.value = 1) +
    scale_x_continuous(limits = range.e) +
    scale_y_continuous(limits = range.c) +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0, colour = "grey") +
    do.call(annotate, annot_line_params) +
    do.call(annotate, annot_polygon_params) +
    do.call(annotate, annot_wtp_params) +
    do.call(labs,
            list(title = plot_annot$title,
                 x = plot_annot$xlab,
                 y = plot_annot$ylab)) +
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
