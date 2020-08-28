
#' ceplane_plot_ggplot
#'
#' @import ggplot2
#' @importFrom grid unit
#'
ceplane_plot_ggplot <- function(he,
                                wtp,
                                pos_legend,
                                graph_params, ...) {
  delta_ce <-
    data.frame(
      delta_e = c(he$delta_e),
      delta_c = c(he$delta_c),
      ##TODO:
      comparison = as.factor(sort(rep(
        1:he$n_comparisons, dim(he$delta_e)[1]))))
  
  scale_size_manual <- 
    if (!plot_aes$exist$point$sizes)
      rep_len(1, length(comparisons.label))
  else
    rep_len(plot_aes$point$sizes,
            length(comparisons.label))
  
  ceplane <-
    ggplot(delta_ce, aes(x = delta_e, y = delta_c, col = comparison)) +
    theme_bw() +
    scale_color_manual(
      labels = comparisons.label,
      values = plot_aes$point$colors,
      na.value = "black") +
    scale_size_manual(
      labels = comparisons.label,
      values = scale_size_manual,
      na.value = 1) +
    scale_x_continuous(limits = range.e, oob = do.nothing) +
    scale_y_continuous(limits = range.c, oob = do.nothing) +
    annotate(
      "line",
      x = plane[1:2, 1],
      y = plane[1:2, 2],
      color = ifelse(
        !plot_aes$exist$area$line_color,
        "black",
        plot_aes$area$line_color)) +
    annotate(
      "polygon",
      plane$x,
      plane$y,
      fill = ifelse(
        is.null(plot_aes$area$color),
        "light gray",
        plot_aes$area$color
      ),
      alpha = 0.3) +
    geom_hline(aes(yintercept = 0), colour = "grey") +
    geom_vline(aes(xintercept = 0), colour = "grey") +
    geom_point(aes(size = comparison))
  
  ##TODO:
  ## why is this decided by sizes?
  if (!all(plot_aes$ICER$sizes <= 0)) {
    ceplane <- ceplane +
      geom_point(
        data = means,
        aes(x = lambda.e, y = lambda.c),
        colour = plot_aes$ICER$colors,
        size = plot_aes$ICER$sizes)
  }
  
  
  ##TODO:
  # wtp label
  ceplane <- ceplane +
    annotate(
      geom = "text",
      x = x.pt,
      y = y.pt,
      label = paste0("k = ", format(wtp, digits = 6)),
      hjust = 0.15,
      size = size)
  
  # subset_by_comparisons()
  
  ceplane <-
    ceplane +
    labs(title = plot_annotations$title,
         x = plot_annotations$xlab,
         y = plot_annotations$ylab)
  
  ##TODO:
  # legend
  
  
  ceplane <-
    ceplane +
    theme(
      legend.position = alt.legend,
      legend.justification = jus,
      legend.title = element_blank(),
      legend.background = element_blank()) +
    theme(
      text = element_text(size = 11),
      legend.key.size = grid::unit(0.66, "lines"),
      legend.spacing = grid::unit(-1.25, "line"),
      panel.grid = element_blank(),
      legend.key = element_blank(),
      legend.text.align = 0) +
    theme(plot.title = element_text(
      lineheight = 1.05,
      face = "bold",
      size = 14.3,
      hjust = 0.5))
  # if n_comparisons == 1
  + theme(legend.key.size = grid::unit(0.1, "lines")) +
    opt.theme
}
