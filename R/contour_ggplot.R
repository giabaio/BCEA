
#' Contour plot ggplot2 version
#' @rdname contour_graph
#' 
#' @template args-he
#' @param params Plot parameters
#' @param extra_args additional arguments
#' 
#' @importFrom grid unit
#' @import ggplot2
#' 
contour_ggplot <- function(he, params, extra_args) {
  
  xlab <- params$xlab
  ylab <- params$ylab
  title <- params$title
  alt.legend <- params$alt.legend
  scale <- params$scale
  nlevels <- params$nlevels
  levels <- params$levels
  xlim <- params$xlim
  ylim <- params$ylim
  
  quadrants <- quadrant_params(he, params)
  ranges <- prep_contour_ggplot_range(he)
  
  if (!is.null(nlevels)) {
    nlevels <- round(nlevels)
    if (nlevels < 0)
      nlevels <- 10
    if (nlevels == 0)
      nlevels <- 1
  }
  
  points.colour <- 
    if (nlevels == 1) {
      "black"
    } else {
      "grey"}
  
  theme_add <- purrr::keep(list(...), is.theme)
  
  kd <- data.frame(
    delta_e = c(as.matrix(he$delta_e)),
    delta_c = c(as.matrix(he$delta_c)),
    comparison =
      as.factor(sort(
        rep(1:he$n_comparisons, dim(as.matrix(he$delta_e))[1]))))
  
  labels.df <-
    data.frame(
      he = c(ranges$e[2], ranges$e[1], ranges$e[1], ranges$e[2]),
      y = c(rep(ranges$c[2], 2), rep(ranges$c[1], 2)),
      label = c(p.ne, p.nw, p.sw, p.se),
      hjust = as.factor(c(1, 0, 0, 1)))
  
  contour_plot <-
    ggplot(kd,
           aes(x = .data$delta_e, y = .data$delta_c,
               col = .data$comparison)) +
    stat_density2d() +
    geom_point(size = 1) +
    scale_color_manual(label = comparisons.label,
                       values = colors.label,
                       na.value = "black") +
    geom_text(data = labels.df,
              aes(x = .data$he,
                  y = .data$y,
                  hjust = .data$hjust,
                  label = .data$label),
              parse = TRUE,
              size = rel(3.5)) +
    guides(colour = guide_legend(override.aes =
                                   list(linetype = 0)))
  
  contour_plot +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0, colour = "grey") +
    coord_cartesian(xlim = graph_params$xlim,
                    ylim = graph_params$ylim,
                    expand = FALSE) +
    do.call(labs,
            list(title = graph_params$title,
                 x = graph_params$xlab,
                 y = graph_params$ylab)) +
    do.call(theme, graph_params$legend) +
    theme_contour() +
    theme_add
}

