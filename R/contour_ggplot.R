
#' Contour plot ggplot2 version
#' @rdname contour_graph
#' 
#' @template args-he
#' @param params Plot parameters
#' @param extra_args Additional arguments
#' 
#' @import ggplot2
#' @importFrom grid unit
#' 
contour_ggplot <- function(he,
                           params,
                           extra_args,
                           ...) {
  
  graph_params <-
    contour_ggplot_params(he, params, ...)
  
  theme_add <- purrr::keep(list(...), is.theme)
  
  rg <- graph_params$ranges
  quad <- graph_params$quadrant
  
  labels.df <-
    data.frame(
      he = c(rg$e[2], rg$e[1], rg$e[1], rg$e[2]),
      y = c(rep(rg$c[2], 2), rep(rg$c[1], 2)),
      label = c(quad$p.ne, quad$p.nw, quad$p.sw, quad$p.se),
      hjust = as.factor(c(1, 0, 0, 1)))

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
  
  ggplot(delta_ce,
         aes(x = .data$delta_e, y = .data$delta_c, group = factor(.data$comparison),
             col = factor(.data$comparison), shape = factor(.data$comparison))) +
    geom_point(size = graph_params$point$size) +
    stat_density_2d(aes(colour = comparison)) +
    geom_text(data = labels.df,
              aes(x = .data$he,
                  y = .data$y,
                  hjust = .data$hjust,
                  label = .data$label),
              parse = TRUE,
              size = rel(3.5),
              inherit.aes = FALSE) +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0, colour = "grey") +
    scale_color_manual(labels = line_labels.default(he),
                       values = graph_params$point$color) +
    scale_shape_manual(labels = line_labels.default(he),
                       values = graph_params$point$shape) +
    coord_cartesian(xlim = graph_params$xlim,
                    ylim = graph_params$ylim,
                    expand = FALSE) +
    do.call(labs,
            list(title = graph_params$title,
                 x = graph_params$xlab,
                 y = graph_params$ylab)) +
    do.call(geom_point, graph_params$icer) +
    do.call(theme, graph_params$legend) +
    theme_contour() +
    theme_add
}

