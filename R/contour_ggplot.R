
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
  
  quad <- graph_params$quadrant
  
  quad_txt <-
    data.frame(
      x = c(params$xlim[2], params$xlim[1], params$xlim[1], params$xlim[2]),
      y = c(params$ylim[2], params$ylim[2], params$ylim[1], params$ylim[1]),
      label = c(quad$t1, quad$t2, quad$t3, quad$t4),
      hjust = c(1, 0, 0, 1))

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
    do.call(geom_density_2d, graph_params$contour) +
    geom_text(data = quad_txt,
              aes(x = .data$x,
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
                    expand = TRUE) +
    do.call(labs,
            list(title = graph_params$title,
                 x = graph_params$xlab,
                 y = graph_params$ylab)) +
    do.call(geom_point, graph_params$icer) +
    do.call(theme, graph_params$legend) +
    theme_contour() +
    theme_add
}

