
#' Contour Cost-Effectiveness Plane
#'
#' Choice of base R, ggplot2
#' @name contour_graph
#' @seealso \code{\link{contour}}
NULL


#' Contour Plot Base R Version
#' @rdname contour_graph
#' 
#' @template args-he
#' @template args-pos
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
#' @template args-pos
#' @param graph_params Plot parameters; list
#' @param ... Additional arguments
#' 
#' @import ggplot2
#' @importFrom grid unit
#' 
contour_ggplot <- function(he,
                           pos_legend,
                           graph_params,
                           ...) {
  extra_args <- list(...)

  plot_params <-
    contour_ggplot_params(he, graph_params, ...)
  
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
    mutate(comparison = factor(comparison))

  ggplot(delta_ce,
         aes(x = .data$delta_e, y = .data$delta_c, group = .data$comparison,
             col = .data$comparison, shape = .data$comparison)) +
    geom_point(size = plot_params$point$size) +
    do.call(geom_density_2d, plot_params$contour) +
    geom_quad_txt(he, plot_params) +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0, colour = "grey") +
    scale_color_manual(labels = line_labels.default(he),
                       values = plot_params$point$color) +
    scale_shape_manual(labels = line_labels.default(he),
                       values = plot_params$point$shape) +
    coord_cartesian(xlim = plot_params$xlim,
                    ylim = plot_params$ylim,
                    expand = TRUE) +
    do.call(labs,
            list(title = plot_params$title,
                 x = plot_params$xlab,
                 y = plot_params$ylab)) +
    do.call(theme, plot_params$legend) +
    theme_contour() +
    theme_add
}

