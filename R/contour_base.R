
#' Contour Cost-Effectiveness Plane
#'
#' Choice of base R, ggplot2
#' @name contour_graph
#' 
NULL


#' Contour plot base R version
#' @rdname contour_graph
#' 
#' @template args-he
#' @param graph_params Plot parameters
#' @param extra_args additional arguments
#' 
contour_base <- function(he,
                         graph_params,
                         extra_args) {
  browser()
  plot_params <-
    contour_base_params(he, graph_params)
  
  legend_params <-
    ceplane_legend_base(he, graph_params$pos_legend, plot_params)
  
  add_ceplane_setup(plot_params)
  add_ceplane_points(he, plot_params)
  add_axes()
  
  # only plot one comparison icer
  do.call("points",
          c(list(
            x = colMeans(he$delta_e),
            y = colMeans(he$delta_c)),
            plot_params$icer_points),
          quote = TRUE)
  
  add_ceplane_legend(legend_params)
  
  add_contour_quadrants(plot_params)
  add_contours(he, plot_params)
}

