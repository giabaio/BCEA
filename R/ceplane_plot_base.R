
#' @rdname ceplane_plot_graph
#' 
#' @return For base R returns a plot
#' @keywords hplot
#' @export
#' 
ceplane_plot_base.bcea <- function(he,
                                   wtp = 25000,
                                   pos_legend,
                                   graph_params, ...) {
  
  plot_params <-
    ceplane_base_params(he, wtp, graph_params)
  
  legend_params <-
    ceplane_legend_base(he, pos_legend, plot_params)
  
  add_ceplane_setup(plot_params)
  add_ceplane_polygon(plot_params)
  add_ceplane_points(he, plot_params)
  add_axes()
  add_ceplane_icer(he, plot_params)
  add_ceplane_k_txt(plot_params)
  add_ceplane_legend(legend_params)
}

#' @export
#' 
ceplane_plot_base <- function(he, ...) {
  UseMethod('ceplane_plot_base', he)
}

