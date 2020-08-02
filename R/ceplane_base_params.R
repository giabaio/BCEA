
#
ceplane_base_params <- function(he,
                                comparison,
                                wtp,
                                graph_params) {
  axes_params <-
    ceplane_axes_params(he,
                        comparison,
                        wtp,
                        graph_params)
  
  plot_params <-
    ceplane_plot_params(he,
                        comparison,
                        graph_params)
  
  modifyList(plot_params, axes_params)
}

