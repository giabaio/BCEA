
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
  
  # same order and length
  
  param_names <- union(names(axes_params),
                       names(plot_params))
  
  empty_list <-
    vector("list", length = length(param_names))
  
  names(empty_list) <- param_names
  
  axes_params <- 
    modifyList(empty_list, axes_params)
  
  plot_params <- 
    modifyList(empty_list, plot_params)
  
  # merge lists
  mapply(FUN = c,
         plot_params,
         axes_params,
         SIMPLIFY = FALSE)
}

