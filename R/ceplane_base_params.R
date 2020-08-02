
#
ceplane_base_params <- function(he,
                                comparison,
                                wtp,
                                graph_params) {
  
  setup_new <- setup_params(he,
                            comparison,
                            graph_params)
  
  graph_params$xlim <- setup_new$xlim
  graph_params$ylim <- setup_new$ylim
  graph_params$wtp <- wtp
  
  c(list(
    setup = setup_new,
    points = points_params(graph_params),
    polygon = polygon_params(graph_params),
    k_txt = k_text(graph_params),
    wtp = wtp),
    icer_params(graph_params,
                he, comparison))
}

