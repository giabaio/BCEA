
#
ceplane_base_params <- function(he,
                                wtp,
                                graph_params) {
  
  list(
    setup = setup_params(graph_params),
    points = points_params(graph_params),
    polygon = polygon_params(graph_params),
    k_txt = k_text(graph_params),
    icer_params(graph_params, he),
    wtp = wtp)
}

