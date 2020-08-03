
#
ceplane_base_params <- function(he,
                                wtp,
                                graph_params) {
  
  c(list(
    setup = setup_params(graph_params),
    points = points_params(graph_params),
    polygon = polygon_params(graph_params, wtp),
    k_txt = k_text(graph_params, wtp),
    wtp = wtp),
    icer_params(graph_params, he))
}

