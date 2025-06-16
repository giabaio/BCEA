
#' @keywords dplot
#' 
ceplane_base_params <- function(he,
                                graph_params) {
  
  c(list(
    setup = setup_params(graph_params),
    points = points_params(graph_params),
    polygon = polygon_params(graph_params),
    k_txt = k_text(graph_params, graph_params$wtp_value),
    wtp = graph_params$wtp_value,
    ref_first = graph_params$ref_first),
    icer_params(graph_params, he))
}


#' @keywords dplot
#' 
contour_base_params <- function(he,
                                graph_params) {

  c(list(
    setup = setup_params(graph_params),
    points = points_params(graph_params),
    quadrants = quadrant_params(he, graph_params),
    scale = graph_params$scale,
    levels = graph_params$levels,
    contour = graph_params$contour,
    nlevels = graph_params$nlevels,
    ref_first = graph_params$ref_first),
    icer_params(graph_params, he))
}
