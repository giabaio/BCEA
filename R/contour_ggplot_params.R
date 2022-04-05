
#' Contour ggplot Parameters
#' 
#' @template args-he
#' @param pos_legend Position of legend
#' @param graph_params Other graphical parameters
#' @param ... Additional arguments
#'
#' @import ggplot2
#' @keywords internal
#' 
contour_ggplot_params <- function(he,
                                  graph_params,
                                  ...) {
  
  ext_params <- ceplane_geom_params(...)
  
  graph_params$legend <-
    make_legend_ggplot(he, graph_params$pos_legend)
  
  if (!is.null(graph_params$nlevels)) {
    nlevels <- round(graph_params$nlevels)
    if (graph_params$nlevels < 0)
      nlevels <- 10
    if (graph_params$nlevels == 0)
      nlevels <- 1
  }
  
  default_params <-
    list(
      quadrant = quadrant_params(he, graph_params),
      size = rel(3.5),
      contour = list(
        aes_string(col = "comparison"),
        adjust = 2,
        size = 1,
        col = "black",
        bins = 5),
      icer = list(
        data = data.frame(x = colMeans(he$delta_e),
                          y = colMeans(he$delta_c)),
        mapping = aes(x = .data$x, y = .data$y),
        color = "red",
        size = convert_pts_to_mm(0.8),
        inherit.aes = FALSE),
      point = list(
        shape = rep(19, he$n_comparisons),
        size = 4),
      line = list(
        color = "black"))
  
  modifyList(default_params,
             graph_params) %>% 
    modifyList(ext_params)
}

