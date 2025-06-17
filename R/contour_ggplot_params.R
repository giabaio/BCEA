
#' Contour ggplot2 Parameters
#' 
#' @template args-he
#' @param pos_legend legend position
#' @param graph_params Other graphical parameters
#' @param ... Additional arguments
#'
#' @import ggplot2
#' @keywords internal
#' 
contour_ggplot_params <- function(he,
                                  pos_legend,
                                  graph_params,
                                  ...) {
  
  ext_params <- ceplane_geom_params(...)
  
  graph_params$legend <-
    make_legend_ggplot(he, pos_legend)
  
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
        aes_string(color = "comparison"),
        contour_var = "ndensity",
        adjust = 2,
        size = 1,
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
  
  params <- 
    modifyList(default_params,
               graph_params) %>% 
    modifyList(ext_params)
  
  params$quad_txt <-
    data.frame(
      x = c(params$xlim[2], params$xlim[1], params$xlim[1], params$xlim[2]),
      y = c(params$ylim[2], params$ylim[2], params$ylim[1], params$ylim[1]),
      label = c(params$quadrant$t1, params$quadrant$t2,
                params$quadrant$t3, params$quadrant$t4),
      hjust = c(1, 0, 0, 1))
  
  params
}

