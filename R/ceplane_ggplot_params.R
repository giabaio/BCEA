
#' ceplane_ggplot_params
#' 
#' @import ggplot2
#'
ceplane_ggplot_params <- function(he,
                                  wtp,
                                  pos_legend,
                                  graph_params,
                                  ...) {
  
  ext_params <- ceplane_geom_params(...)
  
  graph_params$area <-
    modifyList(polygon_params(graph_params, wtp),
               graph_params$area)
  
  graph_params$wtp <- k_text(graph_params, wtp)
  
  graph_params$legend <- make_legend_ggplot(he, pos_legend)
  
  default_params <-
    list(
      size = rel(3.5),         # relative size
      wtp = list(
        label.pos = TRUE,
        geom = "text",
        hjust = 0.15,
        label = paste0("k = ", format(wtp, digits = 6), " "),
        size = 5,
        colour = "black"),
      icer = list(
        data = data.frame(x = colMeans(he$delta_e),
                          y = colMeans(he$delta_c)),
        mapping = aes(x = x, y = y),
        color = "red",
        size = 3,
        inherit.aes = FALSE),
      point = list(
        sizes = 4),
      line = list(
        x = graph_params$area$x[1:2],
        y = graph_params$area$y[1:2],
        geom = "line",
        color = "black"),
      area = list(
        geom = "polygon",
        fill = graph_params$area$color,
        alpha = ifelse(ext_params$area$include, 0.8, 0),
        data = data.frame(x = graph_params$area$x,
                          y = graph_params$area$y),
        mapping = aes(x = x, y = y),
        inherit.aes = FALSE)
    )
  
  modifyList(default_params,
             graph_params) %>% 
  modifyList(ext_params)
}

