
#' ceplane_ggplot_params
#' 
#' @import ggplot2
#'
ceplane_ggplot_params <- function(he,
                                  graph_params,
                                  ...) {
  
  ext_params <- geom_params(...)
  
  graph_params$area <-
    modifyList(polygon_params(graph_params, wtp),
               graph_params$area)
  
  graph_params$wtp <- k_text(graph_params, wtp)
  
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
        alpha = 0.8,
        data = data.frame(x = graph_params$area$x,
                          y = graph_params$area$y),
        mapping = aes(x = x, y = y),
        inherit.aes = FALSE)#,
      # legend = list(
      #   legend.direction = legend_dir,
      #   legend.justification = legend_just,
      #   legend.position = legend_pos)
    )
  
  modifyList(default_params,
             graph_params) %>% 
  modifyList(ext_params)
}

