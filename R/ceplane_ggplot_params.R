
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
  
  graph_params$legend <- make_legend_ggplot(he, pos_legend)
  
  default_params <-
    list(
      size = rel(3.5),         # relative size
      wtp = list(
        label.pos = TRUE,
        geom = "text",
        x = graph_params$xlim[1],
        y = graph_params$ylim[1],
        hjust = "inward",
        vjust = "inward",
        label = paste0("  k = ", format(wtp, digits = 6), "\n"),
        size = convert_pts_to_mm(1),
        colour = "black"),
      icer = list(
        data = data.frame(x = colMeans(he$delta_e),
                          y = colMeans(he$delta_c)),
        mapping = aes(x = x, y = y),
        color = "red",
        size = convert_pts_to_mm(0.8),
        inherit.aes = FALSE),
      icer_txt = list(
        geom = "text",
        label =
          ifelse(length(he$ICER) == 1,
                 paste0("\n", "\U2022",
                        " ICER = ",
                        format(he$ICER, digits = 6, nsmall = 2), "  "),
                 ""),
        x = graph_params$xlim[2],
        y = graph_params$ylim[2],
        col = "red",
        size = convert_pts_to_mm(1),
        hjust = "inward",
        vjust = "inward"),
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

