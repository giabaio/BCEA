
#' ceplane_ggplot_params
#' 
#' @import ggplot2
#'
ceplane_ggplot_params <- function(he, graph_params, ...) {
  
  extra_params <- list(...)
  
  # extract separate parameter sets
  icer_params <-
    extra_params[
      names(extra_params) %in% c("ICER_sizes", "ICER_colors")]
  names(icer_params) <- gsub("ICER_", "", names(icer_params))
  
  point_params <-
    extra_params[
      names(extra_params) %in% c("point_sizes", "point_colors")]
  names(point_params) <- gsub("point_", "", names(point_params))
  
  polygon_params <-
    extra_params[
      names(extra_params) %in% c("area_include", "area_color")]
  names(area_params) <- gsub("area_", "", names(area_params))
  
  wtp_params <-
    extra_params[
      names(extra_params) %in% "label.pos"]
  
  plot_params <- list(
    icer = icer_params,
    point = point_params,
    area = polygon_params,
    wtp = wtp_params)
  
  graph_params$area <-
    modifyList(graph_params$area,
               polygon_params(graph_params, wtp))
  
  graph_params$k_text <- k_text(graph_params, wtp)
  
  
  default_params <-
    list(
      size = rel(3.5),         # relative size
      wtp = list(
        label.pos = TRUE,
        geom = "text",
        hjust = 0.15,
        label = paste0("k = ", format(wtp, digits = 6), " "),
        x = graph_params$k_text$x,
        y = graph_params$k_text$y,
        size = 5,
        colour = "black"),
      icer = list(
        data = data.frame(x = colMeans(he$delta_e),
                          y = colMeans(he$delta_c)),
        mapping = aes(x = x, y = y),
        colors = "red",
        sizes = 4,
        inherit.aes = FALSE),
      point = list(
        sizes = 4,
        colors = graph_params$points$colors),
      line = list(
        x = graph_params$area$x[1:2],
        y = graph_params$area$y[1:2],
        geom = "line",
        color = "black"),
      area = list(
        geom = "polygon",
        # fill = graph_params$area$col,
        color = graph_params$area$col,
        alpha = 0.3,
        data = data.frame(x = graph_params$area$x,
                          y = graph_params$area$y),
        mapping = aes(x = x, y = y),
        inherit.aes = FALSE)#,
      # legend = list(
      #   legend.direction = legend_dir,
      #   legend.justification = legend_just,
      #   legend.position = legend_pos)
    )
  
  modifyList(default_params, plot_params)
  
}

