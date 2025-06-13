
#' CE-plane ggplot Parameters
#' 
#' @template args-he
#' @param pos_legend Position of legend
#' @param graph_params Other graphical parameters
#' @param ... Additional arguments
#'
#' @import ggplot2
#' @keywords internal
#' 
ceplane_ggplot_params <- function(he,
                                  pos_legend,
                                  graph_params,
                                  ...) {
  ext_params <- ceplane_geom_params(...)
  poly_params <- polygon_params(graph_params)
  # use color instead of col with ggplot vs base plot
  poly_params$color <- poly_params$col
  poly_params$col <- NULL
  
  graph_params$area <- modifyList(poly_params, graph_params$area)
  graph_params$legend <- make_legend_ggplot(he, pos_legend)
  
  default_params <-
    list(
      size = rel(3.5),
      wtp = list(
        geom = "text",
        x = graph_params$xlim[1],
        y = ifelse(graph_params$label.pos,
                   max(graph_params$xlim[1] * graph_params$wtp_value, graph_params$ylim[1]),
                   graph_params$ylim[1]),
        hjust = "inward",
        vjust = "inward",
        size = convert_pts_to_mm(1),
        color = "black"),
      icer = list(
        data = data.frame(x = colMeans(he$delta_e),
                          y = colMeans(he$delta_c)),
        mapping = aes(x = .data$x, y = .data$y),
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
        shape = rep(19, he$n_comparisons),
        size = 4),
      line = list(
        color = "black"),
      text = list(
        size =
          if (is.rel(graph_params$text$size)) {
            11 * unclass(graph_params$text$size)  # theme_get()$text$size
          } else {
            graph_params$text$size
          }
      ),
      area = list(
        fill = graph_params$area$col,
        include = ext_params$area_include,
        data = data.frame(x = graph_params$area$x,
                          y = graph_params$area$y),
        mapping = aes(x = .data$x, y = .data$y),
        inherit.aes = FALSE),
      currency = "",
      icer_annot = FALSE)
  
  modifyList(default_params, graph_params) |>
    modifyList(ext_params)
}

