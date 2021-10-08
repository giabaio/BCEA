
#' @rdname ceplane_plot_graph
#'
#' @return For ggplot2 returns ggplot2 object
#' 
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom purrr keep
#' 
#' @keywords hplot
#' @export
#' 
#' @examples
#' 
#' data(Vaccine)
#' he <- bcea(e, c)
#' 
#' ceplane.plot(he, graph = "ggplot2")
#' 
#' data(Smoking)
#' he <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)
#' 
#' ceplane.plot(he, graph = "ggplot2")
#' 
#' ceplane.plot(he,
#'              wtp = 200,
#'              pos = "right",
#'              ICER_size = 2,
#'              graph = "ggplot2")
#'    
#' ceplane.plot(he,
#'              wtp = 200,
#'              pos = TRUE,
#'              graph = "ggplot2")
#'
#' ceplane.plot(he,
#'              graph = "ggplot2",
#'              wtp=200,
#'              theme = ggplot2::theme_linedraw())
#'              
ceplane_plot_ggplot.bcea <- function(he,
                                     wtp = 25000,
                                     pos_legend,
                                     graph_params, ...) {
  
  # single long format for ggplot data
  delta_ce <-
    merge(
      melt(
        cbind(sim = seq_len(nrow(he$delta_c)),
              he$delta_c),
        variable.name = "comparison",
        value.name = "delta_c",
        id.vars = "sim"),
      melt(
        cbind(sim = seq_len(nrow(he$delta_e)),
              he$delta_e),
        variable.name = "comparison",
        value.name = "delta_e",
        id.vars = "sim"),
      by = c("sim", "comparison"))
  
  graph_params <-
    ceplane_ggplot_params(he, wtp, pos_legend, graph_params, ...)
  
  theme_add <- purrr::keep(list(...), is.theme)
  
  ggplot(delta_ce,
         aes(x = .data$delta_e, y = .data$delta_c, col = .data$comparison)) +
    do.call(geom_polygon, graph_params$area) +
    theme_ceplane() +
    theme_add +
    geom_point(size = graph_params$point$size) +
    scale_color_manual(labels = line_labels.default(he),
                       values = graph_params$point$colors) +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0, colour = "grey") +
    coord_cartesian(xlim = graph_params$xlim,
                    ylim = graph_params$ylim,
                    expand = FALSE) +
    do.call(labs,
            list(title = graph_params$title,
                 x = graph_params$xlab,
                 y = graph_params$ylab)) +
    do.call(geom_abline, list(slope = wtp,
                              col = graph_params$line$color)) +
    do.call(geom_point, graph_params$icer) +
    do.call(annotate, graph_params$wtp) +
    do.call(annotate, graph_params$icer_txt) +
    do.call(theme, graph_params$legend)
}


#' @rdname ceplane_plot_graph
#' @export
#' 
ceplane_plot_ggplot <- function(he, ...) {
  UseMethod('ceplane_plot_ggplot', he)
}

