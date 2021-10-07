
#' @name ceef_plot_graph
#' @title Cost-effectiveness efficiency frontier plot
#'
#' Choice of base R, ggplot2.
#' 
#' @template args-he
#' @param frontier_data Frontier data
#' @param frontier_params Frontier parameters
#' @param ... Additional arguments
NULL


#' @rdname ceef_plot_graph
#' @title CEEF plot ggplot2 version
#' 
#' @import ggplot2
#' @importFrom grid unit
#' 
ceef_plot_ggplot <- function(he,
                             frontier_data,
                             frontier_params,
                             ...) {
  
  scatter.data <- frontier_data$scatter.data
  ceef.points <- frontier_data$ceef.points 
  orig.avg <- frontier_data$orig.avg
  
  colour <- frontier_params$colour
  pos <- frontier_params$pos
  flip <- frontier_params$flip
  relative  <- frontier_params$relative
  
  add_dominance_region <- frontier_params$dominance
  add_frontier <- dim(ceef.points)[1] > 1

  extra_args <- list(...)
  
  opt_theme <- purrr::keep(extra_args, is.theme)
  
  ceplane <- ggplot(ceef.points, aes(x = .data$x, y = .data$y))
  
  if (add_dominance_region) {
    ceplane <-
      ceplane +
      geom_rect(data = ceef.points,
                aes(xmax = .data$x, ymin = .data$y),
                ymax = 2*max(abs(range(scatter.data$c))),
                xmin = -2*max(abs(range(scatter.data$e))),
                alpha = 0.35,
                fill = "grey75")
  }
  
  ceplane <- ceplane +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0,
               colour = "grey") +
    geom_point(data = scatter.data,
               aes(x = .data$e, y = .data$c, colour = .data$comp),
               size = 1)
  
  if (add_frontier)
    ceplane <- ceplane + geom_path()
  
  xlab <- ifelse(!relative, "Effectiveness", "Effectiveness differential")
  ylab <- ifelse(!relative, "Cost", "Cost differential")
  
  comparators <- sort(c(he$comp, he$ref))
  
  ### add circles
  ceplane <- ceplane +
    geom_point(
      data = orig.avg,
      aes(x = .data$e.orig, y = .data$c.orig),
      size = 5.5,
      colour = "black") +
    geom_point(
      data = orig.avg,
      aes(x = .data$e.orig, y = .data$c.orig),
      size = 4.5,
      colour = "white") +
    scale_colour_manual(
      "",
      labels = paste(comparators, ":", he$interventions[comparators]),
      values = colour,
      na.value = "black") +
    labs(title = "Cost-effectiveness efficiency frontier",
         x = xlab, y = ylab) +
    theme_bw()
  
  ## add text into circles
  for (i in seq_len(he$n_comparators)) {
    ceplane <- ceplane + 
      geom_text(
        data = orig.avg[i, ],
        aes(x = .data$e.orig, y = .data$c.orig, label = .data$comp),
        size = 3.5,
        colour = ifelse(i %in% ceef.points$comp, "black", "grey60"))
  }
  
  ##TODO: test...
  legend_params <- make_legend_ggplot(he, pos)
  
  ##TODO: include legend direction?
  ceplane <- ceplane + 
    theme(
      legend.position = legend_params$legend.position,
      legend.justification = legend_params$legend.postion,
      legend.title = element_blank(),
      legend.background = element_blank(),
      text = element_text(size = 11),
      legend.key.size = grid::unit(0.66, "lines"),
      legend.spacing = grid::unit(-1.25, "line"),
      panel.grid = element_blank(),
      legend.key = element_blank(),
      legend.text.align = 0,
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        lineheight = 1.05,
        size = 14.3)) +
    opt_theme
  
  if (flip) ceplane <- ceplane + coord_flip()
  
  ceplane
}

