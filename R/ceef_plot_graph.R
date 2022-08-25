
#' @name ceef_plot_graph
#' @title Cost-effectiveness Efficiency Frontier Plot By Graph Device
#'
#' @description Choice of base R, ggplot2.
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
  relative <- frontier_params$relative
  
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
  
  xlab <- ifelse(!relative, "Effectiveness", "Incremental effectiveness")
  ylab <- ifelse(!relative, "Cost", "Incremental cost")
  
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
  
  legend_params <- make_legend_ggplot(he, pos)
  
  ceplane <- ceplane +
    theme(
      legend.position = legend_params$legend.position,
      legend.justification = legend_params$legend.justification,
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

#' @rdname ceef_plot_graph
#' @title CEEF plot base R version
#' 
ceef_plot_base <- function(he,
                           frontier_data,
                           frontier_params) {
  
  scatter.data <- frontier_data$scatter.data
  ceef.points <- frontier_data$ceef.points 
  orig.avg <- frontier_data$orig.avg
  
  colour <- frontier_params$colour
  pos <- frontier_params$pos
  flip <- frontier_params$flip
  relative  <- frontier_params$relative 
  dominance <- frontier_params$dominance
  
  pos <- where_legend_always(he, pos)
  
  if (flip) {
    temp <- scatter.data$e
    scatter.data$e <- scatter.data$c
    scatter.data$c <- temp
    
    temp <- ceef.points$x
    ceef.points$x <- ceef.points$y
    ceef.points$y <- temp
    
    temp <- orig.avg$e.orig
    orig.avg$e.orig <- orig.avg$c.orig
    orig.avg$c.orig <- temp
    
    rm(temp)
  }
  
  # set up plot window
  xlab <- ifelse((!flip & !relative), "Effectiveness",
                 ifelse((!flip & relative), "Incremental effectiveness",
                        ifelse((flip & !relative),
                               "Cost", "Incremental cost")))
  ylab <- ifelse((!flip & !relative), "Cost",
                 ifelse((!flip & relative), "Incremental cost",
                        ifelse((flip & !relative),
                               "Effectiveness", "Incremental effectiveness")))
  plot(NULL,
       xlim = c(min(range(scatter.data$e)[1],0), max(range(scatter.data$e)[2],0)),
       ylim = c(min(range(scatter.data$c)[1],0), max(range(scatter.data$c)[2],0)),
       main = "Cost-effectiveness efficiency frontier",
       xlab = xlab,
       ylab = ylab)
  
  if (dominance) {
    # add dominance regions
    for (i in 1:dim(ceef.points)[1]) {
      rect(
        col = "grey95",
        border = NA,
        xleft = ifelse(!flip, -1, 1) * 2 * max(abs(range(scatter.data$e))),
        xright = ceef.points$x[i],
        ybottom = ceef.points$y[i],
        ytop = ifelse(!flip, 1, -1) * 2 * max(abs(range(scatter.data$c)))
      )
    }
    if (dim(ceef.points)[1] > 1)
      for (i in 2:dim(ceef.points)[1]) {
        rect(
          col = "grey85",
          border = NA,
          xleft = ifelse(!flip, -1, 1) * 2 * max(abs(range(scatter.data$e))),
          xright = ceef.points$x[ifelse(!flip, i - 1, i)],
          ybottom = ceef.points$y[ifelse(!flip, i, i - 1)],
          ytop = ifelse(!flip, 1, -1) * 2 * max(abs(range(scatter.data$c)))
        )
      }
  }
  
  abline(h = 0, v = 0, col = "grey")
  
  ##TODO:
  # plot the scatter
  # matplot()?
  # will need to add sim number column to cast
  # do this in prep_frontier_data()
  
  comparators <- unique(scatter.data$comp)
  
  for (i in seq_len(he$n_comparators)) {
    sub_scatter <-
      dplyr::filter(scatter.data,
                    .data$comp == comparators[i])
    
    points(sub_scatter[, c("e", "c")],
           type = "p",
           pch = 20,
           cex = 0.35,
           col = colour[i])
  }
  
  ##TODO: why are these two separate arrays?
  # add frontier
  points(ceef.points[, c("x", "y")], type = "l", lwd = 2)
  
  # add circles
  points(orig.avg[, c("e.orig", "c.orig")],
         pch = 21, cex = 2, bg = "white", col = "black")
  
  ### legend
  # add text; grey if not on the frontier
  for (i in seq_len(he$n_comparators)) {
    text(orig.avg[i, c("e.orig", "c.orig")],
         labels = orig.avg[i, 3],
         col = ifelse(i %in% ceef.points$comp, "black", "grey60"),
         cex = 0.75)
  }
  
  comparators <- sort(c(he$comp, he$ref))
  text <- paste(comparators, ":", he$interventions[comparators])
  legend(pos, text, col = colour, cex = 0.7, bty = "n", lty = 1)
  
  # because dominance areas overwrite outer box
  box()
}

