
#' @name ceef_plot_graph
#' @title Cost-effectiveness Efficiency Frontier Plot By Graph Device
#'
#' @description Choice of base R, \pkg{ggplot2}.
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
  compLabels <- frontier_data$compLabels
  
  color <- frontier_params$color
  pos <- frontier_params$pos
  flip <- frontier_params$flip
  
  add_dominance_region <- frontier_params$dominance
  add_frontier <- dim(ceef.points)[1] > 1
  
  extra_args <- list(...)
  
  opt_theme <- Filter(f = \(val) ggplot2::is_theme(val), x = extra_args)
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
    geom_hline(yintercept = 0, color = "grey") +
    geom_vline(xintercept = 0,
               color = "grey") +
    geom_point(data = scatter.data,
               aes(x = .data$e, y = .data$c, color = .data$comp),
               size = 1)
  
  if (add_frontier)
    ceplane <- ceplane + geom_path()
  
  xlab <- "Effectiveness"
  ylab <- "Cost"
  
  comparators <- sort(c(he$comp, he$ref))
  
  ### add circles
  ceplane <- ceplane +
    geom_point(
      data = orig.avg,
      aes(x = .data$e.orig, y = .data$c.orig),
      size = 5.5,
      color = "black") +
    geom_point(
      data = orig.avg,
      aes(x = .data$e.orig, y = .data$c.orig),
      size = 4.5,
      color = "white") +
    scale_color_manual(
      "",
      labels = paste(comparators, ":", compLabels),
      values = color,
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
        color = ifelse(orig.avg[i,"comp"] %in% ceef.points$comp, "black", "grey60"))
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
      legend.text = element_text(hjust = 0),
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
#' @title CEEF plot plotly version
#'
#' @import ggplot2
#' @importFrom plotly add_trace layout plot_ly add_lines config
#' @importFrom grid unit
#' @importFrom grDevices gray
#'
ceef_plot_plotly <- function(he,
                             frontier_data,
                             frontier_params,
                             ...) {
  
  scatter.data <- frontier_data$scatter.data
  ceef.points <- frontier_data$ceef.points
  orig.avg <- frontier_data$orig.avg
  compLabels <- frontier_data$compLabels
  
  scatter.data$intervention <- factor(
    scatter.data$comp,
    labels = paste0(
      scatter.data$comp |> unique() |> sort(decreasing = TRUE), ": ",
      he$interventions[scatter.data$comp |> unique() |> sort(decreasing = TRUE)]),
    ordered = TRUE
  )
  
  color <- frontier_params$color
  color_hex = color |> sapply(function(x) gsub("gr(a|e)y", "", x) |> as.numeric()/100) |> grDevices::gray(1)
  pos <- frontier_params$pos
  flip <- frontier_params$flip
  
  add_dominance_region <- frontier_params$dominance
  add_frontier <- dim(ceef.points)[1] > 1
  
  extra_args <- list(...)
  
  opt_theme <- purrr::keep(extra_args, is_theme)
  
  if (flip) {
    names(ceef.points) = c(names(ceef.points)[c(2,1)], names(ceef.points)[-c(1,2)])
    names(scatter.data) = c(names(scatter.data)[c(2,1)], names(scatter.data)[-c(1,2)])
    names(orig.avg) = c(names(orig.avg)[c(2,1)], names(orig.avg)[-c(1,2)])
  }
  
  ceplane <- plotly::plot_ly()
  
  if (add_dominance_region) {
    shapes_list = lapply(
      1:nrow(ceef.points), function(ndx) {
        list(
          type = "rect", fillcolor = "lightgrey", opacity = 0.35,
          line = list(color = "lightgrey"),
          x0 = -2*max(abs(range(scatter.data$e))),
          x1 = ceef.points[ndx, "x"],
          y0 = ceef.points[ndx, "y"],
          y1 = 2*max(abs(range(scatter.data$c))),
          xref = "x", yref = "y"
        )
      }
    )
    ceplane <- ceplane |>
      plotly::layout(
        shapes = shapes_list
      )
  }
  
  # point clouds
  ceplane <- ceplane |>
    plotly::add_trace(
      data = scatter.data,
      type = "scatter",
      mode = "markers",
      y = ~c,
      x = ~e,
      color = ~intervention,
      colors = color_hex
    )
  
  # frontier
  if (add_frontier)
    ceplane <- ceplane |>
    plotly::add_lines(
      name = "Efficiency frontier",
      x = ceef.points$x,
      y = ceef.points$y,
      line = list(color = "black")
    )
  
  # circles
  ceplane <- ceplane |>
    plotly::add_trace(
      name = "Efficient interventions",
      type = "scatter",
      mode = "markers+text",
      data = orig.avg[orig.avg$comp %in% ceef.points$comp, ],
      x = ~e.orig,
      y = ~c.orig,
      marker = list(size = 20, fillcolor = "white", color = "white",
                    line = list(width = 1, color = "black")),
      text = ~comp, 
      textfont = list(color = "black")
    ) |>
    plotly::add_trace(
      name = "Inefficient interventions",
      type = "scatter",
      mode = "markers+text",
      data = orig.avg[!orig.avg$comp %in% ceef.points$comp, ],
      x = ~e.orig,
      y = ~c.orig,
      marker = list(size = 20, fillcolor = "white", color = "white",
                    line = list(width = 1, color = "grey")),
      text = ~comp,
      textfont = list(color = "grey")
    )
  
  legend_params = make_legend_plotly(pos)
  
  xaxis_list = list(
    hoverformat = ".2f",
    title = ifelse(
      !flip,
      "Effectiveness",
      "Cost"
    ),
    range = 
      scatter.data[,ifelse(!flip,1,2)] |> range() |> 
      (\(x) c(x[1] - abs(x[1] - x[2])*0.15, x[2] + abs(x[1] - x[2])*0.15))()
  )
  
  yaxis_list = yaxis = list(
    hoverformat = ".2f",
    title = ifelse(
      !flip,
      "Cost",
      "Effectiveness"
    ),
    range = 
      scatter.data[,ifelse(!flip,2,1)] |> range() |> 
      (\(x) c(x[1] - abs(x[1] - x[2])*0.15, x[2] + abs(x[1] - x[2])*0.15))()
  )
  
  ceplane <- ceplane |>
    plotly::layout(
      title = "Cost-effectiveness efficiency frontier",
      xaxis = xaxis_list,
      yaxis = yaxis_list,
      legend = legend_params
    ) |>
    plotly::config(displayModeBar = FALSE)
  
  return(ceplane)
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
  
  color <- frontier_params$color
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
  xlab <- if (!flip) "Effectiveness" else "Cost"
  ylab <- if (!flip) "Cost" else "Effectiveness"
  plot(NULL,
       xlim = c(min(range(scatter.data$e)[1],0), max(range(scatter.data$e)[2],0)),
       ylim = c(min(range(scatter.data$c)[1],0), max(range(scatter.data$c)[2],0)),
       main = "Cost-effectiveness efficiency frontier",
       xlab = xlab,
       ylab = ylab)
  
  if (dominance) {
    # add dominance regions
    for (i in seq_len(dim(ceef.points)[1])) {
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
           col = color[i])
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
         col = ifelse(orig.avg[i, "comp"] %in% ceef.points$comp, "black", "grey60"),
         cex = 0.75)
  }
  
  comparators <- sort(c(he$comp, he$ref))
  text <- paste(comparators, ":", he$interventions[comparators])
  legend(pos, text, col = color, cex = 0.7, bty = "n", lty = 1)
  
  # because dominance areas overwrite outer box
  box()
}

