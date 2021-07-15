
#' ceef_plot_ggplot
#' 
#' @import ggplot2 grid
#' 
ceef_plot_ggplot <- function(he,
                             ceef.points,
                             scatter.data,
                             exArgs,
                             dominance,
                             relative,
                             orig.avg,
                             colour,
                             flip,
                             pos, ...) {
  
  if(!(requireNamespace("ggplot2", quietly = TRUE) &&
       requireNamespace("grid", quietly = TRUE))){
    message("Falling back to base graphics\n")
    
    ceef.plot(
      he,
      flip = flip,
      comparators = comparators,
      pos = pos,
      start.from.origins = start.from.origins,
      graph = "base")
    return(invisible(NULL))
  }
  
  opt.theme <- theme()
  exArgs <- list(...)
  
  if (length(exArgs) >= 1) {
    for (obj in exArgs)
      if (is.theme(obj))
        opt.theme <- opt.theme + obj
  }
  
  ceplane <- ggplot(ceef.points, aes(x=x,y=y))
  
  if (dominance) {
    ### add dominance regions
    ceplane <-
      ceplane +
      geom_rect(data = ceef.points,
                aes(xmax = x, ymin = y),
                ymax = 2*max(abs(range(scatter.data$c))),
                xmin = -2*max(abs(range(scatter.data$e))),
                alpha = 0.35,
                fill = "grey75")
  }
  
  ceplane <- ceplane +
    ### draw axes
    geom_hline(yintercept = 0, colour = "grey") +
    geom_vline(xintercept = 0,
               colour = "grey") +
    ### add scatter points
    geom_point(data = scatter.data,
               aes(x = e, y = c, colour = comp),
               size = 1)
  ### add frontier
  if (dim(ceef.points)[1] > 1)
    ceplane <- ceplane + geom_path()
  
  ### add circles
  xlab = ifelse(!relative, "Effectiveness", "Effectiveness differential")
  ylab = ifelse(!relative, "Cost", "Cost differential")
  
  ceplane <- ceplane +
    geom_point(
      data = orig.avg,
      aes(x = e.orig, y = c.orig),
      size = 5.5,
      colour = "black") +
    geom_point(
      data = orig.avg,
      aes(x = e.orig, y = c.orig),
      size = 4.5,
      colour = "white") +
    ### set graphical parameters
    scale_colour_manual(
      "",
      labels = paste0(1:he$n_comparators, ": ", he$interventions),
      values = colour,
      na.value = "black") +
    labs(title = "Cost-effectiveness efficiency frontier", x =
                    xlab, y = ylab) +
    theme_bw()
  
  ### add text into circles
  for (i in 1:he$n_comparators) {
    ceplane <- ceplane + 
      geom_text(
        data = orig.avg[i, ],
        aes(x = e.orig, y = c.orig, label = comp),
        size = 3.5,
        colour = ifelse(i %in% ceef.points$comp, "black", "grey60"))
  }
  
  jus <- NULL
  if (isTRUE(pos)) {
    pos <- "bottom"
    ceplane <- ceplane + theme(legend.direction="vertical")
  } else {
    if (is.character(pos)) {
      choices <- c("left", "right", "bottom", "top")
      pos <- choices[pmatch(pos,choices)]
      jus <- "center"
      if (is.na(pos))
        pos <- FALSE
    }
    if (length(pos) > 1)
      jus <- pos
    if (length(pos) == 1 & !is.character(pos)) {
      pos <- c(1,1)
      jus <- pos
    }
  }
  
  ceplane <- ceplane + 
    theme(
      legend.position = pos,
      legend.justification = jus,
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
    opt.theme
  
  if (flip) ceplane <- ceplane + coord_flip()
  
  ceplane
}

