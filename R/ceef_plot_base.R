
#' ceef_plot_base
#' 
ceef_plot_base <- function(frontier_data,
                           frontier_params) {
  
  scatter.data <- frontier_data$scatter.data
  ceef.points <- frontier_data$ceef.points 
  orig.avg <- frontier_data$orig.avg
  
  colour <- frontier_params$colour
  pos <- frontier_params$pos
  flip <- frontier_params$flip
  relative  <- frontier_params$relative 
  dominance <- frontier_params$dominance
  
  pos <- where_legend(he, pos)
  
  if (flip){
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
  
  ### set up plot window
  xlab <- ifelse((!flip & !relative), "Effectiveness",
                 ifelse((!flip & relative), "Differential effectiveness",
                        ifelse((flip & !relative),
                               "Cost", "Differential cost")))
  ylab <- ifelse((!flip & !relative),"Cost",
                 ifelse((!flip & relative),"Differential cost",
                        ifelse((flip & !relative),
                               "Effectiveness", "Differential effectiveness")))
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
  
  # plot the scatter
  for (i in 1:he$n_comparators)
    with(scatter.data,
         points(subset(scatter.data, comp == i)[, -3],
                type = "p",
                pch = 20,
                cex = 0.35,
                col = colour[i]))
  
  # plot the frontier
  points(ceef.points[, 1:2], type = "l", lwd = 2)
  
  # add circles
  points(orig.avg[, -3], pch = 21, cex = 2, bg = "white", col = "black")
  
  # add text; grey if not on the frontier
  for (i in seq_len(he$n_comparators)) {
    text(orig.avg[i, -3],
         labels = orig.avg[i, 3],
         col = ifelse(i %in% ceef.points$comp,"black","grey60"),
         cex = 0.75)
  }
  
  # legend text
  text <- paste(1:he$n_comparators, ":", he$interventions)
  legend(pos, text, col = colour, cex = 0.7, bty = "n", lty = 1)
  
  # needed for dominance areas overwriting the outer box
  box()
}

