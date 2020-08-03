
#
setup_params <- function(graph_params) {
  
  list(xlim = graph_params$xlim,
       ylim = graph_params$ylim,
       xlab = graph_params$xlab,
       ylab = graph_params$ylab,
       x = NULL,
       axes = FALSE,
       main = graph_params$title)
}


#
polygon_params <- function(graph_params, wtp) {
  
  x_min <- graph_params$xlim[1]
  x_max <- graph_params$xlim[2]
  y_min <- graph_params$ylim[1]
  y_max <- graph_params$ylim[2]
  
  box_adj <- 1.5
  polygon_x <- c(y_min/wtp, x_max, x_max)*box_adj
  polygon_y <- c(y_min, x_max*wtp, y_min)*box_adj
  
  list(x = polygon_x,
       y = polygon_y,
       col = graph_params$area$color,
       border = graph_params$area$line_color)
}


#
points_params <- function(graph_params) {
  
  list(pch = 20,
       cex = graph_params$point$sizes,
       col = graph_params$point$colors)
}


#
k_text <- function(graph_params, wtp) {
  
  x_k <- graph_params$xlim[1]
  y_k <- max(x_k*wtp, graph_params$ylim[1])
  
  list(cex = 0.8,
       pos = 4, 
       adj = c(0, 0),
       x = x_k,
       y = y_k)
}


#
icer_params <- function(graph_params, he) {
  
  list(icer_text =
         list(labels = icer_label(he),
              cex = 0.95,
              pos = 2,
              col = "red",
              x = graph_params$xlim[2],
              y = graph_params$ylim[2]),
       icer_points =
         list(pch = 20,
              col = "red",
              cex = 1))
}


#
icer_label <- function(he) {
  
  if (he$n_comparisons == 1) {
    return(
      paste0("\U2022",
             " ICER = ",
             format(
               mean(he$delta_c)/mean(he$delta_e),
               digits = 6,
               nsmall = 2)))
  }
  
  return("")
}




