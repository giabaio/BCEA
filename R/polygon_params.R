
polygon_params <- function(graph_params) {
  
  x_min <- graph_params$xlim[1]
  x_max <- graph_params$xlim[2]
  y_min <- graph_params$ylim[1]
  y_max <- graph_params$ylim[2]
  wtp <- graph_params$wtp
  
  box_adj <- 1.5
  polygon_x <- c(y_min/wtp, x_max, x_max)*box_adj
  polygon_y <- c(y_min, x_max*wtp, y_min)*box_adj
  
  list(x = polygon_x,
       y = polygon_y,
       col = graph_params$area$color,
       border = graph_params$area$line_color)
}