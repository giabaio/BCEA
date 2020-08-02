
#
setup_params <- function(he,
                         comparison,
                         graph_params) {
  
  ##TODO: drop = FALSE in delta_x creation in bcea()
  e_dat <- as.matrix(he$delta_e)[, comparison]
  c_dat <- as.matrix(he$delta_c)[, comparison]
  
  min_e <- min(e_dat)
  max_e <- max(e_dat)
  
  min_c <- min(c_dat)
  max_c <- max(c_dat)
  
  # force negative
  min_e <- -abs(min_e)
  min_c <- -abs(min_c)
  
  # square plotting area
  min_e <- min(min_e, min_c/wtp)
  max_e <- max(max_e, max_c/wtp)
  
  min_c <- min_e*wtp
  max_c <- max_e*wtp
  
  check_provided_xlim <- all(!is.null(graph_params$xlim))
  check_provided_ylim <- all(!is.null(graph_params$ylim))
  
  if (check_provided_xlim) {
    min_e <- graph_params$xlim[1]
    max_e <- graph_params$xlim[2]
  }
  if (check_provided_ylim) {
    min_c <- graph_params$ylim[1]
    max_c <- graph_params$ylim[2]
  }
  
  list(x = NULL,
       axes = FALSE,
       xlim = c(min_e, max_e),
       ylim = c(min_c, max_c),
       xlab = graph_params$xlab,
       ylab = graph_params$ylab,
       main = graph_params$title)
}


#
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


#
points_params <- function(graph_params) {
  
  list(pch = 20,
       cex = graph_params$point$sizes,
       col = graph_params$point$colors)
}


#
k_text <- function(graph_params) {
  
  x_k <- graph_params$xlim[1]
  y_k <- max(x_k*graph_params$wtp,
             graph_params$ylim[1])
  
  list(cex = 0.8,
       pos = 4, 
       adj = c(0, 0),
       x = x_k,
       y = y_k)
}


#
icer_params <- function(graph_params,
                        he,
                        comparison) {
  
  list(icer_text =
         list(labels = icer_label(he, comparison),
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
icer_label <- function(he, comparison) {
  
  if (length(comparison) == 1) {
    return(
      paste0("\U2022",
             " ICER = ",
             format(he$ICER[comparison],
                    digits = 6,
                    nsmall = 2)))
  } 
  return("")
}




