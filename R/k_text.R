
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

