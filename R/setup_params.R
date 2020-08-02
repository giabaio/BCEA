
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

