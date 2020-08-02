
#' ceplane_axes_params
#'
#' @template args-he 
#' @param comparison Interventions to compare against reference
#' @param wtp Willingness to pay threshold
#' @param graph_params Graph parameters
#'
#' @return
#' @export
#' @keywords dplot
#' 
#' @examples
#' 
ceplane_axes_params <- function(he,
                                comparison,
                                wtp,
                                graph_params) {
  
  # axes bounds
  ##TODO: drop = FALSE in delta_x creation in bcea()
  min_e <- range(as.matrix(he$delta_e)[, comparison])[1]
  max_e <- range(as.matrix(he$delta_e)[, comparison])[2]
  min_c <- range(as.matrix(he$delta_c)[, comparison])[1]
  max_c <- range(as.matrix(he$delta_c)[, comparison])[2]
  
  # force negative
  min_e <- -abs(min_e)
  min_c <- -abs(min_c)
  
  # square plotting area
  min_e <- min(min_e, min_c/wtp)
  max_e <- max(max_e, max_c/wtp)
  
  min_c <- min_e*wtp
  max_c <- max_e*wtp

  if (all(!is.null(graph_params$xlim))) {
    min_e <- graph_params$xlim[1]
    max_e <- graph_params$xlim[2]
  }
  if (all(!is.null(graph_params$ylim))) {
    min_c <- graph_params$ylim[1]
    max_c <- graph_params$ylim[2]
  }
  
  box_adj <- 1.5
  polygon_x <- c(min_c/wtp, max_e, max_e)*box_adj
  polygon_y <- c(min_c, max_e*wtp, min_c)*box_adj
  
  x_k <- min_e
  y_k <- max(x_k*wtp, min_c)
  
  list(k_txt =
         list(x = x_k,
              y = y_k),
       limits =
         list(xlim = c(min_e, max_e),
              ylim = c(min_c, max_c)),
       icer_text =
         list(x = max_e,
              y = max_c),
       polygon =
         list(x = polygon_x,
              y = polygon_y),
       wtp = wtp)
}
