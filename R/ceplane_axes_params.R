
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
  
  box_adj <- 1.5
  polygon_x <- c(min_c/wtp, max_e, max_e)*box_adj
  polygon_y <- c(min_c, max_e*wtp, min_c)*box_adj
  
  x_k <- min_e
  y_k <- max(x_k*wtp, min_c)
  
  list(setup =
         list(xlim = c(min_e, max_e),
              ylim = c(min_c, max_c)),
       polygon =
         list(x = polygon_x,
              y = polygon_y),
       k_txt =
         list(x = x_k,
              y = y_k),
       icer_text =
         list(x = max_e,
              y = max_c),
       wtp = wtp)
}
