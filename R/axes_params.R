
#' axes_params
#'
#' @template args-he 
#' @param comparison
#' @param wtp
#'
#' @return
#' @export
#' @keywords dplot
#' 
#' @examples
#' 
axes_params <- function(he,
                        comparison,
                        wtp) {
  
  # upper and lower bounds
  lower_e <- range(he$delta_e[, comparison])[1]
  upper_e <- range(he$delta_e[, comparison])[2]
  lower_c <- range(he$delta_c[, comparison])[1]
  upper_c <- range(he$delta_c[, comparison])[2]
  
  step_size <- (upper_e - lower_e)/10
  
  lower_e <- -abs(lower_e)
  lower_c <- -abs(lower_c)
  
  x_k <- 0.95*lower_e
  y_k <- max(x_k*wtp, lower_c)
  
  x_seq <- seq(from = 100*lower_c/wtp,
               to = 100*upper_c/wtp,
               by = step_size)
  
  y_seq <- x_seq*wtp
  
  x_seq[1] <- ifelse(x_seq[1] < lower_e, x_seq[1], 2*lower_e)
  y_seq[1] <- ifelse(y_seq[1] < lower_c, y_seq[1], 2*lower_c)
  
  x_seq[length(x_seq)] <- ifelse(x_seq[length(x_seq)] < upper_e,
                                 1.5*upper_e,
                                 x_seq[length(x_seq)])
  
  if (!is.null(xlim)) {
    lower_e <- xlim[1]
    upper_e <- xlim[2]
  }
  if (!is.null(ylim)) {
    lower_c <- ylim[1]
    upper_c <- ylim[2]
  }
  
  polygone_x <- c(min(x_seq),
                  seq(min(x_seq), max(x_seq), step_size),
                  max(x_seq))
  
  polygone_y <- c(min(y_seq),
                  wtp * seq(min(x_seq), max(x_seq), step_size),
                  min(y_seq))
  
  list(seq =
         list(x = x_seq,
              y = y_seq),
       k =
         list(x = x_k,
              y = y_k),
       limits =
         list(xlim = c(lower_e, upper_e),
              ylim = c(lower_c, upper_c)),
       polygon =
         list(x = polygon_x,
              y = polygon_y))
}
