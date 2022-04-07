
#' Add Contours to Base R Plot
#'
#' @template args-he 
#' @param params 
#'
#' @return
#' @export
#'
add_contours <- function(he, params) {
  
  scale <- params$scale
  levels <- params$levels
  nlevels <- params$nlevels
  
  density <- MASS::kde2d(as.matrix(he$delta_e),
                         as.matrix(he$delta_c),
                         n = 300,
                         h = c(sd(as.matrix(he$delta_e)) / scale,
                               sd(as.matrix(he$delta_c)) / scale))
  
  ##TODO: this isn't how to use ifelse(); refactor
  # Changes the range so that the plot always shows the x and y axes
  # ch1 <- ifelse(m.e > 0,
  #               m.e <- -m.e,
  #               m.e <- m.e)
  # ch2 <- ifelse(M.e < 0, M.e <- -M.e, M.e <- M.e)
  # ch3 <- ifelse(m.c > 0, m.c <- -m.c, m.c <- m.c)
  # ch4 <- ifelse(M.c < 0, M.c <- -M.c, M.c <- M.c)
  
  if (!any(is.na(density$z))) {
    if (!is.null(levels)) {
      # normalise the density and use levels in the contour
      density$z <-
        (density$z - min(density$z)) / (max(density$z) - min(density$z))
      
      graphics::contour(
        density$x,
        density$y,
        density$z,
        add = TRUE,
        levels = levels,
        drawlabels = TRUE)
    }
    if (is.null(levels)) {
      graphics::contour(
        density$x,
        density$y,
        density$z,
        add = TRUE,
        nlevels = nlevels,
        drawlabels = FALSE)
    }
  }
}

