
#' Add Contours to Base R Plot
#'
#' @template args-he 
#' @param params List
#' @keywords internal aplot
#' @return plot side effect
#'
add_contours <- function(he, params) {
  
  scale <- params$scale
  levels <- params$levels
  nlevels <- params$nlevels
  pts_col <- params$points$col
  
  for (i in seq_along(he$delta_e)) {
    
    density <- MASS::kde2d(as.matrix(he$delta_e[, i]),
                           as.matrix(he$delta_c[, i]),
                           n = 300,
                           h = c(sd(as.matrix(he$delta_e[, i]))/scale,
                                 sd(as.matrix(he$delta_c[, i]))/scale))
    
    if (!any(is.na(density$z))) {
      if (is.null(nlevels)) {
        # normalise the density and use levels in the contour
        density$z <-
          (density$z - min(density$z)) / (max(density$z) - min(density$z))
        
        graphics::contour(
          density$x,
          density$y,
          density$z,
          add = TRUE,
          levels = levels,
          col = pts_col[i],
          lwd = params$contour$size,
          drawlabels = TRUE)
      }
      if (!is.null(nlevels)) {
        graphics::contour(
          density$x,
          density$y,
          density$z,
          add = TRUE,
          col = pts_col[i],
          lwd = params$contour$size,
          nlevels = nlevels,
          drawlabels = FALSE)
      }
    }
  }
}

