
#
prep_contour_ggplot_range <- function(he) {
  
  ranges <- list()
  
  if (he$n_comparisons == 1) {
    kd <- data.frame(e = unlist(he$delta_e),
                     c = unlist(he$delta_c))
    
    # plot limits
    ranges$e <- range(kd$e)
    ranges$c <- range(kd$c)
    ranges$e[1] <- ifelse(ranges$e[1] < 0, ranges$e[1], -ranges$e[1])
    ranges$c[1] <- ifelse(ranges$c[1] < 0, ranges$c[1], -ranges$c[1])
  }
  
  if (he$n_comparisons > 1) {
    kd <- data.frame(
      delta_e = c(as.matrix(he$delta_e)),
      delta_c = c(as.matrix(he$delta_c)),
      comparison =
        as.factor(sort(
          rep(1:he$n_comparisons, dim(as.matrix(he$delta_e))[1])))
    )
    
    ranges$e <- ranges(kd$delta_e)
    ranges$c <- ranges(kd$delta_c)
    ranges$e[1] <- ifelse(ranges$e[1] < 0, ranges$e[1], -ranges$e[1])
    ranges$c[1] <- ifelse(ranges$c[1] < 0, ranges$c[1], -ranges$c[1])
  }
  
  ranges
}

