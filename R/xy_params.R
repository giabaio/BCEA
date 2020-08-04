
##TODO: need to compare with master
#
xy_params <- function(he,
                      wtp,
                      graph_params) {
  
  e_dat <- he$delta_e
  c_dat <- he$delta_c
  
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
  
  # min_c <- min_e*wtp
  # max_c <- max_e*wtp
  
  list(xlim = c(min_e, max_e),
       ylim = c(min_c, max_c))
}
