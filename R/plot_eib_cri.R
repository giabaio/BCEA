
#' credible intervals
#' 
plot_eib_cri <- function(he, plot_params) {
  
  if (!plot_params$plot.cri) return()
  
  do.call(matlines,
          list(x = he$k,
               y = plot_params$cri$low,
               col = plot_params$line$cri_col,
               lty = plot_params$line$cri_lty))
  
  do.call(matlines,
          list(x = he$k,
               y = plot_params$cri$upp,
               col = plot_params$line$cri_col,
               lty = plot_params$line$cri_lty))
}

