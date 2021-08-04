
#' credible intervals
#' 
plot_eib_cri <- function(he, params) {
  
  if (!params$plot.cri) return()
  
  ##TODO: move y rearranging to compute_eib_cri()
  ##TODO: single do.call() on combined y matrix
  do.call(matlines,
          list(x = he$k,
               y = matrix(params$data$low,
                          ncol = he$n_comparisons),
               col = params$col,
               lty = params$lty))
  
  do.call(matlines,
          list(x = he$k,
               y = matrix(params$data$upp,
                          ncol = he$n_comparisons),
               col = params$col,
               lty = params$lty))
}
