
#' Prepare K-star vertical lines
#' 
#' @template args-he
#' @param plot_params Plots parameters
#' @keywords internal
#' 
kstar_vlines <- function(he, plot_params) {
  
  if (length(he$kstar) > 0) {
    abline(v = he$kstar,
           col = "dark grey",
           lty = "dotted")
    
    text(x = he$kstar,
         y = min(plot_params$ylim),
         cex = ifelse(is.null(plot_params$kstar$size), 1, plot_params$kstar$size),
         paste("k* = ", he$kstar , sep = ""))
  }
}

