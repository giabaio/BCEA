
#' @keywords dplot
#' 
ceac_legend_base <- function(he,
                             pos_legend,
                             plot_params) {
  
  pos_legend <- where_legend(he, pos_legend)
  
  text <- line_labels(he)
  
  list(x = pos_legend,
       legend = text,
       cex = 0.7,
       bty = "n", 
       lty = plot_params$lty,
       col = plot_params$col,
       pch = plot_params$pch)
}


#' @keywords dplot
#' 
ceplane_legend_base <- function(he,
                                pos_legend,
                                plot_params) {
  
  pos_legend <- where_legend(he, pos_legend)
  
  text <- line_labels(he)
  
  list(x = pos_legend,
       legend = text,
       cex = 0.7,
       bty = "n", 
       col = plot_params$points$col,
       pch = plot_params$points$pch)
}


#
where_legend <- function(he,
                         pos_legend) {
  
  # empty legend
  if (!inherits(he, "pairwise") & he$n_comparisons == 1)
    return(NA)
  
  if (is.numeric(pos_legend) & length(pos_legend) == 2) {
    
    ns <- ifelse(pos_legend[2] == 1, "top", "bottom")
    ew <- ifelse(pos_legend[1] == 1, "right", "left")
    return(paste0(ns, ew))
  }
  
  if (is.logical(pos_legend)) {
    if (pos_legend)
      return("bottomleft")
    else
      return("bottomright")
  }
}

