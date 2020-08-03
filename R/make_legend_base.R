
#' @keywords dplot
#' 
make_legend_base <- function(he,
                             pos_legend,
                             plot_params) {
  
  # empty legend
  if (!inherits(he, "pairwise") & he$n_comparisons == 1) {
    return(list(x = NA, legend = ""))}
  
  if (is.numeric(pos_legend) & length(pos_legend) == 2) {
    
    ns <- ifelse(pos_legend[2] == 1, "top", "bottom")
    ew <- ifelse(pos_legend[1] == 1, "right", "left")
    pos_legend <- paste0(ns, ew)
  }
  
  if (is.logical(pos_legend)) {
    if (!pos_legend)
      pos_legend <- "bottomright"
    else
      pos_legend <- "bottomleft"
  }
  
  text <- line_labels(he)
  
  list(x = pos_legend,
       legend = text,
       cex = 0.7,
       bty = "n", 
       lty = plot_params$lty,
       col = plot_params$col,
       pch = plot_params$pch)
}
