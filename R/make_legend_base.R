
#' @keywords dplot internal
#' 
evppi_legend_base <- function(evppi_obj,
                              pos_legend,
                              col = NULL) {
  
  legend_here <- where_legend(evppi_obj, pos_legend)
  
  text <- evppi_legend_text(evppi_obj)
  cols <- evppi_legend_cols(evppi_obj, col)
  
  list(x = legend_here,
       legend = text,
       cex = 0.7,
       bty = "n", 
       lty = c(1, seq_along(evppi_obj$parameters)),
       lwd = c(2, rep(1, length(evppi_obj$parameters))),
       col = cols)
}


#' @keywords dplot internal
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


#' @keywords dplot internal
#' 
ceplane_legend_base <- function(he,
                                pos_legend,
                                plot_params) {
  
  pos_legend <- where_legend(he, pos_legend)
  
  text <- line_labels(he, ref_first = plot_params$ref_first)
  
  list(x = pos_legend,
       legend = text,
       cex = 0.7,
       bty = "n",
       col = plot_params$points$col,
       pch = plot_params$points$pch)
}


#' @keywords internal
#' 
where_legend <- function(he,
                         pos_legend) {
  
  # cases with empty legend
  if (!inherits(he, "pairwise") &&
      !inherits(he, "CEriskav") &&
      ("n_comparisons" %in% names(he)) &&
      he$n_comparisons == 1)
    return(NA)
  
  where_legend_always(he, pos_legend)  
}


#' @keywords internal
#' 
where_legend_always <- function(he,
                                pos_legend) {
  
  if (is.numeric(pos_legend) && length(pos_legend) == 2) {
    
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
  
  if (pos_legend != "")
   if (grepl("^(bottom|top)*(left|right)*$", pos_legend))
     return(pos_legend)
  
  message("Legend position not recognised.")
  return(NA)
}



