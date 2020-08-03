
#
subset_by_comparisons <- function(he, comparison) {
  
  
  if (is.null(comparison)) comparison <- 1:he$n_comparisons
  
  he$delta_e <- as.matrix(he$delta_e)[, comparison, drop = FALSE]
  he$delta_c <- as.matrix(he$delta_c)[, comparison, drop = FALSE]
  
  he$n_comparisons <- length(comparison)
  
  he$icer <- he$icer[comparison]
  
  he$comp <- comparison
  
  return(he)
}