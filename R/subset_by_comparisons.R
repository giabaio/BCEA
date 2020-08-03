
#
subset_by_comparisons <- function(he, comparison) {
  
  
  if (is.null(comparison)) comparison <- 1:he$n_comparisons
  
  he$delta_e <- as.matrix(he$delta_e)[, comparison, drop = FALSE]
  he$delta_c <- as.matrix(he$delta_c)[, comparison, drop = FALSE]
  
  he$n_comparisons <- length(comparison)
  he$n_comparators <- length(comparison) + 1
  
  he$icer <- he$icer[comparison]
  
  he$comp <- he$comp[comparison]
  
  he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
  he$ICER <- he$ICER[comparison]
  he$ib <- he$ib[, , comparison]
  he$eib <- he$eib[, comparison]
  he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
  he$ceac <- he$ceac[, comparison]
  he$mod <- TRUE
  
  return(he)
}


