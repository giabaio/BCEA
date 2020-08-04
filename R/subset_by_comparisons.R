
#
subset_by_comparisons <- function(he, comparison) {
  
  if (is.null(comparison)) return(he)
  
  name_comp <- he$interventions[comparison]
  he$delta_e <- as.matrix(he$delta_e)[, name_comp, drop = FALSE]
  he$delta_c <- as.matrix(he$delta_c)[, name_comp, drop = FALSE]
  
  he$n_comparisons <- length(comparison)
  he$n_comparators <- length(comparison) + 1
  
  he$comp <- comparison
  
  he$ICER <- he$ICER[comparison]
  he$ib <- he$ib[, , comparison]
  he$eib <- he$eib[, comparison]
  he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
  he$ceac <- he$ceac[, comparison]
  he$change_comp <- TRUE
  
  return(he)
}


