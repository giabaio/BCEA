
#' subset_by_comparisons
#' 
subset_by_comparisons <- function(he, comparison) {
  
  if (is.null(comparison)) return(he)
  
  if (he$ref %in% comparison)
    stop("Can't select Reference group. Change Reference first.",
         call. = FALSE)
  
  name_comp <- he$interventions[comparison]
  
  res <- 
    bcea(e = he$e,
         c = he$c,
         ref = he$ref,
         interventions = he$interventions,
         Kmax = he$Kmax,
         wtp = he$wtp)
  
  res$delta_e <- as.matrix(he$delta_e)[, name_comp, drop = FALSE]
  res$delta_c <- as.matrix(he$delta_c)[, name_comp, drop = FALSE]
  
  res$ICER <- res$ICER[name_comp]
  res$ib <- res$ib[, , name_comp]
  res$eib <- res$eib[, name_comp]
  res$U <- res$U[, , name_comp]
  res$ceac <- res$ceac[, name_comp]
  
  return(res)
}

