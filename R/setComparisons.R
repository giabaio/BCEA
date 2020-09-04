
#' Set Comparisons Group
#' 
#' @template args-he
#' @template args-comparison 
#' 
#' @export
#' 
setComparisons <- function(he, comparison) {
  
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


#' Set Comparison Group
#'
#' @template args-he
#' @param value Comparison 
#'
#' @return
#' @export
#'
'setComparisons<-' <- function(he, value) {
  UseMethod('setComparisons<-', he)
}

#' @export
#'
'setComparisons<-.bcea' <- function(he, value) {
  
  setComparisons(he, value)
}

#' @export
#'
'setComparisons<-.default' <- function(he, value) {
  stop("No method available.")
}

