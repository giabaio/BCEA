
#' @name setComparisons
#' @title Set Comparisons Group
#' 
#' @description One of the alternative way to set (e,c) comparison group.
#' Simply recompute all comparisons and drop unwanted.
#' 
#' @template args-he
#' @template args-comparison 
#' @seealso [setComparisons<-()]
#' @export
#' 
setComparisons <- function(he, comparison) {
  
  if (is.null(comparison)) return(he)
  
  if (he$ref %in% comparison)
    stop("Can't select Reference group. Change Reference first.",
         call. = FALSE)
  
  n_interv <- ncol(he$e)
  if (any(!comparison %in% 1:n_interv))
    stop("Comparison index not in available comparisons.",
         call. = FALSE)
  
  res <- 
    bcea(eff = he$e,
         cost = he$c,
         ref = he$ref,
         interventions = he$interventions,
         Kmax = he$Kmax,
         k = he$k)
  
  name_comp <- he$interventions[comparison]
  
  res$comp <- comparison
  res$n_comparisons <- length(comparison)
  res$n_comparators <- length(comparison) + 1
  
  res$delta_e <- res$delta_e[, name_comp, drop = FALSE]
  res$delta_c <- res$delta_c[, name_comp, drop = FALSE]
  
  res$ICER <- res$ICER[name_comp]
  res$ib <- res$ib[, , name_comp, drop = FALSE]
  res$eib <- res$eib[, name_comp, drop = FALSE]
  res$ceac <- res$ceac[, name_comp, drop = FALSE]
  
  ##TODO: is there a way not to recompute the whole thing?
  res$best <- best_interv_given_k(res$eib, res$ref, res$comp)
  res$kstar <- compute_kstar(res$k, res$best, res$ref)
  
  ##TODO: currently compute _all_ interventions in compute_U()
  ##      change to this?
  # res$U <- res$U[, , name_comp, drop = FALSE]
  
  return(res)
}


#' @name setComparisons_assign
#' @title Set Comparison Group
#'
#' @description One of the alternative way to set (e,c) comparison group.
#' 
#' @template args-he
#' @param value Comparison 
#' @return bcea-type object
#' @seealso [setComparisons()]
#' @export
#'
'setComparisons<-' <- function(he, value) {
  UseMethod('setComparisons<-', he)
}

#' @rdname setComparisons_assign
#' @export
#'
'setComparisons<-.bcea' <- function(he, value) {
  
  setComparisons(he, value)
}

#' @rdname setComparisons_assign
#' @export
#'
'setComparisons<-.default' <- function(he, value) {
  stop("No method available.")
}

