
#' Optimal intervention
#' 
#' Select the best option for each value of willingness to pay.
#'
#' @param eib Expected incremental benefit 
#' @param ref Reference group number
#' @param comp Comparison group number(s)
#'
#' @return Group index
#' @export
#'
best_interv_given_k <- function(eib,
                                ref,
                                comp) {

  if (length(comp) == 1) {
    
    best <- rep(ref, NROW(eib))
    best[eib < 0] <- comp         ##TODO: why isnt it eib > 0?
    
  } else {
    
    ##TODO: what cases would this be NULL?
    if (is.null(dim(eib))) {
      
      min_eib  <- min(eib)
      which_eib <- which.min(eib)	
      
    } else {
      
      min_eib <- apply(eib, 1, min)
      which_eib <- apply(eib, 1, which.min)
    }
    
    best <- ifelse(min_eib > 0,
                   yes = ref,
                   no = comp[which_eib])
  }
  
  best
}

