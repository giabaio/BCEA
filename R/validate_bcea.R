
#' Validate bcea
#'
#' @param eff Effectiveness matrix
#' @param cost Cost matrix
#' @param ref Reference intervention
#' @param interventions All interventions 
#'
#' @export
#' @keywords internal
#' 
validate_bcea <- function(eff,
                          cost,
                          ref,
                          interventions) {
  
  if (!is.matrix(cost) || !is.matrix(eff))
    stop("eff and cost must be matrices.", call. = FALSE)
  
  if (ncol(cost) == 1 || ncol(eff) == 1)
    stop("Require at least 2 comparators.", call. = FALSE)
  
  if (!is.null(interventions) && length(interventions) != ncol(eff))
    stop("interventions names wrong length.", call. = FALSE)
  
  if (any(dim(eff) != dim(cost)))
    stop("eff and cost are not the same dimensions.", call. = FALSE)
  
  if (!is.numeric(ref) || ref < 1 || ref > ncol(eff))
    stop("reference is not in available interventions.", call. = FALSE)
  
  return()
}

