
#' Create Labels for Plot
#' 
#' @template args-he
#' @param ... Additional arguments
#' @export
#' @keywords internal
#' 
line_labels <- function(he, ...) UseMethod("line_labels", he)


#' Swapped labels so that reference is second
#' @rdname line_labels
#' @export
#' 
line_labels.default <- function(he, ref_first = TRUE, ...) {
  
  if (he$n_comparisons == 1) return("")
  
  if (ref_first) {
    paste(he$interventions[he$ref], "vs",
          he$interventions[he$comp])
  } else {
    paste(he$interventions[he$comp], "vs",
          he$interventions[he$ref])
  }
}

#' @rdname line_labels
#' @export
#' 
line_labels.pairwise <- function(he, ...) {
  he$interventions
}

