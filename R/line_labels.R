
#' Create Labels for Plot
#' 
#' @template args-he
#' @param ... Additional arguments
#' @export
#' 
line_labels <- function(he, ...) UseMethod("line_labels", he)


#' @rdname line_labels
#' @export
#' 
line_labels.default <- function(he, ...) {
  
  if (he$n_comparisons == 1) return("")
  
  paste(he$interventions[he$ref], "vs",
        he$interventions[he$comp])
}


#' @rdname line_labels
#' @export
#' 
line_labels.pairwise <- function(he, ...) {
  he$interventions
}

