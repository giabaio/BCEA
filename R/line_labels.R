
#' @export
#' 
line_labels <- function(he, ...) UseMethod("line_labels", he)

#' @export
#' 
line_labels.default <- function(he) {
  
  if (he$n_comparisons == 1) return("")
  
  paste(he$interventions[he$ref], "vs",
        he$interventions[he$comp])
}

#' @export
#' 
line_labels.pairwise <- function(he) {
  he$interventions
}