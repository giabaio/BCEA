
line_labels <- function(he, ...) UseMethod("line_labels", he)

line_labels.default <- function(he) {
  paste(he$interventions[he$ref], "vs",
        he$interventions[he$comp])
}

line_labels.pairwise <- function(he) {
  he$interventions
}