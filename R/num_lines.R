# num_lines ---------------------------------------------------------------


num_lines <- function(he) {
  UseMethod('num_lines', he)
}

num_lines.pairwise <- function(he) {
  he$n_comparators
}

num_lines.default <- function(he) {
  he$n_comparisons
}

