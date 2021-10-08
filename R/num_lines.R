# num_lines ---------------------------------------------------------------


#' @title Get number of lines
#' @name num_lines
#' @param dat Data
#' 
num_lines <- function(dat) {
  UseMethod('num_lines', dat)
}

#' @rdname num_lines
#' 
num_lines.pairwise <- function(dat) {
  dat$n_comparators
}

#' @rdname num_lines
#' 
num_lines.bcea <- function(dat) {
  dat$n_comparisons
}

#' @rdname num_lines
#' 
num_lines.evppi <- function(dat) {
  2
}

#' @rdname num_lines
#' 
num_lines.default <- function(dat) {
  dat$n_comparisons
}

