
#
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Reports whether x is a rel object
#' Copied from ggplot2
#' 
#' @param x An object to test
#' @keywords internal
is.rel <- function(x) inherits(x, "rel")

#' Allow disabling of the cat messages
#' @param x Object to quietly return
#' @keywords internal
#' 
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}