
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
