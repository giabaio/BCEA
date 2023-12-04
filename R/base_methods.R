
#' Check bcea Class
#' @template args-he
#' @export
#' 
#' @return
#' `is.bcea` returns TRUE or FALSE depending on whether its argument
#'  is a bcea class object.
#' 
is.bcea <- function(he) inherits(he, "bcea")


#' bcea Print Method
#' 
#' @param x A `bcea` object containing the results of the Bayesian
#'        modelling and the economic evaluation.
#' @param digits Minimal number of significant digits, see [print.default()].
#' @param give.attr Logical; if TRUE (default), show attributes as sub structures.
#' @param no.list Logical; if TRUE, no ‘list of ...’ nor the class are printed.
#' @param ... Potential further arguments.
#' @keywords print
#' @export
#' @importFrom utils str
#' 
#' @examples 
#' data("Vaccine")
#' he <- BCEA::bcea(eff, cost)
#' 
print.bcea <- function(x,
                       digits = getOption("digits"),
                       give.attr = FALSE,
                       no.list = TRUE,
                       ...){
  out <-
    str(x,
        give.attr = give.attr,
        digits.d = digits,
        no.list = no.list,
        ...)
  invisible(out)
}

