
#' Check bcea Class
#' @export
#' 
#' @return
#' \code{is.bcea} returns TRUE or FALSE depending on whether its argument is a bcea class object.
#' 
is.bcea <- function(x) inherits(x, "bcea")

#' bcea Print Method
#' 
#' @template args-he
#' @param digits Minimal number of significant digits, see \code{\link{print.default}}.
#' @param give.attr Logical; if TRUE (default), show attributes as sub structures.
#' @param no.list Logical; if TRUE, no ‘list of ...’ nor the class are printed.
#' @param ... Potential further arguments.
#' @keywords print
#' @export
#' 
#' @examples 
#' data("Vaccine")
#' he <- BCEA::bcea(e, c)
#' 
print.bcea <- function(he,
                       digits = getOption("digits"),
                       give.attr = FALSE,
                       no.list = TRUE,
                       ...){
  
  out <- str(he, give.attr = give.attr, digits.d = digits, no.list = no.list, ...)
  invisible(out)
}

