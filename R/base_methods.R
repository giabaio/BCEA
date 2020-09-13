
#'
is.bcea <- function(x) inherits(x, "bcea")

#' data("Vaccine")
#' he <- BCEA::bcea(e, c)
#' 
print.bcea <- function(x,
                       digits = getOption("digits"),
                       give.attr = FALSE,
                       no.list = TRUE,
                       ...){
  
  out <- str(x, give.attr = give.attr, digits.d = digits, no.list = no.list, ...)
  invisible(out)
}

