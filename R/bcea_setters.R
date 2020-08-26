
#' Set Reference Group
#'
#' @template args-he
#' @param ref 
#'
#' @return
#' @export
#'
'setReferenceGroup<-' <- function(he, value) {
  UseMethod('setReferenceGroup<-', he)
}

#' @export
#'
'setReferenceGroup<-.bcea' <- function(he, value) {
  
  bcea(e = he$e,
       c = he$c,         
       ref = value,
       interventions = he$treats,
       Kmax = he$Kmax,          
       plot = FALSE)
}

#' @export
#'
'setReferenceGroup<-.default' <- function(he, value) {
  stop("No method available.")
}

# -------------------------------------------------------------------------

#' Set Maximum Willingness to Pay
#'
#' @template args-he
#' @param Kmax 
#'
#' @return
#' @export
#'
'setKmax<-' <- function(he, value) {
  UseMethod('setKmax<-', he)
}

#' @export
#'
'setKmax<-.bcea' <- function(he, value) {
  
  bcea(e = he$e,
       c = he$c,         
       ref = he$ref,
       interventions = he$treats,
       Kmax = value,          
       plot = FALSE)
}

#' @export
#'
'setKmax<-.default' <- function(he, ref) {
  stop("No method available.")
}
