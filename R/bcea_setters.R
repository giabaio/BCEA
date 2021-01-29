
#' Set Reference Group
#'
#' Alternative way to define (e,c) reference group.
#' 
#' @template args-he
#' @param value Reference group number
#'
#' @return bcea-type objects
#' @export
#'
'setReferenceGroup<-' <- function(he, value) {
  UseMethod('setReferenceGroup<-', he)
}

#' @rdname setReferenceGroup<-
#' @export
#'
'setReferenceGroup<-.bcea' <- function(he, value) {
  
  bcea(eff = he$e,
       cost = he$c,         
       ref = value,
       interventions = he$treats,
       Kmax = he$Kmax,          
       plot = FALSE)
}

#' @rdname setReferenceGroup<-
#' @export
#'
'setReferenceGroup<-.default' <- function(he, value) {
  stop("No method available.")
}

# -------------------------------------------------------------------------

#' Set Maximum Willingness to Pay
#'
#' Alternative way to define `K` statistic.
#' 
#' @template args-he
#' @param value Maximum willingness to pay
#'
#' @return bcea-type oject
#' @export
#'
'setKmax<-' <- function(he, value) {
  UseMethod('setKmax<-', he)
}

#' @rdname setKmax<-
#' @export
#'
'setKmax<-.bcea' <- function(he, value) {
  
  bcea(eff = he$e,
       cost = he$c,         
       ref = he$ref,
       interventions = he$treats,
       Kmax = value,          
       plot = FALSE)
}

#' @rdname setKmax<-
#' @export
#'
'setKmax<-.default' <- function(he, value) {
  stop("No method available.")
}
