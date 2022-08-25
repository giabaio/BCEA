
#' @name setReferenceGroup_assign
#' @title Set Reference Group
#'
#' @description Alternative way to define (e,c) reference group.
#' 
#' @template args-he
#' @param value Reference group number
#'
#' @return bcea-type object
#' @export
#'
'setReferenceGroup<-' <- function(he, value) {
  UseMethod('setReferenceGroup<-', he)
}

#' @rdname setReferenceGroup_assign
#' @export
#'
'setReferenceGroup<-.bcea' <- function(he, value) {
  
  bcea(eff = he$e,
       cost = he$c,         
       ref = value,
       interventions = he$interventions,
       Kmax = he$Kmax,          
       plot = FALSE)
}

#' @rdname setReferenceGroup_assign
#' @export
#'
'setReferenceGroup<-.default' <- function(he, value) {
  stop("No method available.")
}

# -------------------------------------------------------------------------

#' @name setKmax_assign
#' @title Set Maximum Willingness to Pay
#'
#' @description Alternative way to define `K` statistic.
#' 
#' @template args-he
#' @param value Maximum willingness to pay
#'
#' @return bcea-type object
#' @export
#'
'setKmax<-' <- function(he, value) {
  UseMethod('setKmax<-', he)
}

#' @rdname setKmax_assign
#' @export
#'
'setKmax<-.bcea' <- function(he, value) {
  
  bcea(eff = he$e,
       cost = he$c,         
       ref = he$ref,
       interventions = he$interventions,
       Kmax = value,          
       plot = FALSE)
}

#' @rdname setKmax_assign
#' @export
#'
'setKmax<-.default' <- function(he, value) {
  stop("No method available.")
}
