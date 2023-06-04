
#' @export
#'
'mixedAn<-.bcea' <- function(he,
                             value = NULL) {
  
  Ubar <- OL.star <- evi.star <- NULL
  
  value <- value %||% rep(1, he$n_comparators)/he$n_comparators
  
  Ubar <- compute_Ubar(he, value)
  OL.star <- he$Ustar - Ubar
  evi.star <- compute_EVI(OL.star)
  
  structure(
    modifyList(
      he,
      list(
        Ubar = Ubar,
        OL.star = OL.star,
        evi.star = evi.star,
        mkt.shares = value)),
    class = c("mixedAn", class(he)))
}


#' @export
#' 
'mixedAn<-.default' <- function(he,
                                value) {
  stop("No method available", call. = FALSE)
}

