
#' @rdname mixedAn
#' 
#' @export
#'
'mixedAn<-.bcea' <- function(he,
                             value = NULL) {
  
  Ubar <- OL.star <- evi.star <- NULL
  
  if (is.null(value)) {
    value <- rep(1, he$n_comparators)/he$n_comparators
  }
  
  qU <- array(NA,
              c(he$n_sim,length(he$k), he$n_comparators))
  
  for (j in seq_len(he$n_comparators)) {
    qU[, , j] <- value[j]*he$U[, , j]
  }
  
  Ubar <- apply(qU, c(1, 2), sum)
  OL.star <- he$Ustar - Ubar
  evi.star <- apply(OL.star, 2, mean)
  
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


#' @rdname mixedAn
#' @export
#' 
'mixedAn<-.default' <- function(he,
                                value) {
  stop("No method available", call. = TRUE)
}

