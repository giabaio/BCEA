
#' @rdname mixedAn
#' 
#' mkt.shares is a vector of market shares for each comparators
#' if no value is provided, then assumes uniform distribution
#' dev is the device to which the graph should be printed
#' default is x11() on screen.
#' Possibilities are jpeg and postscript
#' @return Object of class mixedAn
#' 
#' @reference: Baio G, Russo P (2009).
#' @export
#'
mixedAn.bcea <- function(he,
                         mkt.shares = NULL,
                         plot = FALSE) {
  
  Ubar <- OL.star <- evi.star <- NULL
  if (is.null(mkt.shares)) {
    mkt.shares <- rep(1, he$n_comparators)/he$n_comparators
  }
  temp <- array(NA, c(he$n_sim,length(he$k), he$n_comparators))
  for (j in seq_len(he$n_comparators)) {
    temp[, , j] <- mkt.shares[j]*he$U[, , j]
  }
  Ubar <- apply(temp, c(1, 2), sum)
  OL.star <- he$Ustar - Ubar
  evi.star <- apply(OL.star, 2, mean)
  
  ma <- list(
    Ubar = Ubar,
    OL.star = OL.star,
    evi.star = evi.star,
    k = he$k,
    Kmax = he$Kmax,
    step = he$step,
    ref = he$ref,
    comp = he$comp,
    mkt.shares = mkt.shares,
    n_comparisons = he$n_comparisons,
    interventions = he$interventions,
    evi = he$evi)
  
  class(ma) <- "mixedAn"
  
  if (plot) {
    plot.mixedAn(ma)
  }
  return(ma)
}


#' @rdname mixedAn
#' @export
#' 
mixedAn.default <- function(he,
                            mkt.shares = NULL,
                            plot = FALSE) {
  stop("No method available", call. = TRUE)
}

