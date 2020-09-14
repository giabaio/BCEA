
#' @export
#'
CEriskav.bcea <- function(he,
                          r = NULL,
                          comparison = 1) {
  
  ### COMPARISON IS USED TO SELECT THE COMPARISON FOR WHICH THE ANALYSIS IS CARRIED OUT
  # Reference: Baio G, Dawid AP (2011).
  # Default vector of risk aversion parameters
  
  if (is.null(r)) {
    r <- c(1e-11, 2.5e-6, 5e-6)
  }
  
  # expected utilities & EVPI for risk aversion cases
  K <- length(he$k)
  R <- length(r)
  Ur <- array(NA, c(dim(he$U), R))
  Urstar <- array(NA, c(dim(he$Ustar), R))
  
  for (i in seq_len(K)) {
    for (l in seq_len(R)) {
      for (j in seq_len(he$n_comparators)) {
        Ur[, i, j, l] <- (1/r[l])*(1 - exp(-r[l]*he$U[, i, j]))
      }
      Urstar[, i, l] <- apply(Ur[, i, , l], 1, max)
    }
  }
  
  IBr <- Ur[, , he$ref, ] - Ur[, , he$comp[comparison], ]
  
  eibr <- apply(IBr, c(2,3), mean)
  vir <- array(NA, c(he$n_sim, K, R))
  
  for (i in seq_len(K)) {
    for (l in seq_len(R)) {
      vir[, i, l] <- Urstar[, i, l] - max(apply(Ur[, i, , l], 2, mean))
    }
  }
  evir <- apply(vir, c(2, 3), mean)
  
  structure(
    list(Ur = Ur,
         Urstar = Urstar,
         IBr = IBr,
         eibr = eibr,
         vir = vir,
         evir = evir,
         R = R,
         r = r,
         k = he$k),
    class = "CEriskav")
}


#' @export
#'
CEriskav.default <- function(he, ...) {
  stop("No available method.", call. = FALSE)
}

