
#
CEriskav.default <- function(he,
                             r = NULL,
                             comparison = 1) {
  ### COMPARISON IS USED TO SELECT THE COMPARISON FOR WHICH THE ANALYSIS IS CARRIED OUT!!!
  # Reference: Baio G, Dawid AP (2011).
  # Default vector of risk aversion parameters
  
  if (is.null(r)) {
    r <- c(1e-11, 0.0000025, 0.000005)
  }
  
  # Computes expected utilities & EVPI for the risk aversion cases
  K <- length(he$k)
  R <- length(r)
  Ur <- array(NA, c(dim(he$U),R))
  Urstar <- array(NA, c(dim(he$Ustar),R))
  
  for (i in seq_len(K)) {
    for (l in seq_len(R)) {
      for (j in seq_len(he$n.comparators)) {
        Ur[, i, j, l] <- (1/r[l])*(1 - exp(-r[l]*he$U[, i, j]))
      }
      Urstar[, i, l] <- apply(Ur[, i, , l], 1, max)
    }
  }
  
  if (he$n.comparisons == 1) {
    IBr <- Ur[, , he$ref, ] - Ur[, , he$comp, ]
  }
  if (he$n.comparisons > 1) {
    IBr <- Ur[, , he$ref, ] - Ur[, , he$comp[comparison], ]
  }
  
  eibr <- apply(IBr, c(2,3), mean)
  vir <- array(NA, c(he$n.sim,K,R))
  
  for (i in seq_len(K)) {
    for (l in seq_len(R)) {
      vir[, i, l] <- Urstar[, i, l] - max(apply(Ur[, i, , l], 2, mean))
    }
  }
  evir <- apply(vir, c(2,3) ,mean)
  
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