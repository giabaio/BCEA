
#' @rdname CEriskav_assign
#' 
#' @description Default vector of risk aversion parameters:
#' 1e-11, 2.5e-6, 5e-6
#' 
#' @export
#' 
'CEriskav<-.bcea' <- function(he,
                              value) {
  
  value[value == 0] <- 1e-10
  
  ### COMPARISON IS USED TO SELECT THE COMPARISON FOR WHICH THE ANALYSIS IS CARRIED OUT
  # Reference: Baio G, Dawid AP (2011).
  
  if (is.null(value)) {
    value <- c(1e-11, 2.5e-6, 5e-6)
  }
  
  # expected utilities & EVPI for risk aversion cases
  K <- length(he$k)
  R <- length(value)
  Ur <- array(NA, c(dim(he$U), R))
  Urstar <- array(NA, c(dim(he$Ustar), R))
  comparator_idx <- c(he$comp, he$ref)
  
  for (i in seq_len(K)) {
    for (l in seq_len(R)) {
      for (j in comparator_idx) {
        Ur[, i, j, l] <- (1/value[l])*(1 - exp(-value[l]*he$U[, i, j]))
      }
      Urstar[, i, l] <- apply(Ur[, i, , l], 1, max, na.rm = TRUE)
    }
  }
  
  IBr <- array(NA, c(he$n_sim, K, he$n_comparisons, R))
  
  for (i in seq_len(he$n_comparisons)) {
    IBr[,, i,] <- Ur[, , he$ref, , drop = FALSE] - Ur[, , he$comp[i], , drop = FALSE]
  }

  eibr <- apply(IBr, c(2,3,4), mean)
  
  vir <- array(NA, c(he$n_sim, K, R))
  
  for (i in seq_len(K)) {
    for (l in seq_len(R)) {
      vir[, i, l] <- Urstar[, i, l] - max(apply(Ur[, i, , l], 2, mean, na.rm = TRUE), na.rm = TRUE)
    }
  }
  
  evir <- apply(vir, c(2, 3), mean)
  
  structure(
    modifyList(
      he,
      list(Ur = Ur,
           Urstar = Urstar,
           IBr = IBr,
           eibr = eibr,
           vir = vir,
           evir = evir,
           R = R,
           r = value)),
    class = c("CEriskav", class(he)))
}


#' @rdname CEriskav_assign
#' 
#' @export
#'
'CEriskav<-.default' <- function(he, value) {
  stop("No available method.", call. = FALSE)
}
