
# Package: ldr
# Type: Package
# Title: Methods for likelihood-based dimension reduction in regression
# Version: 1.3.3
# Date: 2014-06-06
# Author: Kofi Placid Adragni, Andrew Raim
# Maintainer: Kofi Placid Adragni <kofi@umbc.edu>
# Description: Functions, methods, and data sets for fitting likelihood-based dimension reduction in regression,
# using principal fitted components (pfc), likelihood acquired directions (lad), covariance reducing models (core).
# URL: https://www.jstatsoft.org/v61/i03/
#   License: GPL (>= 2)
# Packaged: 2021-10-08 16:32:42 UTC; Nathan
# Repository: https://github.com/cran/ldr
# Date/Publication: 2014-10-29 16:36:14
#' @importFrom cli cli_alert_warning
#
orthonorm <- function (u) {
  if (is.null(u)) 
    return(NULL)
  
  if (!(is.matrix(u))) 
    u <- as.matrix(u)
  dd <- dim(u)
  n <- dd[1]
  p <- dd[2]
  if (prod(abs(La.svd(u)$d) > 1e-08) == 0) 
    stop("collinears vectors in orthonorm")
  if (n < p) {
    cli::cli_alert_warning(
      "There are too many vectors to orthonormalize in orthonorm.")
    u <- as.matrix(u[, 1:p])
    n <- p
  }
  v <- u
  if (p > 1) {
    for (i in 2:p) {
      coef.proj <- c(crossprod(u[, i], v[, 1:(i - 1)]))/diag(crossprod(v[, 
                                                                         1:(i - 1)]))
      v[, i] <- u[, i] - matrix(v[, 1:(i - 1)], nrow = n) %*% 
        matrix(coef.proj, nrow = i - 1)
    }
  }
  coef.proj <- 1/sqrt(diag(crossprod(v)))
  return(t(t(v) * coef.proj))
}

