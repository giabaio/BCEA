
#' Calculate Credible Intervals
#' 
#' For expected incremental benefit plot.
#' 
#' @template args-he
#' @param alpha_cri Significance level, 0 - 1
#' @param cri.quantile Credible interval quantile?; logical
#' 
#' @return cri
#' 
#' @importFrom stats qnorm sd quantile
#' 
compute_eib_cri <- function(he,
                            alpha_cri = 0.05,
                            cri.quantile = TRUE) {
  
  if (he$n_comparisons > 1) {
    margin <- c(1, 3)
  } else {
    margin <- 1
  }
  
  ##TODO; do low and high together so only one call to matlines()
  compute_cri <- function(x, low = TRUE) {
    tau <- ifelse(low, alpha_cri/2, 1 - alpha_cri/2)
    
    if (cri.quantile) {
      return(quantile(x, tau))
    } else {
      return(mean(x) - qnorm(tau) * sd(x))
    }
  }
  
  cri <- data.frame(
    low =
      c(apply(he$ib, margin, FUN = compute_cri, low = TRUE)),
    upp =
      c(apply(he$ib, margin, FUN = compute_cri, low = FALSE)),
    comp =
      as.factor(
        rep(1:he$n_comparisons, each = length(he$k))),
    k = he$k)
  
  return(cri)
}

