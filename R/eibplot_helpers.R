
#' Calculate Credible Intervals
#' 
#' @template args-he
#' @param alpha
#' @param cri.quantile
#' 
#' @return cri
#' 
#' @importFrom stats qnorm sd quantile
#' 
eib.plot.cri <- function(he,
                         alpha,
                         cri.quantile) {
  
  if (alpha < 0 | alpha > 1) {
    warning(
      "Argument alpha must be between 0 and 1. Reset to default at 0.95",
      call. = FALSE)
    alpha <- 0.05
  }
  margin <- 1
  if (he$n_comparison > 1)
    margin <- c(1, 3)
  
  cri <- data.frame(
    "low" =
      c(apply(he$ib,
              margin,
              function(x)
                ifelse(cri.quantile,
                       quantile(x, (alpha) / 2),
                       mean(x) - qnorm((alpha) / 2) * sd(x)))),
    "upp" = c(apply(he$ib, margin,
                    function(x)
                      ifelse(
                        cri.quantile,
                        quantile(x, 1 - (alpha) / 2),
                        mean(x) - qnorm(1 - (alpha) / 2) * sd(x)
                      ))),
    "comp" = as.factor(
      rep(1:he$n_comparison, each = length(he$k)))
  )
  return(cri)
}

