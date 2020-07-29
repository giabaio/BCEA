
#' Compute kstar
#'
#' Find willingness-to-pay threshold when optimal decision changes.
#'  
#' @param k Willingness-to-pay vector
#' @param best Best intervention for each k
#' @param ref Reference intervention
#'
#' @return Array with dimensions
#' @seealso \code{\link{ceac.plot}}
#' 
#' @export
#' 
compute_kstar <- function(k, best, ref) {
  
  if (all(best == ref)) {
    return(NA)    
  }
  
  min(k[best != ref])
}


#' Compute Cost-Effectiveness Acceptability Curve
#'
#' @param ib Incremental benefit
#' @return Array with dimensions (interv x k)
#' @seealso \code{\link{ceac.plot}}
#' 
#' @export
#' 
compute_CEAC <- function(ib) {
  
  apply(ib > 0, c(1,3), mean)
}


#' Compute Expected Incremental Benefit
#' 
#' A summary measure useful to assess the potential changes in the decision
#' under different scenarios.
#' 
#' When considering a pairwise comparison
#' (e.g. in the simple case of a reference intervention \eqn{t = 1} and a comparator,
#' such as the status quo, \eqn{t = 0}), it is defined as the difference between the
#' expected utilities of the two alternatives:
#' 
#' \deqn{eib := \mbox{E}[u(e,c;1)] - \mbox{E}[u(e,c;0)] = \mathcal{U}^1 - \mathcal{U}^0}.
#' 
#' Analysis of the expected incremental benefit describes how the decision changes
#' for different values of the threshold. The EIB marginalises out the uncertainty,
#' and does not incorporate and describe explicitly the uncertainty in the outcomes.
#' To overcome this problem the tool of choice is the CEAC.
#' 
#' @param ib Incremental benefit
#' @return Array with dimensions (interv x k)
#' @seealso \code{\link{ceac.plot}}, \code{\link{compute_CEAC}}, \code{\link{compute_IB}}
#' 
#' @export
#'  
compute_EIB <- function(ib) {
  
  eib <- apply(ib, 3, function(x) apply(x, 1, mean))
  # eib <- apply(ib, 3, function(x) rowMeans(x))  ##TODO: test
}


#' Compute Ustar Statistic
#' 
#' The maximum utility value among the comparators, indicating which
#' intervention produced the most benefits at each simulation. 
#'
#' @param U Net monetary benefit (sim x k x intervs)
#'
#' @return Array with dimensions (sim x k)
#' 
#' @export
#' 
compute_Ustar <- function(U) {
  
  n_sim <- dim(U)[[1]]
  K <- dim(U)[[2]]
  
  Ustar <- matrix(NA, n_sim, K)
  
  for (i in seq_len(K)) {
    Ustar[, i] <- rowMax(U[, i, ])
  }
  
  Ustar
}


#' Compute Value of Information
#'
#' The difference between the maximum utility computed for the current
#' parameter configuration \eqn{U^*} and the utility of the intervention which
#' is associated with the maximum utility overall.
#' 
#' The value of obtaining additional information on the parameter \eqn{\theta}
#' to reduce the uncertainty in the decisional process.
#' It is defined as:
#' 
#' \deqn{\text{VI}(\theta) := U^*(\theta) - \mathcal{U}^*}
#' 
#' with \eqn{U^*(\theta)} the maximum utility value for the given simulation
#' among all comparators and \eqn{\mathcal{U}^*(\theta)} the expected utility
#' gained by the adoption of the cost-effective intervention.
#' 
#' @param Ustar Maximum utility value (sim x k)
#' @param U Net monetary benefit (sim x k x interv)
#'
#' @return Array with dimensions (sim x k)
#' @seealso \code{\link{compute_ol}}
#' 
#' @export
#' 
compute_vi <- function(Ustar, U) {
  
  n_sim <- dim(U)[[1]]
  K <- dim(U)[[2]]
  vi <- matrix(NA, nrow = n_sim, ncol = K)
  
  for (i in seq_len(K)) {
    vi[, i] <- Ustar[, i] - max(apply(U[, i, ], 2, mean))
  }
  
  vi
}


#' Compute Opportunity Loss
#' 
#' The difference between the maximum utility computed for the current
#' parameter configuration (e.g. at the current simulation) \eqn{U^*} and the current
#' utility of the intervention associated with the maximum utility overall.
#' 
#' In mathematical notation,
#' \deqn{\text{OL}(\theta) := U^*(\theta) - U(\theta^\tau)}
#'
#' where \eqn{\tau} is the intervention associated with the overall maximum utility
#' and \eqn{U^*(\theta)} is the maximum utility value among the comparators in the given simulation.
#' The opportunity loss is a non-negative quantity, since \eqn{U(\theta^\tau)\leq U^*(\theta)}.
#' 
#' In all simulations where the intervention is more
#' cost-effective (i.e. when incremental benefit is positive), then \eqn{OL(\theta) = 0}
#' as there would be no opportunity loss, if the parameter configuration were the
#' one obtained in the current simulation.
#' 
#' @param Ustar Maximum utility value (sim x k)
#' @param U  Net monetary benefit (sim x k x interv)
#' @param best Best intervention for given willingness-to-pay
#'
#' @return Array with dimensions (sim x k)
#' @seealso \code{\link{compute_vi}}
#' 
#' @export
#' 
compute_ol <- function(Ustar,
                       U,
                       best) {
  n_sim <- dim(U)[[1]]
  K <- dim(U)[[2]]
  
  ol <- matrix(NA, nrow = n_sim, ncol = K) 
  
  for (i in seq_len(K)) {
    ol[, i] <- Ustar[, i] - U[, i, best[i]]
  }
  
  ol
}


#
rowMax <- function(dat) apply(dat, 1, max)


#' Compute U Statistic
#'
#' Sample of net monetary benefit for each
#' willingness-to-pay threshold and intervention.
#'
#' @param df_ce Cost-effectiveness dataframe
#' @param k Willingness to pay vector
#'
#' @return Array with dimensions (sim x k x ints)
#' 
#' @export
#' 
compute_U <- function(df_ce, k) {
  
  sims <- sort(unique(df_ce$sim))
  ints <- sort(unique(df_ce$ints))
  
  U_df <-
    data.frame(k = rep(k, each = nrow(df_ce)),
               df_ce) %>% 
    mutate(U = k*eff1 - cost1) %>% 
    arrange(ints, k, sim)
  
  array(U_df$U,
        dim = c(length(sims),
                length(k),
                length(ints)))
}
