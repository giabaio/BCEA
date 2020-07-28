
#' Compute kstar
#'
#' Find willingness-to-pay threshold when optimal decision changes.
#'  
#' @param k Willingness-to-pay vector
#' @param best Best intervention for each k
#' @param ref Reference intervention
#'
#' @return kstar
#' 
compute_kstar <- function(k, best, ref) {
  
  if (all(best == ref)) {
    return(NA)    
  }
  
  min(k[best != ref])
}


#' Compute Cost-Effectiveness Acceptability Curve
#'
compute_CEAC <- function(ib) {
  
  apply(ib > 0, c(1,3), mean)
}


#' Compute Expected Incremental Benefit
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
#' @return Ustar (sim x k)
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
#' parameter configuration U* and the utility of the intervention which
#' is associated with the maximum utility overall.
#' 
#' For each row of the simulations table, the
#' VI is computed as the difference between the current value of \eqn{U^*} and the
#' mean of the entire vector U2. Negative values of the VI imply that for
#' those simulation specific parameter values treatment options are less
#' valuable than the current optimal decision.
#'   
#' @param Ustar Maximum utility value
#' @param U Net monetary benefit
#'
#' @return vi
#' 
compute_vi <- function(Ustar, U) {
  
  n_sim <- dim(U)[[1]]
  K <- dim(U)[[2]]
  
  vi <- matrix(NA, n_sim, K)
  
  for (i in seq_len(K)) {
    vi[, i] <- Ustar[, i] - max(apply(U[, i, ], 2, mean))
  }
  
  vi
}


#' Compute Opportunity Loss
#' 
#' Obtained as the difference between the maximum utility computed for the current
#' parameter configuration (e.g. at the current simulation) \eqn{U^*} and the current
#' utility of the intervention associated with the maximum utility overall.
#' 
#' In mathematical notation,
#' \deqn{OL(\mathbf{\theta}) = U^*(\mathbf{\theta}) - U(\mathbf{\theta}^{\tau})}
#'  
#' In all simulations where vaccination is indeed more
#' cost-effective (i.e. when incremental benefit is positive), then \eqn{OL(\theta) = 0}
#' as there would be no opportunity loss, if the parameter configuration were the
#' one obtained in the current simulation.
#' 
#' @param Ustar Maximum utility value
#' @param U  Net monetary benefit
#' @param best Best intervention for given willingness-to-pay
#'
#' @return array with dimensions (sim x k)
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
#' @param df_ce cost-effectiveness dataframe
#' @param k Willingness to pay vector
#'
#' @return U (sim x k x ints)
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
