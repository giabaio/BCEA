
#
compute_CEAC <- function(ib) {
  
  apply(ib > 0, c(1,3), mean)
}

# Compute Expected Incremental Benefit
#
compute_EIB <- function(ib) {
  
  eib <- apply(ib, 3, function(x) apply(x, 1, mean))
  # eib <- apply(ib, 3, function(x) rowMeans(x))  ##TODO: test
}


#' Compute Ustar statistic
#'
#' @param n_sim 
#' @param K 
#' @param U 
#'
#' @return
#' @export
#'
#' @examples
#' 
compute_Ustar <- function(n_sim, K, U) {
  
  Ustar <- matrix(NA, n_sim, K)
  
  for (i in seq_len(K)) {
    Ustar[, i] <- rowMax(U[, i, ])
  }
  
  Ustar
}


#' Compute Value of Information
#'
#' @param n_sim 
#' @param K 
#' @param Ustar 
#' @param U 
#'
#' @return
#' @export
#'
#' @examples
#' 
compute_vi <- function(n_sim,
                       K,
                       Ustar,
                       U) {
  
  vi <- matrix(NA, n_sim, K)
  
  for (i in seq_len(K)) {
    vi[, i] <- Ustar[, i] - max(apply(U[, i,], 2, mean))
  }
  
  vi
}


#' Compute ol
#'
#' @param n_sim 
#' @param K 
#' @param Ustar 
#' @param U 
#' @param best 
#'
#' @return
#' @export
#'
#' @examples
#' 
compute_ol <- function(n_sim,
                       K,
                       Ustar,
                       U,
                       best) {
  
  ol <- matrix(NA, n_sim, K) 
  
  ##TODO: is there a clearer way of doing this?
  for (i in seq_len(K)) {
    cmd <- paste("ol[, i] <- Ustar[, i] - U[, i,", best[i], "]", sep = "")
    eval(parse(text = cmd))     
  }
  
  ol
}


#
rowMax <- function(dat) apply(dat, 1, max)


#' Compute U statistic
#'
#' @param df_ce 
#' @param k Willingness to pay vector
#'
#' @return
#' @export
#'
#' @examples
#' 
compute_U <- function(df_ce, k) {
  
  sims <- sort(unique(df_ce$sim))
  ints <- sort(unique(df_ce$ints))
  
  U_df <-
    expand.grid(sim = sims,
                k = k,
                ints = ints) %>%
    merge(df_ce) %>% 
    mutate(U = k*eff1 - cost1) %>% 
    arrange(ints, k, sim)
  
  array(U_df$U,
        dim = c(length(sims),
                length(k),
                length(ints)))
}
