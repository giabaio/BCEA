

compute_U <- function() {
  
  ##TODO: should we only have one df with deltas too?
  df_ce <-
    data.frame(
      sim = 1:n_sim,
      comp = rep(1:n_comparitors, each = n_sim),
      eff = matrix(eff, ncol = 1),
      cost = matrix(cost, ncol = 1))
  
  U_df <-
    expand.grid(sim = sims,
                k = k,
                comp = comps) %>%
    merge(df_ce) %>% 
    mutate(U = k*eff - cost) %>% 
    arrange(comp, sim, k)
  
  # check configuration
  array(U_df$U,
        dim = c(length(k),
                length(sims),
                length(comps)))
}



compute_Ustar <- function(n_sim, K, U) {
  
  rowMax <- function(x){do.call(pmax, as.data.frame(x))}
  
  Ustar <- matrix(NA, n_sim, K) 
  
  for (i in seq_len(K)) {
    Ustar[, i] <- rowMax(U[, i, ])
  }
  
  Ustar
}


compute_vi <- function(n_sim, K, Ustar, U) {
  
  vi <- matrix(NA, n_sim, K) 
  
  for (i in seq_len(K)) {
    vi[, i] <- Ustar[, i] - max(apply(U[, i,], 2, mean))
  }
  
  vi
}


compute_ol <- function(n_sim, K, Ustar, U, best) {
  
  ol <- matrix(NA, n_sim, K) 
  
  for (i in seq_len(K)) {
    cmd <- paste("ol[, i] <- Ustar[, i] - U[, i,", best[i], "]", sep = "")
    eval(parse(text = cmd))     
  }
  
  ol
}


evi <- colMeans(ol)



######################

# n_comparisons > 1
res <- 
  bcea(e = cbind(eff, eff[, 2]),
       c = cbind(cost, cost[, 2]))
