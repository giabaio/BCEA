
#' Constructor for bcea
#'
#' @param df_ce dataframe of all simulation eff and cost
#' @param k vector of willingness to pay values
#'
#' @import reshape2, dplyr
#'
#' @return
#' @export
#'
new_bcea <- function(df_ce, k) {
  
  K <- length(k)
  n_sim <- length(unique(df_ce$sim))
  ref <- unique(df_ce$ref)
  comp <- (1:max(df_ce$ints))[-ref]
  df_ce_comp <- df_ce %>% filter(ints != ref)
  
  ICER <- compute_ICER(df_ce)
  
  ib <- compute_IB(df_ce, k)
  
  ceac <- compute_CEAC(ib)
  
  eib <- compute_EIB(ib)
  
  best <- best_interv_given_k(eib, ref, comp)
  
  kstar <- compute_kstar(k, best, ref)
  
  U <- compute_U(df_ce, k)
  
  Ustar <- compute_Ustar(n_sim, K, U)
  
  vi <- compute_vi(n_sim, K, Ustar, U)
  
  ol <- compute_ol(n_sim, K, Ustar, U, best)
  
  evi <- colMeans(ol)
  
  he <- 
    list(n_sim = length(unique(df_ce$sim)),
         n_comparators = length(comp) + 1,
         n_comparisons = length(comp),
         delta_e = dcast(sim ~ interv_names,
                         value.var = "delta_e",
                         data = df_ce_comp)[, -1],
         delta_c = dcast(sim ~ interv_names,
                         value.var = "delta_c",
                         data = df_ce_comp)[, -1],
         ICER = ICER,
         Kmax = max(k),
         k = k,
         ceac = ceac,
         ib = ib,
         eib = eib,
         kstar = kstar,
         best = best,
         U = U,
         vi = vi,
         Ustar = Ustar,
         ol = ol,
         evi = evi,
         interventions = sort(unique(df_ce$interv_names)),
         ref = ref,
         comp = comp,
         step = k[2] - k[1],
         e = dcast(sim ~ interv_names,
                   value.var = "eff1",
                   data = df_ce)[, -1],
         c = dcast(sim ~ interv_names,
                   value.var = "cost1",
                   data = df_ce)[, -1])
  
  structure(he, class = "bcea")
}