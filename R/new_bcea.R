
#' Constructor for bcea
#'
#' @param df_ce Dataframe of all simulation eff and cost
#' @param k Vector of willingness to pay values
#'
#' @import reshape2 dplyr
#' @importFrom rlang int
#'
#' @return List object of class bcea.
#' @seealso [bcea()]
#' 
#' @export
#'
new_bcea <- function(df_ce, k) {
  
  # K <- length(k)
  ref <- unique(df_ce$ref)
  comp <- (1:max(df_ce$ints))[-ref]
  df_ce_comp <- df_ce %>% filter(.data$ints != ref)
  
  ICER <- compute_ICER(df_ce)
  
  ib <- compute_IB(df_ce, k)
  
  ceac <- compute_CEAC(ib)
  
  eib <- compute_EIB(ib)
  
  best <- best_interv_given_k(eib, ref, comp)
  
  kstar <- compute_kstar(k, best, ref)
  
  U <- compute_U(df_ce, k)
  
  Ustar <- compute_Ustar(U)
  
  vi <- compute_vi(Ustar, U)
  
  ol <- compute_ol(Ustar, U, best)
  
  evi <- compute_EVI(ol)
  
  interv_names <- levels(df_ce$interv_names)
  
  e_dat <-
    reshape2::dcast(sim ~ interv_names,
                    value.var = "eff1",
                    data = df_ce)[, -1]
  
  c_dat <-
    reshape2::dcast(sim ~ interv_names,
                    value.var = "cost1",
                    data = df_ce)[, -1]
  
  
  delta_e <- 
    reshape2::dcast(sim ~ interv_names,
                    value.var = "delta_e",
                    data = df_ce_comp)[, -1, drop = FALSE]
  
  delta_c <- 
    reshape2::dcast(sim ~ interv_names,
                    value.var = "delta_c",
                    data = df_ce_comp)[, -1, drop = FALSE] 

  he <- 
    list(n_sim = length(unique(df_ce$sim)),
         n_comparators = length(comp) + 1,
         n_comparisons = length(comp),
         delta_e = delta_e,
         delta_c = delta_c,
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
         ref = ref,
         comp = comp,
         step = k[2] - k[1],
         interventions = interv_names,
         e = as.matrix(e_dat),
         c = as.matrix(c_dat))
  
  structure(he, class = c("bcea", class(he)))
}

