
#' Compute Incremental Benefit
#'
#' @param deltas Dataframe of cost and effectiveness deltas
#' @param k Vector of willingness to pay values
#'
#' @import dplyr
#' 
#' @return
#' @export
#'
#' @examples
#' 
compute_IB <- function(deltas, k) {
  
  sims <- unique(deltas$sim)
  comps <- unique(deltas$comp)
  
  ib_df <-
    expand.grid(sim = sims,
                k = k,
                comp = comps) %>%
    merge(deltas) %>%
    mutate(ib = k*delta_e - delta_c) %>%
    arrange(comp, sim, k)
  
  array(ib_df$ib,
        dim = c(length(k),
                length(sims),
                length(comps)))
}


