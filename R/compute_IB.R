
#' Compute Incremental Benefit
#'
#' @param df_ce Dataframe of cost and effectiveness deltas
#' @param k Vector of willingness to pay values
#'
#' @import dplyr
#' 
#' @return
#' @export
#'
#' @examples
#' 
compute_IB <- function(df_ce, k) {

  df_ce <-
    df_ce %>% 
    filter(ints != ref) %>%
    rename(comps = ints)
    
  sims <- unique(df_ce$sim)
  comps <- unique(df_ce$comps)
  
  ib_df <-
    expand.grid(sim = sims,
                k = k,
                comps = comps) %>%
    merge(df_ce) %>%
    mutate(ib = k*delta_e - delta_c) %>%
    arrange(comps, sim, k)
  
  array(ib_df$ib,
        dim = c(length(k),
                length(sims),
                length(comps)))
}


