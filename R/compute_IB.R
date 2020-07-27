
#' Compute Incremental Benefit
#'
#' Sample of incremental net monetary benefit for each
#' willingness-to-pay threshold and comparator.
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
    
  sims <- unique(df_ce$sim)
  ints <- unique(df_ce$ints)

  df_ce <-
    df_ce %>% 
    filter(ints != ref) %>%
    rename(comps = ints)
  
  ib_df <-
    data.frame(k = rep(k, each = nrow(df_ce)),
               df_ce) %>% 
    mutate(ib = k*delta_e - delta_c) %>%
    arrange(comps, sim, k)
  
  array(ib_df$ib,
        dim = c(length(k),
                length(sims),
                length(ints) - 1))
}


