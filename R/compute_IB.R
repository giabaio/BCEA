
#' Compute Incremental Benefit
#'
#' Sample of incremental net monetary benefit for each
#' willingness-to-pay threshold, \eqn{k}, and comparator.
#' 
#' Defined as:
#' 
#' \deqn{IB = u(e,c; 1) - u(e,c; 0)}
#' 
#' If the net benefit function is used as utility function,
#' the definition can be re-written as
#' 
#' \deqn{IB = k\cdot\Delta_e - \Delta_c}.
#'
#' @param df_ce Dataframe of cost and effectiveness deltas
#' @param k Vector of willingness to pay values
#'
#' @import dplyr
#' 
#' @return Array with dimensions (k x sim x ints)
#' 
#' @export
#' @seealso \code{\link{compute_EIB}}
#'
compute_IB <- function(df_ce, k) {
    
  sims <- unique(df_ce$sim)
  ints <- unique(df_ce$ints)
  comp_names <- comp_names_from_(df_ce)
  
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
                length(ints) - 1),
        dimnames = list(k = NULL,
                        sims = NULL,
                        ints = comp_names))
}


