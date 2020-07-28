
#' Compute Incremental Cost-Effectiveness Ratio
#'
#' Defined as
#' 
#' \deqn{ICER = \Delta_c/\Delta_e}
#'
#' @param df_ce Cost-effectiveness dataframe 
#'
#' @return
#' @export
#'
#' @examples
#' 
compute_ICER <- function(df_ce) {
  
  df_ce %>%
    filter(ints != ref) %>% 
    group_by(ints) %>% 
    summarise(ICER = mean(delta_c)/mean(delta_e)) %>% 
    ungroup() %>% 
    select(ICER) %>%  # required to match current format 
    unlist() %>% 
    setNames(NULL)
}


