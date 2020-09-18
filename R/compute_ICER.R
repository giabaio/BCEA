
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
  
  comp_names <- comp_names_from_(df_ce)
  
  df_ce %>%
    filter(ints != ref) %>% 
    group_by(ints) %>% 
    summarise(ICER = mean(delta_c)/mean(delta_e)) %>% 
    ungroup() %>% 
    select(ICER) %>%  # required to match current format 
    unlist() %>% 
    setNames(comp_names)
}


#'
comp_names_from_ <- function(df_ce) {
  
  df_ce[, c("ref", "ints", "interv_names")] %>%
  filter(ref != ints) %>%
  distinct() %>%
  arrange(ints) %>% 
  select(interv_names) %>% 
  unlist()
}