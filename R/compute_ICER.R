
#
compute_ICER <- function(deltas) {
  
  deltas %>% 
    group_by(comp) %>% 
    summarise(ICER = mean(delta_c)/mean(delta_e)) %>% 
    ungroup() %>% 
    select(ICER) %>%  # required to match current format 
    unlist() %>% 
    `names<-`(NULL)
}


