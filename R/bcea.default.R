
#' @rdname bcea
#' 
#' @import dplyr
#' 
#' @export
#'
bcea.default <- function(eff,
                         cost,
                         ref = 1,
                         interventions = NULL,
                         .comparison = NULL,
                         Kmax = 50000,
                         wtp = NULL,
                         plot = FALSE) {
  
  ##TODO: S3 only dispatches on the first argument so how does e and c work? change to list?
  ##      in fact why is this S3?
  ##TODO: how to check that e and c are the right way round?
  ##TODO: can we dispatch directly on jags/BUGS output?
  
  
  if (!is.matrix(cost) | !is.matrix(eff))
    stop("eff and cost must be matrices.", call. = FALSE)
  
  if (ncol(cost) == 1 | ncol(eff) == 1)
    stop("Require at least 2 comparators.", call. = FALSE)
  
  if (!is.null(interventions) & length(interventions) != ncol(eff))
    stop("interventions names wrong length.", call. = FALSE)
  
  if (any(dim(eff) != dim(cost)))
    stop("eff and cost are not the same dimensions.", call. = FALSE)
  
  if (!is.numeric(ref) | ref < 1 | ref > ncol(eff))
    stop("reference is not in available interventions.", call. = FALSE)
  
  n_sim <- dim(eff)[1]
  n_intervs <- dim(eff)[2]
  
  intervs <- 1:n_intervs
  
  interv_names <- 
    if (is.null(interventions)) {
      paste("intervention", intervs)
    } else {
      as.factor(interventions)}
  
  if (!is.null(wtp)) {
    k <- sort(unique(wtp))
  } else {
    step <- Kmax/500
    k <- seq(0, Kmax, by = step)
  }
  
  df_ce <-
    data.frame(
      sim = 1:n_sim,
      ref = ref,
      ints = rep(intervs, each = n_sim),
      eff = matrix(eff, ncol = 1),
      cost = matrix(cost, ncol = 1))
  
  df_ce <- 
    df_ce %>%
    select(-ref) %>% 
    rename(ref = .data$ints) %>% 
    merge(df_ce,
          by = c("ref", "sim"),
          suffixes = c("0", "1"),
          all.x = FALSE) %>% 
    mutate(delta_e = .data$eff0 - .data$eff1,
           delta_c = .data$cost0 - .data$cost1)   ##TODO: is this the wrong way around?...
  
  df_ce$interv_names <- factor(interv_names[df_ce$ints],
                               levels = interv_names)
  
  he <- new_bcea(df_ce, k)
  
  he <- setComparisons(he, .comparison)
  
  ##TODO: should separate out this really  
  if (plot)
    plot(he)
  
  return(he)
}
