
#' @rdname bcea
#' 
#' @import dplyr
#' 
#' @export
#'
bcea.default <- function(eff,
                         cost,
                         ref = NULL,
                         interventions = NULL,
                         .comparison = NULL,
                         Kmax = 50000,
                         wtp = NULL,
                         plot = FALSE) {
  
  if (is.null(ref)) {
    ref <- 1
    message("No reference selected. Defaulting to first intervention.")  
  }
  
  validate_bcea(eff, cost, ref, interventions)
  
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
           delta_c = .data$cost0 - .data$cost1)
  
  df_ce$interv_names <- factor(interv_names[df_ce$ints],
                               levels = interv_names)
  
  he <- new_bcea(df_ce, k)
  
  he <- setComparisons(he, .comparison)
  
  ##TODO: should separate out this really  
  if (plot)
    plot(he)
  
  return(he)
}


#' @rdname bcea
#' @importFrom MCMCvis MCMCchains
#' @export
bcea.rjags <- function(eff, ...) {
  
  cost <-
    MCMCvis::MCMCchains(eff, params = "cost")
  eff <- 
    MCMCvis::MCMCchains(eff, params = "eff")
  bcea.default(eff, cost, ...)
}


#' @rdname bcea
#' @importFrom rstan extract
#' @export
bcea.rstan <- function(eff, ...) {
  
  cost <- rstan::extract(eff, "cost")
  eff <- rstan::extract(eff, "eff")
  bcea.default(as.matrix(eff[[1]]), as.matrix(cost[[1]]), ...)
}


#' @rdname bcea
#' @export
bcea.bugs <- function(eff, ...) {
  
  cost <- eff$sims.list$cost
  eff <- eff$sims.list$eff
  bcea.default(eff, cost, ...)
}

