
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
                         k = NULL,
                         plot = FALSE, ...) {
  exArgs <- list(...)
  
  # provide named reference
  if (is.character(ref)) {
    if (length(ref) > 1 || !ref %in% interventions) {
      ref <- NULL
    } else {
      ref <- which(ref == interventions)
    }
  }
  
  if (is.null(ref)) {
    ref <- 1
    message("No reference selected. Defaulting to first intervention.")
  }
  
  if (!is.null(k) && length(k) == 1)
    message("k should be a vector of length > 1, otherwise plots will be empty.")
  
  
  if (exists("wtp", exArgs)) {
    message("wtp argument soft deprecated. Please use k instead in future.")
    k <- exArgs$wtp
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
  
  if (!is.null(k)) {
    k <- sort(unique(k))
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
    rename(ref = "ints") %>% 
    merge(df_ce,
          by = c("ref", "sim"),
          suffixes = c("0", "1"),
          all.x = FALSE) %>% 
    mutate(delta_e = .data$eff0 - .data$eff1,
           delta_c = .data$cost0 - .data$cost1)
  
  df_ce$interv_names <- factor(interv_names[df_ce$ints],
                               levels = interv_names) 
  
  # Arranges the dataset so that it can be operated upon using pivot_wider
  # NB: Needs to arrange by both sim and interv_names to ensure that
  #     the resulting data are in the same order when changing the 
  #     reference intervention using one of the setters. Needs to be
  #     explicit in defining the variables by which to sort!
  df_ce <- df_ce |> arrange(.data$sim, .data$ints) 

  he <- new_bcea(df_ce, k)
  
  he <- setComparisons(he, .comparison)
  
  ##TODO: should separate out this really  
  if (plot)
    plot(he)
  
  return(he)
}

### These methods are removed because they imply dependencies on `rstan` and `rjags`, which can be expensive and not very helpful...
# #' @rdname bcea
# #' @param ... Additional arguments
# #' @importFrom MCMCvis MCMCchains
# #' @export
# bcea.rjags <- function(eff, ...) {
#   
#   cost <-
#     MCMCvis::MCMCchains(eff, params = "cost")
#   eff <- 
#     MCMCvis::MCMCchains(eff, params = "eff")
#   bcea.default(eff, cost, ...)
# }
# 
# 
# #' @rdname bcea
# #' @param ... Additional arguments
# #' @export
# bcea.rstan <- function(eff, ...) {
#   
#   # check if rstan installed
#   if (!requireNamespace("rstan", quietly = TRUE)) {
#     stop(
#       "The 'rstan' package is required to process stanfit objects.\n",
#       "Please install it with: install.packages('rstan')",
#       call. = FALSE
#     )
#   }
#   
#   # if installed
#   cost_list <- rstan::extract(eff, "cost")
#   eff_list <- rstan::extract(eff, "eff")
#   
#   bcea.default(as.matrix(eff_list[[1]]), as.matrix(cost_list[[1]]), ...)
# }
# 
# 
# #' @rdname bcea
# #' @param ... Additional arguments
# #' @export
# bcea.bugs <- function(eff, ...) {
#   
#   cost <- eff$sims.list$cost
#   eff <- eff$sims.list$eff
#   bcea.default(eff, cost, ...)
# }