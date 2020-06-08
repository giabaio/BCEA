
#' Default function
#'
#' Compute a Bayesian cost-effectiveness analysis of two or more interv_names
#'
#' INPUTS:
#' 1. Two objects (`e`,`c`). These can be directly computed in a simulation object `sim` from JAGS/BUGS, 
#'    or derived by postprocessing of `sim` in R. The objects (`e`,`c`) have dimension (`n_sim` x number of 
#'    interv_names) and contain n_sim simulated values for the measures of effectiveness and costs 
#'    for each intervention being compared. 
#' 2. The reference intervention as a numeric value. Each intervention is a column in the matrices `e` 
#'    and `c` so if `ref` = 1 the first column is assumed to be associated with the reference intervention. 
#'    Intervention 1 is assumed the default reference. All others are considered comparators.
#' 3. A string vector "interv_names" including the names of the interv_names. If none is provided 
#'    then labels each as "intervention1",...,"interventionN".
#' 4. The value `Kmax` which represents the maximum value for the willingness to pay parameter. If none 
#'    is provided, then it is assumed `Kmax` = 50000.
#' 5. A(n optional) vector wtp including the values of the willingness to pay grid. If not specified
#'    then `bcea` will construct a grid of 501 values from 0 to `Kmax`. This option is useful when 
#'    performing intensive computations (e.g. for the EVPPI)
#'
#' @return List of computed values for CE Plane, ICER, EIB, CEAC, EVPI 
#' @export
#'
bcea.default <- function(eff,
                         cost,
                         ref = 1,
                         interventions = NULL,
                         Kmax = 50000,
                         wtp = NULL,
                         plot = FALSE) {
  
  ##TODO: S3 only dispatches on the first argument so how does e and c work? change to list?
  ##TODO: how to check that e and c are the right way round?
  ##TODO: can we dispatch directly on jags/BUGS output?
  ##TODO: there several n.comparator == 1, >1 bits. can we improve this?
  
  ##TODO: why not just reorder so that ref is always first then don't have to use ref and comp?
  
  
  if (!is.matrix(cost) | !is.matrix(eff)) stop("eff and cost must be matrices.") 
  if (ncol(cost) == 1 | ncol(eff) == 1) stop("Require at least 2 comparators.")
  if (!is.null(interventions) & length(interventions) != ncol(eff)) stop("interventions names wrong length.")
  if (any(dim(eff) != dim(cost))) stop("eff and cost are not the same dimensions.")
  
  if (!is.double(ref) | ref < 1 | ref > ncol(eff)) stop("reference is not in available interventions.")
  
  n_sim <- dim(eff)[1]
  n_intervs <- dim(eff)[2]
  
  intervs <- 1:n_intervs
  
  interv_names <- 
    if (is.null(interventions)) {
      paste("intervention", intervs)
    } else {
      interventions}
  
  if (!exists("Kmax")) {Kmax <- 50000}
  
  if (!is.null(wtp)) {
    k <- sort(unique(wtp))
  } else {
    step <- Kmax/500
    k <- seq(0, Kmax, by = step)
  }
  
  # create complete data input dataframe
  
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
    rename(ref = ints) %>% 
    merge(df_ce,
          by = c("ref", "sim"),
          suffixes = c("0", "1"),
          all.x = FALSE) %>% 
    mutate(delta_e = eff0 - eff1,
           delta_c = cost0 - cost1)   ##TODO: is this the wrong way around?...
  
  df_ce$interv_names <- interv_names[df_ce$ints]
  
  
  he <- new_bcea(df_ce, k)
  
  
  ##TODO: should separate out this really  
  if (plot)
    plot(he)
  
  return(he)
}



