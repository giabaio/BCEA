
#' Default function
#'
#' Compute a Bayesian cost-effectiveness analysis of two or more interventions
#'
#' INPUTS:
#' 1. Two objects (`e`,`c`). These can be directly computed in a simulation object `sim` from JAGS/BUGS, 
#'    or derived by postprocessing of `sim` in R. The objects (`e`,`c`) have dimension (`n_sim` x number of 
#'    interventions) and contain n_sim simulated values for the measures of effectiveness and costs 
#'    for each intervention being compared. 
#' 2. The reference intervention as a numeric value. Each intervention is a column in the matrices `e` 
#'    and `c` so if `ref` = 1 the first column is assumed to be associated with the reference intervention. 
#'    Intervention 1 is assumed the default reference. All others are considered comparators.
#' 3. A string vector "interventions" including the names of the interventions. If none is provided 
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
                         plot = FALSE
) {
  
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
  
  # Number of simulations & interventions analysed
  n_sim <- dim(eff)[1]
  n_comparators <- dim(eff)[2]
  
  # Define reference & comparator intervention
  # (different labels can be given here if available!)
  
  ints <- 1:n_comparators
  
  if (is.null(interventions))
  { interventions <- paste("intervention", ints) }
  
  # Define intervention i as the reference 
  # where i can be a number in [1,...,n_comparators])
  # and the other(s) as comparator(s)
  # Default is the first intervention (first column of eff or cost)
  
  comp <- ints[-ref]
  n_comparisons <- n_comparators - 1
  
  # Compute Effectiveness & Cost differentials (wrt to reference intervention)
  ##TODO: is this the wrong way around?...
  
  delta_e <- eff[, ref] - eff[, comp]
  delta_c <- cost[, ref] - cost[, comp]
  
  # Compute and plot CEAC & EIB
  if(!exists("Kmax")){ Kmax <- 50000}
  
  # Lets you select the willingness to pay grid
  # useful when doing EVPPI (computationally intensive)
  if (!is.null(wtp)) {
    wtp <- sort(unique(wtp))
    npoints <- length(wtp) - 1
    Kmax <- max(wtp)
    step <- NA
    k <- wtp                 ##TODO: this is potential issue k and K similar? whats wrong with using wtp?
    K <- npoints + 1
  } else {
    npoints <- 500  ##TODO: magic number?
    step <- Kmax/npoints
    k <- seq(0, Kmax, by = step)
    K <- length(k)
  }

  
  deltas <-
    data.frame(
      sim = 1:n_sim,
      comp = rep(1:n_comparisons, each = n_sim),
      delta_e = matrix(delta_e, ncol = 1),
      delta_c = matrix(delta_c, ncol = 1))
  
  ICER <- compute_ICER(deltas)
  
  ib <- compute_IB(deltas, k)
  
  ceac <- compute_CEAC(ib)

  eib <- compute_EIB(ib)

  best <- best_interv_given_k(eib, ref, comp)
  
  kstar <- min(k[best != ref])  # find k when optimal decision changes
  
  # copmute_U()
  
  ##TODO: replace with:
  # EVPI <- compute_EVPI()
  
  ## same as for ib
  U <- array(rep(eff, K)*rep(k, each = n_sim*n_comparators) - as.vector(cost),
             dim=c(n_sim, n_comparators, K))
  U <- aperm(U, c(1,3,2))
  
  rowMax <- function(x){do.call(pmax, as.data.frame(x))}
  
  Ustar <- vi <- ol <- matrix(NA, n_sim, K) 
  
  for (i in seq_len(K)) {
    Ustar[, i] <- rowMax(U[, i,])
    cmd <- paste("ol[, i] <- Ustar[, i] - U[, i,", best[i], "]", sep = "")
    eval(parse(text = cmd))     
    vi[, i] <- Ustar[, i] - max(apply(U[, i,], 2, mean))
  }
  evi <- colMeans(ol)
  
  
  
  
  
  
  he <- list(
    n_sim = n_sim,
    n_comparators = n_comparators,
    n_comparisons = n_comparisons,
    delta_e = delta_e,
    delta_c = delta_c,
    ICER = ICER,
    Kmax = Kmax,
    k = k,
    ceac = ceac,
    ib = ib,
    eib = eib,
    kstar = kstar,
    best = best,
    U = U,
    vi = vi,
    Ustar = Ustar,
    ol = ol,
    evi = evi,
    interventions = interventions,
    ref = ref,
    comp = comp,
    step = step,
    e = eff,
    c = cost)
  
  he <- structure(he, class = "bcea")
  
  ##TODO: should separate out this really  
  if(plot)
    plot(he)
  
  return(he)
}



