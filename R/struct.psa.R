
#' Structural Probability Sensitivity Analysis
#' 
#' Computes the weights to be associated with a set of competing models in
#' order to perform structural PSA.
#' 
#' The model is a list containing the output from either R2jags or
#' R2WinBUGS for all the models that need to be combined in the model average
#' effect is a list containing the measure of effectiveness computed from the 
#' various models (one matrix with `n_sim` x `n_ints` simulations for each model)
#' cost is a list containing the measure of costs computed from the various
#' models (one matrix with `n_sim` x `n_ints` simulations for each model).
#' 
#' @param models A (possibly named) list containing the output from either 
#' R2jags or R2WinBUGS for all the models that need to be combined in the model 
#' average 
#' @param effect A list containing the measure of effectiveness computed from
#' the various models (one matrix with `n_sim` x `n_ints` simulations for each
#' model)
#' @param cost A list containing the measure of costs computed from the various
#' models (one matrix with `n_sim` x `n_ints` simulations for each model)
#' @param ref Which intervention is considered to be the reference
#' strategy. The default value `ref=1` means that the intervention
#' appearing first is the reference and the other(s) is(are) the comparator(s)
#' @param interventions Defines the labels to be associated with each
#' intervention. By default and if `NULL`, assigns labels in the form
#' "Intervention1", ... , "InterventionT"
#' @param Kmax Maximum value of the willingness to pay to be considered.
#' Default value is `50000`. The willingness to pay is then approximated
#' on a discrete grid in the interval `[0, Kmax]`. The grid is equal to
#' `k` if the parameter is given, or composed of `501` elements if
#' `k=NULL` (the default)
#' @param plot A logical value indicating whether the function should produce
#' the summary plot or not
#' @param w A vector of weights. By default it's `NULL` to indicate that the 
#' function will calculate the model weights based on DIC and the individual
#' model fit. This behaviour can be overridden by passing a vector `w`, 
#' for instance based on expert opinion
#' 
#' @return List object of bcea object, model weights and DIC
#' @author Gianluca Baio
#' @seealso [bcea()]
#' @importFrom Rdpack reprompt
#' 
#' @references
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @export
#' 
#' @examples
#' 
#' # load sample jags output
#' data("statins_base")
#' data("statins_HC")
#' 
#' interventions <- c("Atorvastatin", "Fluvastatin",
#'                    "Lovastatin", "Pravastatin",
#'                    "Rosuvastatin", "Simvastatin")
#' 
#' m1 <- bcea(eff = statins_base$sims.list$effect,
#'            cost = statins_base$sims.list$cost.tot,
#'            ref = 1, interventions = interventions)
#' 
#' m2 <- bcea(eff = statins_HC$sims.list$effect,
#'            cost = statins_HC$sims.list$cost.tot,
#'            ref = 1, interventions = interventions)
#' 
#' models <- list("Base"=statins_base, "Half Cauchy"=statins_HC)
#' 
#' effects <- list(statins_base$sims.list$effect,
#'                 statins_HC$sims.list$effect)
#' costs <- list(statins_base$sims.list$cost.tot,
#'               statins_HC$sims.list$cost.tot)
#' 
#' \dontrun{
#' m3 <- struct.psa(models, effects, costs,
#'                  ref = 1, interventions = interventions)
#' }
#' 
struct.psa <- function(models,
                       effect,
                       cost,
                       ref = NULL,
                       interventions = NULL,
                       Kmax = 50000,
                       plot = FALSE,
                       w = NULL) {
  
  if (is.null(ref)) {
    ref <- 1
    message("No reference selected. Defaulting to first intervention.")  
  }
  
  n_models <- length(models)
  
  if (n_models == 1) {
    stop("Need at least two models to run structural PSA",
         call. = FALSE)
  }
  d <- numeric()
  mdl <- list()
  
  for (i in seq_len(n_models)) {
    # 1. checks whether each model has been run using JAGS or BUGS    
    if (inherits(models[[i]], "rjags")) {
      mdl[[i]] <- models[[i]]$BUGSoutput
    }
    if (inherits(models[[i]], "bugs")) {
      mdl[[i]] <- models[[i]]
    }
    
    # 2. saves the DIC in vector d
    d[i] <- mdl[[i]]$DIC
  }
  
  dmin <- min(d)   # minimum value to re-scale DICs
  delta_dic <- abs(dmin - d)
  
  # Only compute w using DIC if the user hasn't passed a suitable value 
  if (is.null(w)) {
    w <- exp(-0.5*(delta_dic))/sum(exp(-0.5*(delta_dic))) # model weights (cfr BMHE)
  } 
  if ((!is.null(w) && length(w) != n_models)) {
    stop("If you are considering user-defined weights, you must pass a vector whose
         length is the same as the number of models to average!")
  }
  
  # weights the simulations for the variables of effectiveness and costs in each model
  # using the respective weights, to produce the economic analysis for the average model
  e <- w[1]*effect[[1]]
  c <- w[1]*cost[[1]]
  
  for (i in 2:n_models) {
    e <- e + w[i]*effect[[i]]
    c <- c + w[i]*cost[[i]]
  }
  
  # perform economic analysis on averaged model
  he <-
    bcea(eff = e,
         cost = c,
         ref = ref,
         interventions = interventions,
         Kmax = Kmax,
         plot = plot)
  
  # Gives names to objects (if not available, does nothing...)
  names(w)=names(d)=names(models)
  res <- 
    c(he, list(w = w, DIC = d))
  
  structure(res, class = c("struct.psa", class(he)))
}

