
#' Structural Probability Sensitivity Analysis
#' 
#' Computes the weights to be associated with a set of competing models in
#' order to perform structural PSA.
#' 
#' The model is a list containing the output from either R2jags or
#' R2WinBUGS for all the models that need to be combined in the model average
#' effect is a list containing the measure of effectiveness computed from the 
#' various models (one matrix with n_sim x n_ints simulations for each model)
#' cost is a list containing the measure of costs computed from the various
#' models (one matrix with n_sim x n_ints simulations for each model).
#' 
#' @param models A list containing the output from either R2jags or
#' R2WinBUGS for all the models that need to be combined in the model average
#' @param effect A list containing the measure of effectiveness computed from
#' the various models (one matrix with n.sim x n.ints simulations for each
#' model)
#' @param cost A list containing the measure of costs computed from the various
#' models (one matrix with n.sim x n.ints simulations for each model)
#' @param ref Which intervention is considered to be the reference
#' strategy. The default value \code{ref=1} means that the intervention
#' appearing first is the reference and the other(s) is(are) the comparator(s)
#' @param interventions Defines the labels to be associated with each
#' intervention. By default and if \code{NULL}, assigns labels in the form
#' "Intervention1", ... , "InterventionT"
#' @param Kmax Maximum value of the willingness to pay to be considered.
#' Default value is \code{k=50000}. The willingness to pay is then approximated
#' on a discrete grid in the interval \code{[0, Kmax]}. The grid is equal to
#' \code{wtp} if the parameter is given, or composed of \code{501} elements if
#' \code{wtp=NULL} (the default)
#' @param plot A logical value indicating whether the function should produce
#' the summary plot or not
#' 
#' @return List object of bcea object, model weights and DIC
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' 
#' @export
#' 
struct.psa <- function(models,
                       effect,
                       cost,
                       ref = NULL,
                       interventions = NULL,
                       Kmax = 50000,
                       plot = FALSE) {
  
  if (is.null(ref)) {
    ref <- 1
    message("No reference selected. Defaulting to first intervention.")  
  }
  
  n_models <- length(models)
  
  if (n_models == 1) {
    stop("Need at least two models to run structural PSA",
         call. = FALSE)
  }
  d <- w <- numeric()
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
    mdl[[i]] <- models[[i]]
    d[i] <- mdl[[i]]$DIC
  }
  
  dmin <- min(d)					                        # minimum value to re-scale DICs
  w <- exp(-0.5*(d - dmin))/sum(exp(-0.5*(d - dmin))) 	# model weights (cfr BMHE)
  
  # weights the simulations for the variables of effectiveness and costs in each model
  # using the respective weights, to produce the economic analysis for the average model
  e <- c <- matrix(NA,
                   nrow = dim(effect[[1]])[1],
                   ncol = dim(effect[[1]])[2])
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
  
  list(he = he,
       w = w,
       DIC = d)
}

