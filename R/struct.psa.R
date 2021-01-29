
#' Structural Probability Sensitivity Analysis
#' 
#' Computes the weights to be associated with a set of competing models in
#' order to perform structural PSA.
#' 
#' The model is a list containing the output from either R2jags or R2OpenBUGS /
#' R2WinBUGS for all the models that need to be combined in the model average
#' effect is a list containing the measure of effectiveness computed from the 
#' various models (one matrix with n_sim x n_ints simulations for each model)
#' cost is a list containing the measure of costs computed from the various
#' models (one matrix with n_sim x n_ints simulations for each model).
#' 
#' @param models A list containing the output from either R2jags or
#' R2OpenBUGS/R2WinBUGS for all the models that need to be combined in the
#' model average
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
#' on a discrete grid in the interval \code{[0,Kmax]}. The grid is equal to
#' \code{wtp} if the parameter is given, or composed of \code{501} elements if
#' \code{wtp=NULL} (the default)
#' @param plot A logical value indicating whether the function should produce
#' the summary plot or not
#' 
#' @return List object of bcea object, model weights and DIC
#' @author Gianluca Baio
#' @seealso [bcea()]
#' @references
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' 
#' @export
#' 
struct.psa <- function(models,
                       effect,
                       cost,
                       ref = 1,
                       interventions = NULL,
                       Kmax = 50000,
                       plot = FALSE) {
  
  n.models <- length(models)
  
  if (n.models == 1) {
    stop("NB: Needs at least two models to run structural PSA",
         call. = FALSE)
  }
  d <- w <- numeric()
  mdl <- list()
  
  for (i in seq_len(n.models)) {
    # 1. checks whether each model has been run using JAGS or BUGS    
    if (class(models[[i]]) == "rjags") {
      if (!(requireNamespace("R2jags", quietly = TRUE))) {
        stop ("You need to install the package 'R2jags'.
             Please run in your R terminal:\n install.packages('R2jags')",
              call. = FALSE)
      }
      if (requireNamespace("R2jags", quietly = TRUE)) {
        mdl[[i]] <- models[[i]]$BUGSoutput  # if model is run using R2jags/rjags        
      }
    }
    if (class(models[[i]]) == "bugs") {		# if model is run using R2WinBUGS/R2OpenBUGS
      if (!(requireNamespace("R2OpenBUGS", quietly = TRUE))) {
        stop ("You need to install the package 'R2OpenBUGS'.
             Please run in your R terminal:\n install.packages('R2OpenBUGS')",
              call. = FALSE)
      }
      if (requireNamespace("R2OpenBUGS", quietly = TRUE)) {
        mdl[[i]] <- models[[i]]
      }
    }
    mdl[[i]] <- models[[i]]
    # 2. saves the DIC in the vector d
    d[i] <- mdl[[i]]$DIC
  }
  dmin <- min(d)					# computes the minimum value to re-scale the DICs
  w <- exp(-0.5*(d - dmin))/sum(exp(-0.5*(d - dmin))) 	# Computes the model weights (cfr BMHE)
  
  # Now weights the simulations for the variables of effectiveness and costs in each model
  # using the respective weights, to produce the economic analysis for the average model
  e <- c <- matrix(NA,
                   nrow = dim(effect[[1]])[1],
                   ncol = dim(effect[[1]])[2])
  e <- w[1]*effect[[1]]
  c <- w[1]*cost[[1]]
  
  for (i in 2:n.models) {
    e <- e + w[i]*effect[[i]]
    c <- c + w[i]*cost[[i]]
  }
  
  # perform the economic analysis on the averaged model
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

