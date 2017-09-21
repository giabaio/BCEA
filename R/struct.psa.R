######Structural PSA#############################################################################


#' Structural PSA
#' 
#' Computes the weights to be associated with a set of competing models in
#' order to perform structural PSA
#' 
#' 
#' @param models A list containing the output from either R2jags or
#' R2OpenBUGS/R2WinBUGS for all the models that need to be combined in the
#' model average
#' @param effect A list containing the measure of effectiveness computed from
#' the various models (one matrix with n.sim x n.ints simulations for each
#' model)
#' @param cost A list containing the measure of costs computed from the various
#' models (one matrix with n.sim x n.ints simulations for each model)
#' @param ref Defines which intervention is considered to be the reference
#' strategy. The default value \code{ref=1} means that the intervention
#' appearing first is the reference and the other(s) is(are) the comparator(s)
#' @param interventions Defines the labels to be associated with each
#' intervention. By default and if \code{NULL}, assigns labels in the form
#' "Intervention1", ... , "Intervention T"
#' @param Kmax Maximum value of the willingness to pay to be considered.
#' Default value is \code{k=50000}. The willingness to pay is then approximated
#' on a discrete grid in the interval \code{[0,Kmax]}. The grid is equal to
#' \code{wtp} if the parameter is given, or composed of \code{501} elements if
#' \code{wtp=NULL} (the default)
#' @param plot A logical value indicating whether the function should produce
#' the summary plot or not
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references Baio G. (2012). Bayesian Methods in Health Economics.
#' CRC/Chapman Hall, London
#' @export struct.psa
struct.psa <- function(models,effect,cost,ref=1,interventions=NULL,Kmax=50000,plot=F) {
  # Computes the weights to be associated with a set of competing models in order to
  #   perform structural PSA
  # model is a list containing the output from either R2jags or R2OpenBUGS/R2WinBUGS
  #   for all the models that need to be combined in the model average
  # effect is a list containing the measure of effectiveness computed from the 
  #   various models (one matrix with n.sim x n.ints simulations for each model)
  # cost is a list containing the measure of costs computed from the 
  #   various models (one matrix with n.sim x n.ints simulations for each model)
  
  n.models <- length(models)  # number of models to be combined
  if(n.models==1) {
    stop("NB: Needs at least two models to run structural PSA")
  }
  d <- w <- numeric()		# initialises the relevant vectors
  mdl <- list()		     	# and list
  for (i in 1:n.models) {
    # 1. checks whether each model has been run using JAGS or BUGS    
    if(class(models[[i]])=="rjags") {
      if(!isTRUE(requireNamespace("R2jags",quietly=TRUE))) {
        stop("You need to install the package 'R2jags'. Please run in your R terminal:\n install.packages('R2jags')")
      }
      if (isTRUE(requireNamespace("R2jags",quietly=TRUE))) {
        mdl[[i]] <- models[[i]]$BUGSoutput  # if model is run using R2jags/rjags        
      }
    }
    if(class(models[[i]])=="bugs") {		# if model is run using R2WinBUGS/R2OpenBUGS
      if(!isTRUE(requireNamespace("R2OpenBUGS",quietly=TRUE))) {
        stop("You need to install the package 'R2OpenBUGS'. Please run in your R terminal:\n install.packages('R2OpenBUGS')")
      }
      if(isTRUE(requireNamespace("R2OpenBUGS",quietly=TRUE))) {
        mdl[[i]] <- models[[i]]
      }
    }
    mdl[[i]] <- models[[i]]
    # 2. saves the DIC in the vector d
    d[i] <- mdl[[i]]$DIC
  }
  dmin <- min(d)					# computes the minimum value to re-scale the DICs
  w <- exp(-.5*(d-dmin))/sum(exp(-.5*(d-dmin))) 	# Computes the model weights (cfr BMHE)
  
  # Now weights the simulations for the variables of effectiveness and costs in each model
  # using the respective weights, to produce the economic analysis for the average model
  e <- c <- matrix(NA,dim(effect[[1]])[1],dim(effect[[1]])[2])
  e <- w[1]*effect[[1]]
  c <- w[1]*cost[[1]]
  for (i in 2:n.models) {
    e <- e + w[i]*effect[[i]]
    c <- c + w[i]*cost[[i]]
  }
  
  # Now performs the economic analysis on the averaged model
  he <- bcea(e=e,c=c,ref=ref,interventions=interventions,Kmax=Kmax,plot=plot)
  
  # And finally saves the results
  list(he=he,w=w,DIC=d)
}
