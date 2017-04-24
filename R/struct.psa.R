######Structural PSA#############################################################################
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
