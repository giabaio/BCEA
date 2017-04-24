######CreateInputs##############################################################################################
CreateInputs <- function(x) {
  # Utility function --- creates inputs for the EVPPI
  # First checks whether the model is run with JAGS or BUGS
  # 1. checks whether each model has been run using JAGS or BUGS or Stan  
  if(class(x)=="rjags") {
    if(!isTRUE(requireNamespace("R2jags",quietly=TRUE))) {
      stop("You need to install the package 'R2jags'. Please run in your R terminal:\n install.packages('R2jags')")
    }
    if (isTRUE(requireNamespace("R2jags",quietly=TRUE))) {
      mdl <- x$BUGSoutput      
    }
  }
  if(class(x)=="bugs") {  	# if model is run using R2WinBUGS/R2OpenBUGS
    if(!isTRUE(requireNamespace("R2OpenBUGS",quietly=TRUE))) {
      stop("You need to install the package 'R2OpenBUGS'. Please run in your R terminal:\n install.packages('R2OpenBUGS')")
    }
    if(isTRUE(requireNamespace("R2OpenBUGS",quietly=TRUE))) {
      mdl <- x
    }
  }
  if (class(x)%in%c("bugs","rjags")) {
    # Defines the inputs matrix  
    inputs <- mdl$sims.matrix
    # If the deviance is computed, then removes it 
    if("deviance"%in%colnames(inputs)) {
      w <- which(colnames(inputs)=="deviance")
      inputs <- inputs[,-w]
    }
  }
  if(class(x)=="stanfit") { # if the model is run with Stan
    inputs <- as.data.frame(x)
    # Removes the likelihood, which is never a relevant parameters for HE evaluation
    inputs <- inputs[,-which(colnames(inputs)=="lp__")]
  }
  # 2. saves the output to a relevant list
  pars <- colnames(data.frame(inputs))
  list(mat=data.frame(inputs),parameters=pars)
}