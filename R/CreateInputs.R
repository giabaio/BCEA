######CreateInputs##############################################################################################


#' CreateInputs
#' 
#' Creates an object containing the matrix with the parameters simulated using
#' the MCMC procedure (using JAGS, BUGS or Stan) and a vector of parameters
#' (strings) that can be used to perform the expected value of partial
#' information analysis. In the process, \code{CreateInputs} also checks for
#' linear dependency among columns of the PSA samples or columns having
#' constant values and removes them to only leave the fundamental parameters
#' (to run VoI analysis). This also deals with simulations stored in a
#' \code{.csv} or \code{.txt} file (eg as obtained using bootstrapping from a
#' non-Bayesian model)
#' 
#' 
#' @param x A \code{rjags}, \code{bugs} or \code{stanfit} object, containing
#' the results of a call to either \code{jags}, (under \code{R2jags}), bugs
#' (under \code{R2WinBUGS} or \code{R2OpenBUGS}), or \code{stan} (under
#' \code{rstan}).
#' @return \item{mat}{A data.frame contaning all the simulations for all the
#' monitored parameters} \item{parameters}{A character vectors listing the
#' names of all the monitored parameters}
#' @author Gianluca Baio and Mark Strong
#' @seealso \code{\link{bcea}}, \code{\link{evppi}}
#' @keywords R2jags R2WinBUGS R2OpenBUGS
#' @export CreateInputs
CreateInputs <- function(x) {
   # Utility function --- creates inputs for the EVPPI
   if(class(x)=="rjags") {
      inputs <- x$BUGSoutput$sims.matrix
   }
   if(class(x)=="bugs") {
      inputs <- x$sims.matrix
   }
   if(class(x)=="stanfit") { 
      inputs <- x
   }
   if(class(x)%in%c("data.frame","matrix","numeric")) { 
      inputs <- x
   }

   # Removes the deviance (which is not relevant for VOI computations
   if (class(x)%in%c("bugs","rjags")) {
      if("deviance"%in%colnames(inputs)) {
        inputs <- inputs[,-which(colnames(inputs)=="deviance")]
      }
      else {
         if(class(x)=="stanfit") {
            inputs <- inputs[,-which(colnames(inputs)=="lp__")]
         }
      }
   }
   
   # Now removes redundant parameters (linear combination of columns or columns that are constant)
   # Code by Mark Strong
   sets=colnames(inputs)
   constantParams <- (apply(inputs, 2, var) == 0)
   if (sum(constantParams) > 0) sets <- sets[-which(constantParams)] # remove constants
   paramSet <- cbind(cbind(inputs)[, sets, drop=FALSE]) # now with constants removed
   rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
   while(length(unique(rankifremoved)) > 1) {
      linearCombs <- which(rankifremoved == max(rankifremoved))
      print(linearCombs)
      print(paste("Linear dependence: removing column", colnames(paramSet)[max(linearCombs)]))
      paramSet <- cbind(paramSet[, -max(linearCombs), drop=FALSE])
      rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
   }
   while(qr(paramSet)$rank == rankifremoved[1]) {
      print(paste("Linear dependence... removing column", colnames(paramSet)[1]))
      paramSet <- cbind(paramSet[, -1, drop=FALSE]) # special case only lincomb left
      rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
   }

   # Now saves the output to a relevant list
   list(mat=data.frame(paramSet),parameters=colnames(data.frame(paramSet)))
}

