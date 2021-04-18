
#' @rdname createInputs
#' main method
#' 
#' @param inputs matrix
#' @export
#' @examples
#' 
#' data(Vaccine)
#' 
#' # jags
#' inp <- createInputs(vaccine)
#' 
createInputs.default <- function(inputs, print.lincom = TRUE) {
  
  # Now removes redundant parameters (linear combination of columns or columns that are constant)
  # Code by Mark Strong
  sets <- colnames(inputs)
  constantParams <- (apply(inputs, 2, var) == 0)
  
  if (sum(constantParams) > 0) sets <- sets[-which(constantParams)] # remove constants
  
  paramSet <- cbind(cbind(inputs)[, sets, drop=FALSE]) # now with constants removed
  
  rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  
  while (length(unique(rankifremoved)) > 1) {
    linearCombs <- which(rankifremoved == max(rankifremoved))
    
    if (print.lincom == TRUE) {
      print(linearCombs)
      print(paste("Linear dependence: removing column",
                  colnames(paramSet)[max(linearCombs)]))
    }
    
    paramSet <- cbind(paramSet[, -max(linearCombs), drop=FALSE])
    rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  }
  
  while (qr(paramSet)$rank == rankifremoved[1]) {
    
    if (print.lincom == TRUE) {
      print(paste("Linear dependence... removing column", colnames(paramSet)[1]))
    }
    paramSet <- cbind(paramSet[, -1, drop=FALSE]) # special case only lincomb left
    rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  }
  
  list(mat = data.frame(paramSet),
       parameters = colnames(data.frame(paramSet)))
}


#' Create Inputs for EVPI Calculation
#' 
#' Creates an object containing the matrix with the parameters simulated using
#' the MCMC procedure (using JAGS, BUGS or Stan) and a vector of parameters
#' (strings) that can be used to perform the expected value of partial
#' information analysis. In the process, \code{CreateInputs} also checks for
#' linear dependency among columns of the PSA samples or columns having
#' constant values and removes them to only leave the fundamental parameters
#' (to run VoI analysis). This also deals with simulations stored in a
#' \code{.csv} or \code{.txt} file (eg as obtained using bootstrapping from a
#' non-Bayesian model).
#' 
#' @param inputs A \code{rjags}, \code{bugs} or \code{stanfit} object, containing
#' the results of a call to either JAGS, (under \code{R2jags}), BUGS
#' (under \code{R2WinBUGS} or \code{R2OpenBUGS}), or Stan (under \code{rstan}).
#' @param print_is_linear_comb A TRUE/FALSE indicator. If set to \code{TRUE} (default)
#' then prints the output of the procedure trying to assess whether there are
#' some parameters that are a linear combination of others (in which case
#' they are removed).
#' 
#' @export
#' 
createInputs <- function(inputs,
                         print_is_linear_comb = TRUE) {
  UseMethod("createInputs",  inputs)
}


#' @rdname createInputs
#' @export
#' 
createInputs.rjags <- function(inputs,
                               print_is_linear_comb = TRUE) {
  
  if ("deviance" %in% colnames(inputs))
    inputs <- inputs[, colnames(inputs) != "deviance"]
  
  inputs <- as.matrix(inputs$BUGSoutput$sims.matrix)
  NextMethod("createInputs")
}

#' @rdname createInputs
#' @export
#' 
createInputs.bugs <- function(inputs,
                              print_is_linear_comb = TRUE) {
  
  if ("deviance" %in% colnames(inputs))
    inputs <- inputs[, colnames(inputs) != "deviance"]
  
  inputs <- as.matrix(inputs$sims.matrix)
  NextMethod("createInputs")
}

#' @rdname createInputs
#' @export
#' 
createInputs.stanfit <- function(inputs,
                                 print_is_linear_comb = TRUE) {

  inputs <- as.matrix(inputs[, colnames(inputs) != "lp__"])
  NextMethod("createInputs")
}

#' @rdname createInputs
#' @export
#' 
createInputs.data.frame <- function(inputs,
                                    print_is_linear_comb = TRUE) {
  inputs <- as.matrix(inputs)
  NextMethod("createInputs")
}

#' @rdname createInputs
#' @export
#' 
createInputs.numeric <- function(inputs,
                                 print_is_linear_comb = TRUE) {
  inputs <- as.matrix(inputs)
  NextMethod("createInputs")
}

