
#' @rdname createInputs
#' 
#' @return \item{mat}{A data.frame containing all the simulations
#'         for all the monitored parameters}
#'         \item{parameters}{A character vectors listing the names
#'         of all the monitored parameters}
#' 
#' @author Gianluca Baio and Mark Strong
#' @seealso \code{\link{bcea}},
#'          \code{\link{evppi}}
#' @keywords R2jags R2WinBUGS R2OpenBUGS
#' @export
#' 
create_inputs_evpi <- function(inputs,
                               print_is_linear_comb = TRUE) {
   
   # removes deviance (not relevant for VOI computations)
   inputs <- inputs[, !colnames(inputs) %in% c("lp__", "deviance")]
   
   # remove redundant parameters (linear combination of columns or constant columns)
   # by M Strong
   cols_keep <- colnames(inputs)
   const_params <- apply(inputs, 2, "var") == 0
   if (sum(const_params) > 0) cols_keep <- cols_keep[!const_params]
   
   paramSet <- inputs[, cols_keep, drop = FALSE]
   
   rankifremoved <- function(paramSet)
      sapply(1:NCOL(paramSet), function (x) qr(paramSet[, -x])$rank)
   
   rank_if_removed <- rankifremoved(paramSet)
   
   while (length(unique(rank_if_removed)) > 1) {
      
      linear_combs <- which(rank_if_removed == max(rank_if_removed))
      
      if (print_is_linear_comb) {
         print(linear_combs)
         print(paste("Linear dependence: removing column",
                     colnames(paramSet)[max(linear_combs)]))
      }
      paramSet <- cbind(paramSet[, -max(linear_combs), drop = FALSE])
      rank_if_removed <- rankifremoved(paramSet)
   }
   
   while (qr(paramSet)$rank == rank_if_removed[1]) {
      
      if (print_is_linear_comb) {
         print(paste("Linear dependence... removing column",
                     colnames(paramSet)[1]))
      }
      # special case only linear combination remains
      paramSet <- cbind(paramSet[, -1, drop = FALSE])
      rank_if_removed <- rankifremoved(paramSet)
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
#' the results of a call to either \code{jags}, (under \code{R2jags}), bugs
#' (under \code{R2WinBUGS} or \code{R2OpenBUGS}), or \code{stan} (under \code{rstan}).
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
   
   dat <- inputs$BUGSoutput$sims.matrix
   create_inputs_evpi(dat, print_is_linear_comb)
}

#' @rdname createInputs
#' @export
#' 
createInputs.bugs <- function(inputs,
                              print_is_linear_comb = TRUE) {
   
   dat <- inputs$sims.matrix
   create_inputs_evpi(dat, print_is_linear_comb)
}

#' @rdname createInputs
#' @export
#' 
createInputs.stanfit <- function(inputs,
                                 print_is_linear_comb = TRUE) {
   
   create_inputs_evpi(inputs, print_is_linear_comb)
}

#' @rdname createInputs
#' @export
#' 
createInputs.data.frame <- function(inputs,
                                    print_is_linear_comb = TRUE) {
   
   create_inputs_evpi(inputs, print_is_linear_comb)
}

#' @rdname createInputs
#' @export
#' 
createInputs.matrix <- function(inputs,
                                print_is_linear_comb = TRUE) {
   
   create_inputs_evpi(inputs, print_is_linear_comb)
}

#' @rdname createInputs
#' @export
#' 
createInputs.numeric <- function(inputs,
                                 print_is_linear_comb = TRUE) {
   
   create_inputs_evpi(inputs, print_is_linear_comb)
}

#' @rdname createInputs
#' @export
#' 
createInputs.default <- function(inputs,
                                 print_is_linear_comb) {
   
   stop("MCMC variable not of required type.",
        call. = FALSE)
}

