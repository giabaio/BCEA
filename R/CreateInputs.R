
#' @rdname createInputs
#'
#' @return \item{mat}{Data.frame containing all the simulations
#'         for all the monitored parameters}
#'         \item{parameters}{Character vectors of the names
#'         of all the monitored parameters}
#'
#' @author Gianluca Baio, Anna Heath and Mark Strong
#' @seealso [bcea()],
#'          [evppi()]
#' @export
#'
createInputs.default <- function(inputs,
                                 print_is_linear_comb = TRUE) {
  
  # remove NA columns
  if (sum(is.na(inputs)) > 0) {
    inputs <- inputs[ , colSums(is.na(inputs)) == 0]
    message("Dropped any columns containing NAs")
  }
  
  if (!is.logical(print_is_linear_comb))
    stop("print_is_linear_comb must be logical.", call. = FALSE)
  
  inputs <- inputs[, !colnames(inputs) %in% c("lp__", "deviance")]
  
  # remove redundant parameters
  # linear combination of columns or constant columns
  # by M Strong
  
  cols_keep <- colnames(inputs)
  is_const_params <- apply(inputs, 2, "var") == 0
  if (any(is_const_params)) cols_keep <- cols_keep[!is_const_params]
  
  params <- inputs[, cols_keep, drop = FALSE]
  ranks <- loo_rank(params)
  are_multiple_ranks <- length(unique(ranks)) > 1
  
  while (are_multiple_ranks) {
    
    linear_combs <- which(ranks == max(ranks))
    
    if (print_is_linear_comb) {
      print(paste(linear_combs,
                  "\nLinear dependence: removing column",
                  colnames(params)[max(linear_combs)]))
    }
    
    ##TODO: what does cbind do here?
    params <- cbind(params[, -max(linear_combs), drop = FALSE])
    ranks <- loo_rank(params)
    are_multiple_ranks <- length(unique(ranks)) > 1
  }
  
  # special case only linear combination remains
  while (qr(params)$rank == ranks[1]) {
    
    if (print_is_linear_comb) {
      print(paste("Linear dependence... removing column",
                  colnames(params)[1]))
    }
    params <- cbind(params[, -1, drop = FALSE])
    ranks <- loo_rank(params)
  }
  params <- data.frame(params)
  
  list(mat = params,
       parameters = colnames(params))
}


#' Create Inputs for EVPI Calculation
#'
#' Creates an object containing the matrix with the parameters simulated using
#' the MCMC procedure (using JAGS, BUGS or Stan) and a vector of parameters
#' (strings) that can be used to perform the expected value of partial
#' information analysis. In the process, `createInputs` also checks for
#' linear dependency among columns of the PSA samples or columns having
#' constant values and removes them to only leave the fundamental parameters
#' (to run VoI analysis). This also deals with simulations stored in a
#' `.csv` or `.txt` file (e.g. as obtained using bootstrapping from a
#' non-Bayesian model).
#'
#' @param inputs An object containing suitable simulations for the model 
#' parameters. It can be a `rjags` or `bugs` object (obtained directly by 
#' running `R2jags`, `rjags`, `R2OpenBUGS` or `R2WinBUGS`). For those objects,
#' which are in the class `rjags` or `bugs`, the user can simply pass them to 
#' `createInputs()`, which will now how to process them. If the user has 
#' run their model using `rstan`, then they should pre-process the output using
#' the `rstan::extract()` function; so if `rstan` is used to run a Bayesian
#' model and save the results to, say, the object `fit`, then the user should 
#' first create another object `tmp=rstan::extract(fit) |> as.data.frame()`
#' and then pass this onto `createInputs()`. Finally, this function handles 
#' inputs from data-frames of vectors directly. 
#' @param print_is_linear_comb Logical indicator. If set to `TRUE` (default)
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


#' Leave-one-out ranking
#' @param params Parameters
#' @keywords internal
#' 
loo_rank <- function(params)
  sapply(seq_len(NCOL(params)), function(x) qr(params[, -x])$rank)

