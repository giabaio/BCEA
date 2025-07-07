
#' @name multi.ce
#' @title Cost-effectiveness Analysis With Multiple Comparison
#' 
#' @description Computes and plots the probability that each of the `n_int` interventions
#' being analysed is the most cost-effective and the cost-effectiveness
#' acceptability frontier.
#' 
#' @template args-he
#' 
#' @return Original `bcea` object (list) of class "pairwise" with additional:
#'    \item{p_best_interv}{A matrix including the probability that each
#'    intervention is the most cost-effective for all values of the willingness to
#'    pay parameter}
#'    \item{ceaf}{A vector containing the cost-effectiveness acceptability frontier}
#' 
#' @author Gianluca Baio
#' @seealso [bcea()],
#'          [ceaf.plot()]
#' @keywords hplot dplot
#' 
#' @examples
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#' 
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#'  
#' # Runs the health economic evaluation using BCEA
#' 
#' m <- bcea(e=eff, c=cost,    # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e,c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0,Kmax)
#'       plot=FALSE            # inhibits graphical output
#' )
#' 
#' mce <- multi.ce(m)          # uses the results of the economic analysis
#' 
#' ceac.plot(mce)
#' ceaf.plot(mce)
#' 
#' @export
#' 
multi.ce.bcea <- function(he) {
  
  # p_best_interv <- compute_p_best_interv(he)
  p_best_interv <- compute_p_optimal_best(he)
  ceaf <- compute_ceaf(p_best_interv)
  
  res <- c(he,
           list(p_best_interv = p_best_interv,
                p_best_overall = compute_p_best_interv(he),
                ceaf = ceaf))
  
  structure(res, class = c("pairwise", class(he)))
}


#' @export
#' 
multi.ce <- function(he) {
  UseMethod('multi.ce', he)
}


