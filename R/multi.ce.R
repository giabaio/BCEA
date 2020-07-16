
#' Cost-effectiveness analysis with multiple comparison
#' 
#' Computes and plots the probability that each of the n_int interventions
#' being analysed is the most cost-effective and the cost-effectiveness
#' acceptability frontier.
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' 
#' @return \item{p_best_interv}{A matrix including the probability that each
#' intervention is the most cost-effective for all values of the willingness to
#' pay parameter} \item{ceaf}{A vector containing the cost-effectiveness
#' acceptability frontier}
#' 
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}, \code{\link{mce.plot}}, \code{\link{ceaf.plot}}
#' @keywords Health economic evaluation Multiple comparison
#' 
#' @examples
#' 
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#' 
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#'  
#' # Runs the health economic evaluation using BCEA
#' 
#' m <- bcea(e=e,c=c,          # defines the variables of 
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
#' @export
#' 
multi.ce <- function(he) {
  
  # grey scale
  color <- colors()[floor(seq(262, 340, length.out = he$n_comparators))]
  
  p_best_interv <- array(NA, c(length(he$k), he$n_comparators))
  
  for (i in seq_len(he$n_comparators)) {
    for (k in seq_along(he$k)) {
      
      is_interv_best <- he$U[, k, ] <= he$U[, k, i]
      
      rank <- apply(!is_interv_best, 1, sum)
      
      p_best_interv[k, i] <- mean(rank == 0)
    }
  }
  
  # cost-effectiveness acceptability frontier
  
  ##TODO: fixed ref value. do we really want this? [NG]
  ceaf <- p_best_interv[cbind(1:nrow(p_best_interv), he$best)]
  #ceaf <- apply(p_best_interv, 1, max)
  
  he <- c(he,
          list(p_best_interv = p_best_interv,
          ceaf = ceaf))
  
  structure(he, class = c("multi", class(he)))
}
