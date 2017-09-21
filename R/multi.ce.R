#####multi.ce##################################################################################################


#' Cost-effectiveness analysis with multiple comparison
#' 
#' Computes and plots the probability that each of the n_int interventions
#' being analysed is the most cost-effective and the cost-effectiveness
#' acceptability frontier
#' 
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @return \item{m.ce}{A matrix including the probability that each
#' intervention is the most cost-effective for all values of the willingness to
#' pay parameter} \item{ceaf}{A vector containing the cost-effectiveness
#' acceptability frontier}
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}, \code{\link{mce.plot}}, \code{\link{ceaf.plot}}
#' @keywords Health economic evaluation Multiple comparison
#' @examples
#' 
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#' #
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#' # 
#' # Runs the health economic evaluation using BCEA
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
#' #
#' mce <- multi.ce(m           # uses the results of the economic analysis 
#' )
#' 
#' @export multi.ce
multi.ce <- function(he){
  # Cost-effectiveness analysis for multiple comparison 
  # Identifies the probability that each comparator is the most cost-effective as well as the
  # cost-effectiveness acceptability frontier
  cl <- colors()
  # choose colors on gray scale
  color <- cl[floor(seq(262,340,length.out=he$n.comparators))]	
  
  rank <- most.ce <- array(NA,c(he$n.sim,length(he$k),he$n.comparators))
  for (t in 1:he$n.comparators) {
    for (j in 1:length(he$k)) {
      rank[,j,t] <- apply(he$U[,j,]<=he$U[,j,t],1,sum)
      most.ce[,j,t] <- rank[,j,t]==he$n.comparators
    }
  }
  m.ce <- apply(most.ce,c(2,3),mean)		# Probability most cost-effective
  ceaf <- apply(m.ce,1,max)			# Cost-effectiveness acceptability frontier
  
  # Output of the function
  list(
    m.ce=m.ce,ceaf=ceaf,n.comparators=he$n.comparators,k=he$k,interventions=he$interventions
  )
}
