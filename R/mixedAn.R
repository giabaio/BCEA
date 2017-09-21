###mixedAn####################################################################################################


#' Cost-effectiveness analysis when multiple (possibly non cost-effective)
#' interventions are present on the market
#' 
#' Runs the cost-effectiveness analysis, but accounts for the fact that more
#' than one intervention is present on the market
#' 
#' 
#' @aliases mixedAn mixedAn.default
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param mkt.shares A vector of market shares associated with the
#' interventions. Its size is the same as the number of possible comparators.
#' By default, assumes uniform distribution for each intervention.
#' @param plot Logical value indicating whether the function should produce
#' graphical output, via \code{\link{plot.mixedAn}}, or not. Default is set to
#' \code{FALSE}.
#' @return Creates an object in the class \code{mixedAn} which contains the
#' results of the health economic evaluation in the mixed analysis case
#' \item{Ubar}{An array with the simulations of the ''known-distribution''
#' mixed utilities, for each value of the discrete grid approximation of the
#' willingness to pay parameter} \item{OL.star}{An array with the simulations
#' of the distribution of the Opportunity Loss for the mixed strategy, for each
#' value of the discrete grid approximation of the willingness to pay
#' parameter} \item{evi.star}{The Expected Value of Information for the mixed
#' strategy, for each value of the discrete grid approximation of the
#' willingness to pay parameter} \item{k}{The discrete grid approximation of
#' the willingness to pay parameter used for the mixed strategy analysis}
#' \item{Kmax}{The maximum value of the discrete grid approximation for the
#' willingness to pay parameter} \item{step}{The step used to form the grid
#' approximation to the willingness to pay} \item{ref}{The numeric index
#' associated with the intervention used as reference in the analysis}
#' \item{comp}{The numeric index(es) associated with the intervention(s) used
#' as comparator(s) in the analysis} \item{mkt.shares}{The vector of market
#' shares associated with each available intervention} \item{n.comparisons}{The
#' total number of pairwise comparisons available} \item{interventions}{A
#' vector of labels for all the interventions considered} \item{evi}{The vector
#' of values for the ''optimal'' Expected Value of Information, as a function
#' of the willingness to pay} The function can also produce a graph showing the
#' difference between the ''optimal'' version of the EVPI (when only the most
#' cost-effective intervention is included in the market) and the mixed
#' strategy one (when more than one intervention is considered in the market)
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references Baio, G. and Russo, P. (2009).A decision-theoretic framework for
#' the application of cost-effectiveness analysis in regulatory processes.
#' Pharmacoeconomics 27(8), 645-655 doi:10.2165/11310250
#' 
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health
#' Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Mixed analysis
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
#' ma <- mixedAn(m,        # uses the results of the mixed strategy 
#'                         #  analysis (a "mixedAn" object)
#'       mkt.shares=NULL,  # the vector of market shares can be defined 
#'                         #  externally. If NULL, then each of the T 
#'                         #  interventions will have 1/T market share
#'       plot=TRUE         # produces the plots
#' )
#' 
#' @export mixedAn
mixedAn <- function(he,mkt.shares=NULL,plot=FALSE) UseMethod("mixedAn")
