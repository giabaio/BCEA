###INTRO#############################################################################################
## Define Classes & Methods
## v1.0. 4 January, 2012
## v1.1. 14 September, 2012
## v1.2. 17 September 2012
## v1.3-0 June, 2013
## v2.0-1 July, 2013
## v2.0-2 November, 2013
## v2.0-2b February, 2014 - ceac.plot and eib.plot: option comparison included for base graphics
## v2.0-2c July, 2014
## v2.1-0-pre1 AB September, 2014: documentation updated, Smoking dataset and ceef.plot function included, additional modifications
## v2.1.0-pre2 GB October, 2014: modifications to ceef.plot, CreateInputs, struct.psa
## v2.1.0 AB October, 2014: migrated from if(require()) to if(requireNamespace(,quietly=TRUE)); documentation updated
## v2.1.0 AB December, 2014: added threshold argument to ceef.plot function; documentation updated
## v2.1.1 GB+AH April/July 2015: new function for EVPPI using SPDE-INLA; modifications to the EVPPI functions; 
##        documentation updated; allows xlim & ylim in the ceplane.plot, contour and contour2 functions;
##	  it is now possible to run bcea for a scalar wtp; the old evppi function and method has been renamed 
## 	  evppi0, which means there's also a new plot.evppi0 method
## v2.2   GB October 2015: cleaned up and aligned with R's settings. EVPPI function polished up
## v2.2.1 GB+AH October 2015: adds the info-rank plot
## v2.2.2 AB January 2016: minor change to ceef.plot to align with ggplot2 v2.0.0
## v2.2.3 AH+GB May 2016: major update for the EVPPI to include PFC + fixed issues with info.rank
## v2.2.4 AB Nov 2016: fixes for new ggplot2 version (legend.spacing() and plot.title hjust argument)
## v2.2.5 Some changes to EVPPI
## v2.2.6 Fix in evppi to allow N to be selected in all methods + fix diag.evppi
## (C) Gianluca Baio + contributions by Andrea Berardi, Chris Jackson, Mark Strong & Anna Heath

###bcea##############################################################################################


#' Bayesian Cost-Effectiveness Analysis
#' 
#' Cost-effectiveness analysis based on the results of a simulation model for a
#' variable of clinical benefits (e) and of costs (c). Produces results to be
#' post-processed to give the health economic analysis. The output is stored in
#' an object of the class "bcea"
#' 
#' 
#' @aliases bcea bcea.default CEanalysis
#' @param e An object containing \code{nsim} simulations for the variable of
#' clinical effectiveness for each intervention being considered. In general it
#' is a matrix with \code{nsim} rows and \code{nint} columns.
#' @param c An object containing \code{nsim} simulations for the variable of
#' cost for each intervention being considered. In general it is a matrix with
#' \code{nsim} rows and \code{nint} columns.
#' @param ref Defines which intervention (columns of \code{e} or \code{c}) is
#' considered to be the reference strategy. The default value \code{ref=1}
#' means that the intervention associated with the first column of \code{e} or
#' \code{c} is the reference and the one(s) associated with the other column(s)
#' is(are) the comparators.
#' @param interventions Defines the labels to be associated with each
#' intervention. By default and if \code{NULL}, assigns labels in the form
#' "Intervention1", ... , "Intervention T".
#' @param Kmax Maximum value of the willingness to pay to be considered.
#' Default value is \code{k=50000}. The willingness to pay is then approximated
#' on a discrete grid in the interval \code{[0,Kmax]}. The grid is equal to
#' \code{wtp} if the parameter is given, or composed of \code{501} elements if
#' \code{wtp=NULL} (the default).
#' @param wtp A(n optional) vector wtp including the values of the willingness
#' to pay grid. If not specified then BCEA will construct a grid of 501 values
#' from 0 to Kmax. This option is useful when performing intensive computations
#' (eg for the EVPPI).
#' @param plot A logical value indicating whether the function should produce
#' the summary plot or not.
#' @return An object of the class "bcea" containing the following elements
#' \item{n.sim}{Number of simulations produced by the Bayesian model}
#' \item{n.comparators}{Number of interventions being analysed}
#' \item{n.comparisons}{Number of possible pairwise comparisons}
#' \item{delta.e}{For each possible comparison, the differential in the
#' effectiveness measure} \item{delta.c}{For each possible comparison, the
#' differential in the cost measure} \item{ICER}{The value of the Incremental
#' Cost-Effectiveness Ratio} \item{Kmax}{The maximum value assumed for the
#' willingness to pay threshold} \item{k}{The vector of values for the grid
#' approximation of the willingness to pay} \item{ceac}{The value for the
#' Cost-Effectiveness Acceptability Curve, as a function of the willingness to
#' pay} \item{ib}{The distribution of the Incremental Benefit, for a given
#' willingness to pay} \item{eib}{The value for the Expected Incremental
#' Benefit, as a function of the willingness to pay} \item{kstar}{The grid
#' approximation of the break even point(s)} \item{best}{A vector containing
#' the numeric label of the intervention that is the most cost-effective for
#' each value of the willingness to pay in the selected grid approximation}
#' \item{U}{An array including the value of the expected utility for each
#' simulation from the Bayesian model, for each value of the grid approximation
#' of the willingness to pay and for each intervention being considered}
#' \item{vi}{An array including the value of information for each simulation
#' from the Bayesian model and for each value of the grid approximation of the
#' willingness to pay} \item{Ustar}{An array including the maximum
#' "known-distribution" utility for each simulation from the Bayesian model and
#' for each value of the grid approximation of the willingness to pay}
#' \item{ol}{An array including the opportunity loss for each simulation from
#' the Bayesian model and for each value of the grid approximation of the
#' willingness to pay} \item{evi}{The vector of values for the Expected Value
#' of Information, as a function of the willingness to pay}
#' \item{interventions}{A vector of labels for all the interventions
#' considered} \item{ref}{The numeric index associated with the intervention
#' used as reference in the analysis} \item{comp}{The numeric index(es)
#' associated with the intervention(s) used as comparator(s) in the analysis}
#' \item{step}{The step used to form the grid approximation to the willingness
#' to pay} \item{e}{The \code{e} matrix used to generate the object (see
#' Arguments)} \item{c}{The \code{c} matrix used to generate the object (see
#' Arguments)}
#' @author Gianluca Baio, Andrea Berardi
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation
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
#'       plot=TRUE          # plots the results
#' )
#' #
#' # Creates a summary table
#' summary(m,      # uses the results of the economic evalaution 
#'                 #  (a "bcea" object)
#'       wtp=25000	# selects the particular value for k 
#' )
#' 
#' \donttest{
#' #
#' # Plots the cost-effectiveness plane using base graphics
#' ceplane.plot(m,      # plots the Cost-Effectiveness plane
#'       comparison=1,  # if more than 2 interventions, selects the
#'                      #  pairwise comparison 
#'       wtp=25000,     # selects the relevant willingness to pay 
#'                      #  (default: 25,000) 
#'       graph="base"   # selects base graphics (default)
#' )
#' #
#' # Plots the cost-effectiveness plane using ggplot2
#' if(requireNamespace("ggplot2")){
#' ceplane.plot(m,      # plots the Cost-Effectiveness plane
#'       comparison=1,  # if more than 2 interventions, selects the
#'                      #  pairwise comparison 
#'       wtp=25000,     # selects the relevant willingness to pay 
#'                      #  (default: 25,000) 
#'       graph="ggplot2"# selects ggplot2 as the graphical engine
#' )
#' #
#' # Some more options
#' ceplane.plot(m,
#'       graph="ggplot2",
#'       pos="top",
#'       size=5,
#'       ICER.size=1.5,
#'       label.pos=FALSE,
#'       opt.theme=ggplot2::theme(text=ggplot2::element_text(size=8))
#' )
#' }
#' #
#' # Plots the contour and scatterplot of the bivariate 
#' # distribution of (Delta_e,Delta_c)
#' contour(m,          # uses the results of the economic evalaution 
#'                     #  (a "bcea" object)
#'       comparison=1, # if more than 2 interventions, selects the 
#'                     #  pairwise comparison 
#'       nlevels=4,    # selects the number of levels to be 
#'                     #  plotted (default=4)
#'       levels=NULL,  # specifies the actual levels to be plotted 
#'                     #  (default=NULL, so that R will decide)
#'       scale=0.5,    # scales the bandwiths for both x- and 
#'                     #  y-axis (default=0.5)
#'       graph="base"  # uses base graphics to produce the plot
#' )
#' #
#' # Plots the contour and scatterplot of the bivariate 
#' #   distribution of (Delta_e,Delta_c)
#' contour2(m,       # uses the results of the economic evalaution 
#'                   #  (a "bcea" object)
#'       wtp=25000,  # selects the willingness-to-pay threshold
#'       xl=NULL,    # assumes default values
#'       yl=NULL     # assumes default values
#' )
#' #
#' # Using ggplot2
#' if(requireNamespace("ggplot2")){
#' contour2(m,           # uses the results of the economic evalaution 
#'                       #  (a "bcea" object)
#'       graph="ggplot2",# selects the graphical engine
#'       wtp=25000,      # selects the willingness-to-pay threshold
#'       xl=NULL,        # assumes default values
#'       yl=NULL,        # assumes default values
#'       label.pos=FALSE # alternative position for the wtp label
#' )
#' }
#' #
#' # Plots the Expected Incremental Benefit for the "bcea" object m
#' eib.plot(m)
#' #
#' # Plots the distribution of the Incremental Benefit
#' ib.plot(m,        # uses the results of the economic evalaution 
#'                   #  (a "bcea" object)
#'     comparison=1, # if more than 2 interventions, selects the 
#'                   #  pairwise comparison 
#'     wtp=25000,    # selects the relevant willingness 
#'                   #  to pay (default: 25,000)
#'     graph="base"  # uses base graphics
#' )
#' #
#' # Produces a plot of the CEAC against a grid of values for the 
#' # willingness to pay threshold
#' ceac.plot(m)
#' #
#' # Plots the Expected Value of Information for the "bcea" object m
#' evi.plot(m)
#' #
#' }
#' 
#' @export bcea
bcea <- function(e,c,ref=1,interventions=NULL,Kmax=50000,wtp=NULL,plot=FALSE) UseMethod("bcea")





























