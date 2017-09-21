###sim.table##################################################################################################
# Produce a summary table with the results of simulations for the health economic variables of interest


#' Table of simulations for the health economic model
#' 
#' Using the input in the form of MCMC simulations and after having run the
#' health economic model, produces a summary table of the simulations from the
#' cost-effectiveness analysis
#' 
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param wtp The value of the willingness to pay threshold to be used in the
#' summary table.
#' @return Produces the following elements: \item{Table}{A table with the
#' simulations from the economic model} \item{names.cols}{A vector of labels to
#' be associated with each column of the table} \item{wtp}{The selected value
#' of the willingness to pay} \item{ind.table}{The index associated with the
#' selected value of the willingness to pay threshold in the grid used to run
#' the analysis}
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
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
#'       Kmax=50000            # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0,Kmax)
#' )
#' #
#' # Now can save the simulation exercise in an object using sim.table()
#' st <- sim.table(m, # uses the results of the economic evalaution 
#'                    #  (a "bcea" object)
#'         wtp=25000  # selects the particular value for k
#' )
#' #
#' # The table can be explored. For example, checking the 
#' #  element 'Table' of the object 'st'
#' 
#' @export sim.table
sim.table <- function(he,wtp=25000) {
  
  if(wtp>he$Kmax){wtp=he$Kmax}
  
  if (!is.element(wtp,he$k)) {
    if (!is.na(he$step)) {# The user has selected a non-acceptable value for wtp, but has not specified wtp in the call to bcea
      stop(paste("The willingness to pay parameter is defined in the interval [0-",he$Kmax,
                 "], with increments of ",he$step,"\n",sep=""))
    } else { # The user has actually specified wtp as input in the call to bcea
      tmp <- paste(he$k,collapse=" ")
      stop(paste0("The willingness to pay parameter is defined as:\n[",tmp,"]\nPlease select a suitable value",collapse=" "))
    }
  }
  
  ind.table <- which(he$k==wtp)
  cols.u <- 1:he$n.comparators
  cols.ustar <- max(cols.u)+1
  cols.ib <- (cols.ustar+1):(cols.ustar+he$n.comparisons)
  cols.ol <- max(cols.ib)+1
  cols.vi <- cols.ol+1
  n.cols <- cols.vi
  
  Table <- matrix(NA,(he$n.sim+1),n.cols)
  Table[1:he$n.sim,cols.u] <- he$U[,ind.table,]
  Table[1:he$n.sim,cols.ustar] <- he$Ustar[,ind.table]
  if(length(dim(he$ib))==2){Table[1:he$n.sim,cols.ib] <- he$ib[ind.table,]}
  if(length(dim(he$ib))>2){Table[1:he$n.sim,cols.ib] <- he$ib[ind.table,,]}
  Table[1:he$n.sim,cols.ol] <- he$ol[,ind.table]
  Table[1:he$n.sim,cols.vi] <- he$vi[,ind.table]
  if(length(dim(he$ib))==2){
    Table[(he$n.sim+1),] <- c(apply(he$U[,ind.table,],2,mean),mean(he$Ustar[,ind.table]),
                              mean(he$ib[ind.table,]),mean(he$ol[,ind.table]),mean(he$vi[,ind.table]))	
  }
  if(length(dim(he$ib))>2){
    Table[(he$n.sim+1),] <- c(apply(he$U[,ind.table,],2,mean),mean(he$Ustar[,ind.table]),
                              apply(he$ib[ind.table,,],2,mean),mean(he$ol[,ind.table]),mean(he$vi[,ind.table]))
  }
  
  names.cols <- c(paste("U",seq(1:he$n.comparators),sep=""),"U*",paste("IB",he$ref,"_",he$comp,sep=""),"OL","VI")
  colnames(Table) <- names.cols
  rownames(Table) <- c(1:he$n.sim,"Average")
  
  ## Outputs of the function
  list(Table=Table,names.cols=names.cols,wtp=wtp,ind.table=ind.table)
}


