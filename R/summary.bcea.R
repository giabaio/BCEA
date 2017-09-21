###summary.bcea#################################################################################################
## Summary of the results


#' Summary method for objects in the class \code{bcea}
#' 
#' Produces a table printout with some summary results of the health economic
#' evaluation
#' 
#' 
#' @param object A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param wtp The value of the willingness to pay threshold to be used in the
#' summary table.
#' @param ...  Additional arguments affecting the summary produced.
#' @return Prints a summary table with some information on the health economic
#' output and synthetic information on the economic measures (EIB, CEAC, EVPI).
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation
#' @export summary.bcea
summary.bcea <- function(object,wtp=25000,...) {
  
  if(max(object$k)<wtp) {
    wtp <- max(object$k)
    cat(paste("NB: k (wtp) is defined in the interval [",min(object$k)," - ",wtp,"]\n",sep=""))
  }
  if (!is.element(wtp,object$k)) {
    if (!is.na(object$step)) {# The user has selected a non-acceptable value for wtp, but has not specified wtp in the call to bcea
      stop(paste("The willingness to pay parameter is defined in the interval [0-",object$Kmax,
                 "], with increments of ",object$step,"\n",sep=""))
    } else { # The user has actually specified wtp as input in the call to bcea
      tmp <- paste(object$k,collapse=" ")
      stop(paste0("The willingness to pay parameter is defined as:\n[",tmp,"]\nPlease select a suitable value",collapse=" "))
    }
  }
  ind.table <- which(object$k==wtp)
  cols.u <- 1:object$n.comparators
  cols.ustar <- max(cols.u)+1
  cols.ib <- (cols.ustar+1):(cols.ustar+object$n.comparisons)
  cols.ol <- max(cols.ib)+1
  cols.vi <- cols.ol+1
  n.cols <- cols.vi
  
  Table <- matrix(NA,(object$n.sim+1),n.cols)
  Table[1:object$n.sim,cols.u] <- object$U[,ind.table,]
  Table[1:object$n.sim,cols.ustar] <- object$Ustar[,ind.table]
  if(length(dim(object$ib))==2){Table[1:object$n.sim,cols.ib] <- object$ib[ind.table,]}
  if(length(dim(object$ib))>2){Table[1:object$n.sim,cols.ib] <- object$ib[ind.table,,]}
  Table[1:object$n.sim,cols.ol] <- object$ol[,ind.table]
  Table[1:object$n.sim,cols.vi] <- object$vi[,ind.table]
  if(length(dim(object$ib))==2){
    Table[(object$n.sim+1),] <- c(apply(object$U[,ind.table,],2,mean),mean(object$Ustar[,ind.table]),
                                  mean(object$ib[ind.table,]),mean(object$ol[,ind.table]),mean(object$vi[,ind.table]))     
  }
  if(length(dim(object$ib))>2){
    Table[(object$n.sim+1),] <- c(apply(object$U[,ind.table,],2,mean),mean(object$Ustar[,ind.table]),
                                  apply(object$ib[ind.table,,],2,mean),mean(object$ol[,ind.table]),mean(object$vi[,ind.table]))
  }
  
  names.cols <- c(paste("U",seq(1:object$n.comparators),sep=""),"U*",
                  paste("IB",object$ref,"_",object$comp,sep=""),"OL","VI")
  colnames(Table) <- names.cols
  
  tab1 <- matrix(NA,object$n.comparators,1)
  tab1[,1] <- Table[object$n.sim+1,(paste("U",seq(1:object$n.comparators),sep=""))]
  colnames(tab1) <- "Expected utility"
  rownames(tab1) <- object$interventions
  
  tab2 <- matrix(NA,object$n.comparisons,3)
  tab2[,1] <- Table[object$n.sim+1,paste("IB",object$ref,"_",object$comp,sep="")]
  if (object$n.comparisons==1) {
    tab2[,2] <- sum(Table[1:object$n.sim,paste("IB",object$ref,"_",object$comp,sep="")]>0)/object$n.sim
    tab2[,3] <- object$ICER
  }
  if (object$n.comparisons>1) {
    for (i in 1:object$n.comparisons) {
      tab2[i,2] <- sum(Table[1:object$n.sim,paste("IB",object$ref,"_",object$comp[i],sep="")]>0)/object$n.sim
      tab2[i,3] <- object$ICER[i]
    }
  }
  colnames(tab2) <- c("EIB","CEAC","ICER")
  rownames(tab2) <- paste(object$interventions[object$ref]," vs ",object$interventions[object$comp],sep="")
  
  tab3 <- matrix(NA,1,1)
  tab3[,1] <- Table[object$n.sim+1,"VI"]
  rownames(tab3) <- "EVPI"
  colnames(tab3) <- ""
  
  ## Prints the summary table
  cat("\n")
  cat("Cost-effectiveness analysis summary \n")
  cat("\n")
  cat(paste("Reference intervention:  ",object$interventions[object$ref],"\n",sep=""))
  if(object$n.comparisons==1) {
    text.temp <- paste("Comparator intervention: ",object$interventions[object$comp],"\n",sep="")
    cat(text.temp)
  }
  
  if(object$n.comparisons>1) {
    text.temp <- paste("Comparator intervention(s): ",object$interventions[object$comp[1]],"\n",sep="")
    cat(text.temp)
    for (i in 2:object$n.comparisons) {
      cat(paste("                          : ",object$interventions[object$comp[i]],"\n",sep=""))
    }
  }
  cat("\n")
  if(length(object$kstar)==0 & !is.na(object$step)){
    cat(paste(object$interventions[object$best[1]]," dominates for all k in [",
              min(object$k)," - ",max(object$k),"] \n",sep=""))
  }
  if(length(object$kstar)==1 & !is.na(object$step)){
    cat(paste("Optimal decision: choose ",object$interventions[object$best[object$k==object$kstar-object$step]],
              " for k<",object$kstar," and ",object$interventions[object$best[object$k==object$kstar]],
              " for k>=",object$kstar,"\n",sep=""))
  }
  if(length(object$kstar)>1 & !is.na(object$step)){
    cat(paste("Optimal decision: choose ",object$interventions[object$best[object$k==object$kstar[1]-object$step]],
              " for k < ",object$kstar[1],"\n",sep=""))
    for (i in 2:length(object$kstar)) {
      cat(paste("                         ",object$interventions[object$best[object$k==object$kstar[i]-object$step]],
                " for ",object$kstar[i-1]," <= k < ",object$kstar[i],"\n",sep=""))
    }
    cat(paste("                         ",object$interventions[object$best[object$k==object$kstar[length(object$kstar)]]],
              " for k >= ",object$kstar[length(object$kstar)],"\n",sep=""))
  }
  cat("\n\n")
  cat(paste("Analysis for willingness to pay parameter k = ",wtp,"\n",sep=""))
  cat("\n")
  print(tab1,quote=F,digits=5,justify="center")
  cat("\n")
  print(tab2,quote=F,digits=5,justify="center")
  cat("\n")
  cat(paste("Optimal intervention (max expected utility) for k=",wtp,": ",
            object$interventions[object$best][object$k==wtp],"\n",sep=""))
  print(tab3,quote=F,digits=5,justify="center")
}


