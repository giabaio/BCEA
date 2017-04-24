###summary.mixedAn############################################################################################
summary.mixedAn <- function(object,wtp=25000,...) {
  if(max(object$k)<wtp) {
    wtp <- max(object$k)
  }
  if(length(which(object$k==wtp))==0) {
    stop(paste("The willingness to pay parameter is defined in the interval [0-",object$Kmax,"], 
                     with increments of ",object$step,"\n",sep=""))
  }
  
  n.digits <- 2
  n.small <- 2
  cat("\n")
  cat(paste("Analysis of mixed strategy for willingness to pay parameter k = ",
            wtp,"\n",sep=""))
  cat("\n")
  cat(paste("Reference intervention: ",object$interventions[object$ref]," (",
            format(100*object$mkt.shares[object$ref],digits=n.digits,nsmall=n.small),"% market share)\n",sep=""))
  if(object$n.comparisons==1) {
    text.temp <- paste("Comparator intervention: ",object$interventions[object$comp]," (",
                       format(100*object$mkt.shares[object$comp],digits=n.digits,nsmall=n.small),"% market share)\n",sep="")
    cat(text.temp)
  }
  
  if(object$n.comparisons>1) {
    text.temp <- paste("Comparator intervention(s): ",object$interventions[object$comp[1]]," (",
                       format(100*object$mkt.shares[object$comp[1]],digits=n.digits,nsmall=n.small),"% market share)\n",sep="")
    cat(text.temp)
    for (i in 2:object$n.comparisons) {
      cat(paste("                          : ",object$interventions[object$comp[i]]," (", 
                format(100*object$mkt.shares[object$comp[i]],digits=n.digits,nsmall=n.small),"% market share)\n",sep=""))
    }
  }
  cat("\n")
  cat(paste("Loss in the expected value of information = ",
            format(object$evi.star[object$k==wtp]-object$evi[object$k==wtp],digits=n.digits,nsmall=n.small),"\n",sep=""))
  cat("\n")
}