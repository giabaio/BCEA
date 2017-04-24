###sim.table##################################################################################################
# Produce a summary table with the results of simulations for the health economic variables of interest
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


