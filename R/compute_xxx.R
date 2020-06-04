
#
compute_CEAC <- function(ib) {
  
  apply(ib > 0, c(1,3), mean)
}

#
compute_EIB <- function(ib) {
  
  eib <- apply(ib, 3, function(x) apply(x, 1, mean))
  # eib <- apply(ib, 3, function(x) rowMeans(x))  ##TODO: test
}


##TODO:
get_kstar <- function(variables) {
  
  # Select the best option for each value of the willingness to pay parameter
  if(n.comparisons == 1) {
    best <- rep(ref,K)
    best[which(eib < 0)] <- comp
    ## Finds the k for which the optimal decision changes
    check <- c(0, diff(best))
    kstar <- k[check != 0]
  }
  if(n.comparisons > 1) {
    
    if (is.null(dim(eib))) {
      tmp <- min(eib)
      tmp2 <- which.min(eib)	
    } else {
      tmp <- apply(eib,1,min)
      tmp2 <- apply(eib,1,which.min)
    }
    
    best <- ifelse(tmp > 0,ref,comp[tmp2])
    # Finds the k for which the optimal decision changes
    check <- c(0,diff(best))
    kstar <- k[check != 0]
  }
}


##TODO:
compute_EVPI <- function(){
  
  U <- array(rep(e, K)*rep(k, each=n.sim*n.comparators) - as.vector(c),
             dim=c(n.sim, n.comparators, K))
  U <- aperm(U, c(1,3,2))
  
  rowMax <- function(x){do.call(pmax, as.data.frame(x))}
  
  Ustar <- vi <- ol <- matrix(NA,n.sim,K) 
  
  for (i in 1:K) {
    Ustar[,i] <- rowMax(U[,i,])
    cmd <- paste("ol[,i] <- Ustar[,i] - U[,i,",best[i],"]",sep="")
    eval(parse(text=cmd))     
    vi[,i] <- Ustar[,i] - max(apply(U[,i,],2,mean))
  }
  
  colMeans(ol)
}