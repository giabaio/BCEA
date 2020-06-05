
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