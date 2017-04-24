CEriskav.default <- function(he,r=NULL,comparison=1) {
  ### COMPARISON IS USED TO SELECT THE COMPARISON FOR WHICH THE ANALYSIS IS CARRIED OUT!!!
  # Reference: Baio G, Dawid AP (2011).
  # Default vector of risk aversion parameters
  if(is.null(r)==TRUE){
    r <- c(0.000000000001,0.0000025,.000005)
  }
  
  # Computes expected utilities & EVPI for the risk aversion cases
  K <- length(he$k)
  R <- length(r)
  Ur <- array(NA,c(dim(he$U),R))
  Urstar <- array(NA,c(dim(he$Ustar),R))
  for (i in 1:K) {
    for (l in 1:R) {
      for (j in 1:he$n.comparators) {
        Ur[,i,j,l] <- (1/r[l])*(1-exp(-r[l]*he$U[,i,j]))
      }
      Urstar[,i,l] <- apply(Ur[,i,,l],1,max)
    }
  }
  
  if (he$n.comparisons==1){
    IBr <- Ur[,,he$ref,] - Ur[,,he$comp,]
  }
  if (he$n.comparisons>1){
    IBr <- Ur[,,he$ref,] - Ur[,,he$comp[comparison],]
  }
  
  eibr <- apply(IBr,c(2,3),mean)
  vir <- array(NA,c(he$n.sim,K,R))
  for (i in 1:K) {
    for (l in 1:R) {
      vir[,i,l] <- Urstar[,i,l] - max(apply(Ur[,i,,l],2,mean))
    }
  }
  evir <- apply(vir,c(2,3),mean)
  
  ## Outputs of the function
  cr <- list(
    Ur=Ur,Urstar=Urstar,IBr=IBr,eibr=eibr,vir=vir,evir=evir,R=R,r=r,k=he$k
  )
  class(cr) <- "CEriskav"
  cr
}