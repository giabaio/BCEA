mixedAn.default <- function(he,mkt.shares=NULL,plot=FALSE) {
  # mkt.shares is a vector of market shares for each comparators
  # if no value is provided, then assumes uniform distribution
  # dev is the device to which the graph should be printed
  # default is x11() --- on screen. Possibilities are jpeg and postscript
  # Reference: Baio G, Russo P (2009).
  
  Ubar <- OL.star <- evi.star <- NULL
  if(is.null(mkt.shares)==TRUE){
    mkt.shares <- rep(1,he$n.comparators)/he$n.comparators
  }
  temp <- array(NA,c(he$n.sim,length(he$k),he$n.comparators))
  for (j in 1:he$n.comparators) {
    temp[,,j] <- mkt.shares[j]*he$U[,,j]
  }
  Ubar <- apply(temp,c(1,2),sum)
  OL.star <- he$Ustar - Ubar
  evi.star <- apply(OL.star,2,mean)
  
  ## Outputs of the function
  ma <- list(
    Ubar=Ubar,OL.star=OL.star,evi.star=evi.star,k=he$k,Kmax=he$Kmax,step=he$step,
    ref=he$ref,comp=he$comp,mkt.shares=mkt.shares,n.comparisons=he$n.comparisons,
    interventions=he$interventions,evi=he$evi
  )
  class(ma) <- "mixedAn"
  if(plot) {
    plot.mixedAn(ma)
  }
  return(ma)
}
