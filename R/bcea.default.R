###bcea.default######################################################################################
## Default function
bcea.default <- function(e,c,ref=1,interventions=NULL,Kmax=50000,wtp=NULL,plot=FALSE) {
  ## Compute a Bayesian cost-effectiveness analysis of two or more interventions
  ## INPUTS:
  ## 1. Two objects (e,c). These can be directly computed in a simulation object "sim" from JAGS/BUGS, 
  ##    or derived by postprocessing of "sim" in R. The objects (e,c) have dimension (n.sim x number of 
  ##    interventions) and contain n.sim simulated values for the measures of effectiveness and costs 
  ##    for each intervention being compared. 
  ## 2. The reference intervention as a numeric value. Each intervention is a column in the matrices e 
  ##    and c so if ref=1 the first column is assumed to be associated with the reference intervention. 
  ##    Intervention 1 is assumed the default reference. All others are considered comparators.
  ## 3. A string vector "interventions" including the names of the interventions. If none is provided 
  ##    then labels each as "intervention1",...,"interventionN".
  ## 4. The value Kmax which represents the maximum value for the willingness to pay parameter. If none 
  ##    is provided, then it is assumed Kmax=50000.
  ## 5. A(n optional) vector wtp including the values of the willingness to pay grid. If not specified
  ##    then BCEA will construct a grid of 501 values from 0 to Kmax. This option is useful when 
  ##    performing intensive computations (eg for the EVPPI)
  ##
  ## OUTPUTS:
  ## Graphs & computed values for CE Plane, ICER, EIB, CEAC, EVPI 
  
  # Set the working directory to wherever the user is working, if not externally set
  if(!exists("working.dir")){working.dir <- getwd()}
  
  # Number of simulations & interventions analysed
  n.sim <- dim(e)[1]
  n.comparators <- dim(e)[2]
  
  # Define reference & comparator intervention (different labels can be given here if available!)
  if(is.null(interventions)){interventions <- paste("intervention",1:n.comparators)}
  ints <- 1:n.comparators
  
  # Define intervention i (where i can be a number in [1,...,n.comparators]) as the reference 
  # and the other(s) as comparator(s). Default is the first intervention (first column of e or c)
  comp <- ints[-ref]
  n.comparisons <- n.comparators-1
  
  # Compute Effectiveness & Cost differentials (wrt to reference intervention)
  delta.e <- e[,ref]-e[,comp]
  delta.c <- c[,ref]-c[,comp]
  
  # Compute the ICER
  if(n.comparisons==1) {
    ICER <- mean(delta.c)/mean(delta.e)
  }
  if(n.comparisons>1) {
    ICER <- colMeans(delta.c)/colMeans(delta.e) #apply(delta.c,2,mean)/apply(delta.e,2,mean)
  }
  
  
  # Compute and plot CEAC & EIB
  if(!exists("Kmax")){Kmax<-50000}
  # Lets you select the willingness to pay grid --- useful when doing EVPPI (computationally intensive)
  if (!is.null(wtp)) {
    wtp <- sort(unique(wtp))
    npoints <- length(wtp) - 1
    Kmax <- max(wtp)
    step <- NA
    k <- wtp
    K <- npoints+1
  } else {
    npoints <- 500
    step <- Kmax/npoints
    k <- seq(0,Kmax,step)
    K <- length(k)	
  }
  
  if(n.comparisons==1) {
    ib <- scale(k%*%t(delta.e),delta.c,scale=FALSE)
    ceac <- rowMeans(ib>0) #apply(ib>0,1,mean)
  }
  if(n.comparisons>1) { 
    ib <- array(rep(delta.e, K)*rep(k, each=n.sim*n.comparisons)-as.vector(delta.c),
                dim=c(n.sim, n.comparisons, K))
    ib <- aperm(ib, c(3,1,2))
    ###          ib <- sweep(apply(delta.e,c(1,2),function(x) k%*%t(x)),c(2,3),delta.c,"-")
    ceac <- apply(ib>0,c(1,3),mean)
  }
  
  # Select the best option for each value of the willingness to pay parameter
  if(n.comparisons==1) {
    eib <- rowMeans(ib)  #apply(ib,1,mean)
    best <- rep(ref,K)
    best[which(eib<0)] <- comp
    ## Finds the k for which the optimal decision changes
    check <- c(0,diff(best))
    kstar <- k[check!=0]
  }
  if(n.comparisons>1) {
    eib <- apply(ib,3,function(x) apply(x,1,mean))
    if (is.null(dim(eib))) {
      tmp <- min(eib)
      tmp2 <- which.min(eib)	
    } else {
      tmp <- apply(eib,1,min)
      tmp2 <- apply(eib,1,which.min)
    }
    best <- ifelse(tmp>0,ref,comp[tmp2])
    # Finds the k for which the optimal decision changes
    check <- c(0,diff(best))
    kstar <- k[check!=0]
  }
  
  # Compute EVPI 
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
  evi <- colMeans(ol)
  
  ## Outputs of the function
  he <- list(
    n.sim=n.sim,n.comparators=n.comparators,n.comparisons=n.comparisons,delta.e=delta.e,
    delta.c=delta.c,ICER=ICER,Kmax=Kmax,k=k,ceac=ceac,ib=ib,eib=eib,kstar=kstar,
    best=best,U=U,vi=vi,Ustar=Ustar,ol=ol,evi=evi,interventions=interventions,
    ref=ref,comp=comp,step=step,e=e,c=c
  )
  
  class(he) <- "bcea"
  if(plot)
    plot(he)
  return(he)
}



