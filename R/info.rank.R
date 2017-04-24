# ###info.rank#########################################################################
info.rank <- function(parameter,input,he,wtp=he$k[min(which(he$k>=he$ICER))],...) {
  # parameter = vector of parameters for which to make the plot
  # input = a matrix of PSA runs for all the parameters
  # he = a bcea object with the economic evaluation
  # wtp = a willingness-to-pay threshold (default at the break even point from he)
  # ... extra arguments including
  #     xlim = x-axis limits
  #     ca = cex axis label (default = 0.7)
  #     cn = cex names label (default = 0.7)
  #     mai = graphical parameter to determine the margins
  #     rel = if TRUE (default) then shows a plot of EVPPI/EVPI, if FALSE then only EVPPI
  #     N = number of PSA to be used to perform the evppi analysis
  
  # Function to actually create the bar plot
  make.barplot <- function(scores,chk2,xlab,xlim,ca,cn,mai,space) {
    col <- rep(c("blue","red"),length(chk2))
    par(mai=mai)
    res <- data.frame(parameter=names(chk2),info=scores,row.names=NULL)
    barplot(res[order(res$info),2],horiz=T,names.arg=res[order(res$info),1],cex.names=cn,las=1,col=col,cex.axis=ca,
            xlab=xlab,space=space,main=tit,xlim=xlim)
    par(mai=c(1.360000,1.093333,1.093333,0.560000))
    list(rank=data.frame(parameter=res[order(-res$info),1],info=res[order(-res$info),2]))
  }
  
  # Prevents BCEA::evppi from throwing messages
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  exArgs <- list(...)
  
  if(class(parameter[1])=="character"){
    parameters<-array()
    for(i in 1:length(parameter)){
      parameters[i]<-which(colnames(input)==parameter[i])
    }
  }else{
    parameters=parameter
    ####     parameter=colnames(input)[parameters]
  }
  parameter=colnames(input)[parameters]
  
  # needs to exclude parameters with weird behaviour (ie all 0s)
  w <- unlist(lapply(parameter,function(x) which(colnames(input)==x)))
  input <- input[,w]
  chk1 <- which(apply(input,2,var)>0)   # only takes those with var>0
  tmp <- lapply(1:dim(input)[2],function(x) table(input[,x])) # check those with <5 possible values (would break GAM)
  chk2 <- which(unlist(lapply(tmp,function(x) length(x)>=5))==TRUE)
  names(chk2) <- colnames(input[,chk2])
  
  # Can do the analysis on a smaller number of PSA runs
  if(exists("N",where=exArgs)) {N <- exArgs$N} else {N <- he$n.sim}
  if(any(!is.na(N)) & length(N)>1) {
    select <- N
  } else {
    N <- min(he$n.sim,N,na.rm=T)
    if(N==he$n.sim) {select <-1:he$n.sim} else {select <- sample(1:he$n.sim,size=N,replace=F)} 
  }
  m <- he; m$k=wtp
  x <- list()
  for (i in 1:length(chk2)) {
    x[[i]] <- quiet(evppi(parameter=chk2[i],input=input,he=m,N=N))
  }
  scores <- unlist(lapply(x,function(x) x$evppi/x$evi[which(he$k==wtp)]))
  # Optional inputs
  if(exists("ca",where=exArgs)) {ca <- exArgs$ca} else {ca <- .7}
  if(exists("cn",where=exArgs)) {cn <- exArgs$cn} else {cn <- .7}
  xlab <- "Proportion of total EVPI"
  if(exists("rel",where=exArgs)) {
    if (exArgs$rel==FALSE) {
      scores <- unlist(lapply(x,function(x) x$evppi))
      xlab <- "Absolute value of the EVPPI"
    }
  }
  if(exists("xlim",where=exArgs)) {xlim=exArgs$xlim} else {xlim=c(0,range(scores)[2])}
  if(exists("mai",where=exArgs)) {mai=exArgs$mai} else {mai=c(1.36,1.5,1,1)}
  if(exists("tit",where=exArgs)) {tit=exArgs$tit} else {tit <- paste0("Info-rank plot for willingness to pay=",wtp)}
  if(exists("space",where=exArgs)) {space=exArgs$space} else {space=.5}
  
  # Makes the plot
  make.barplot(scores=scores,chk2=chk2,xlab=xlab,xlim=xlim,ca=ca,cn=cn,mai=mai,space)
}
