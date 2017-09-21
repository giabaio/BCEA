###ib.plot####################################################################################################
## Plots the IB


#' Incremental Benefit (IB) distribution plot
#' 
#' Plots the distribution of the Incremental Benefit (IB) for a given value of
#' the willingness to pay threshold
#' 
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param comparison In the case of multiple interventions, specifies the one
#' to be used in comparison with the reference. Default value of \code{NULL}
#' forces R to consider the first non-reference intervention as the comparator.
#' @param wtp The value of the willingness to pay threshold. Default value at
#' \code{25000}.
#' @param bw Identifies the smoothing bandwith used to construct the kernel
#' estimation of the IB density.
#' @param n The number of equally spaced points at which the density is to be
#' estimated.
#' @param xlim The limits of the plot on the x-axis.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @return \item{ib}{ A ggplot object containing the requested plot. Returned
#' only if \code{graph="ggplot2"}. } The function produces a plot of the
#' distribution of the Incremental Benefit for a given value of the willingness
#' to pay parameter. The dashed area indicates the positive part of the
#' distribution (ie when the reference is more cost-effective than the
#' comparator).
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{ib.plot}},
#' \code{\link{ceplane.plot}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation
#' @export ib.plot
ib.plot <- function(he,comparison=NULL,wtp=25000,bw=nbw,n=512,xlim=NULL,graph=c("base","ggplot2")){
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE)
  # comparison controls which comparator is used when more than 2 interventions are present
  # bw and n control the level of smoothness of the kernel density estimation
  options(scipen=10)
  
  if(!is.null(comparison))
    stopifnot(comparison<=he$n.comparison)
  
  if(base.graphics) {
    if(max(he$k)<wtp) {
      wtp <- max(he$k)
      cat(paste("NB: k (wtp) is defined in the interval [",min(he$k)," - ",wtp,"]\n",sep=""))
    }
    
    if (!is.element(wtp,he$k)) {
      if (!is.na(he$step)) {# The user has selected a non-acceptable value for wtp, but has not specified wtp in the call to bcea
        stop(paste("The willingness to pay parameter is defined in the interval [0-",he$Kmax,
                   "], with increments of ",he$step,"\n",sep=""))
      } else { # The user has actually specified wtp as input in the call to bcea
        tmp <- paste(he$k,collapse=" ")
        stop(paste0("The willingness to pay parameter is defined as:\n  [",tmp,"]\n  Please select a suitable value",collapse=" "))
      }
    }
    
    w <- which(he$k==wtp)
    if(he$n.comparisons==1) {
      nbw <- sd(he$ib[w,])/1.5
      d <- density(he$ib[w,],bw=bw,n=n)
      txt <- paste("Incremental Benefit distribution\n",he$interventions[he$ref],
                   " vs ",he$interventions[he$comp],sep="")
    }
    if(he$n.comparisons>1) {
      if(is.null(comparison)){
        comparison <- 1
      }
      nbw <- sd(he$ib[w,,comparison])/1.5
      d <- density(he$ib[w,,comparison],bw=bw,n=n)
      txt <- paste("Incremental Benefit distribution\n",he$interventions[he$ref],
                   " vs ",he$interventions[he$comp[comparison]],sep="")
    }
    if(is.null(xlim)==TRUE){
      xlim<-range(d$x)
    }
    plot(d$x,d$y,t="l",ylab="Density",xlab=expression(paste("IB(",bold(theta),")",sep="")),main=txt,axes=F,col="white",xlim=xlim)
    box()
    axis(1)
    ypt <- .95*max(d$y)
    xpt <- d$x[max(which(d$y>=ypt))]
    text(xpt,ypt,parse(text=paste("p(IB(",expression(bold(theta)),")>0,k==",format(wtp,digits=8,nsmall=2),")",sep="")),
         cex=.85,pos=4)
    xplus <- d$x[d$x>=0]
    yplus <- d$y[d$x>=0]
    polygon(c(0,xplus),c(0,yplus),density=20,border="white")
    points(d$x,d$y,t="l")
    abline(v=0,col="black")
    abline(h=0,col="black")
  }                   # ! base graphics
  else{
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))) {
      message("falling back to base graphics\n")
      ib.plot(he,comparison=comparison,wtp=wtp,bw=bw,n=n,xlim=xlim,graph="base")
      return(invisible(NULL))
    }
    
    ### no visible binding note
    x <- y <- NA_real_
    
    if(is.null(comparison)) {
      comparison <- 1
    }
    
    if(max(he$k)<wtp) {
      wtp <- max(he$k)
      message(paste0("NB: k (wtp) is defined in the interval [",min(he$k)," - ",wtp,"]\n"))
    }
    
    if (!is.element(wtp,he$k)) {
      if (!is.na(he$step)) {# The user has selected a non-acceptable value for wtp, but has not specified wtp in the call to bcea
        stop(paste("The willingness to pay parameter is defined in the interval [0-",he$Kmax,
                   "], with increments of ",he$step,"\n",sep=""))
      } else { # The user has actually specified wtp as input in the call to bcea
        tmp <- paste(he$k,collapse=" ")
        stop(paste0("The willingness to pay parameter is defined as:\n  [",tmp,"]\n  Please select a suitable value",collapse=" "))
      }
    }
    
    w <- which(he$k==wtp)
    if(he$n.comparisons==1) {
      nbw <- sd(he$ib[w,])/1.5
      density <- density(he$ib[w,],bw=bw,n=n)
      df <- data.frame("x"=density$x,"y"=density$y)
    }
    if(he$n.comparisons>1) {
      nbw <- sd(he$ib[w,,comparison])/1.5
      density <- density(he$ib[w,,comparison],bw=bw,n=n)
      df <- data.frame("x"=density$x,"y"=density$y)
    }
    if(is.null(xlim)){
      xlim<-range(df$x)
    }
    ib <- ggplot2::ggplot(df,ggplot2::aes(x,y)) + ggplot2::theme_bw() +
      ggplot2::geom_vline(xintercept=0,colour="grey50",size=0.5) + ggplot2::geom_hline(yintercept=0,colour="grey50",size=0.5) +
      ggplot2::geom_ribbon(data=subset(df,x>0),ggplot2::aes(ymax=y),ymin=0,fill="grey50",alpha=.2) + ggplot2::geom_line() +
      ggplot2::annotate(geom="text",label=paste0("p(IB(theta)>0,k==",wtp,")"),parse=T,x=df$x[which.max(df$y)],y=max(df$y),hjust=-.5,vjust=1,size=3.5) + ggplot2::coord_cartesian(xlim=xlim)
    
    labs.title <- paste0("Incremental Benefit Distribution\n",he$interventions[he$ref]," vs ",
                         he$interventions[he$comp[comparison]],"")
    
    ib <- ib + 
      ggplot2::theme(text=ggplot2::element_text(size=11),panel.grid=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),axis.ticks.y=ggplot2::element_blank()) +
      ggplot2::labs(title=labs.title,x=parse(text="IB(theta)"),y="Density") +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5))
    return(ib)
    
  } #! base.graphics
}


