#####


#' Specialised contour plot for objects in the class "bcea"
#' 
#' Produces a scatterplot of the cost-effectiveness plane, with a contour-plot
#' of the bivariate density of the differentials of cost (y-axis) and
#' effectiveness (x-axis).  Also adds the sustainability area (i.e. below the
#' selected value of the willingness-to-pay threshold).
#' 
#' 
#' @param he A "bcea" object containing the results of the Bayesian modelling
#' and the economic evaluation
#' @param wtp The selected value of the willingness-to-pay. Default is
#' \code{25000}.
#' @param xlim Limits on the x-axis (default=\code{NULL}, so that R will select
#' appropriate limits).
#' @param ylim Limits on the y-axis (default=\code{NULL}, so that R will select
#' appropriate limits).
#' @param comparison The comparison being plotted. Default to \code{NULL}
#' chooses the first comparison if \code{graph="base"}. If
#' \code{graph="ggplot2"} the default value will choose all the possible
#' comparisons. Any subset of the possible comparisons can be selected (e.g.,
#' \code{comparison=c(1,3)}).
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  Arguments to be passed to \code{\link{ceplane.plot}}. See the
#' relative manual page for more details.
#' @return \item{contour}{ A ggplot item containing the requested plot.
#' Returned only if \code{graph="ggplot2"}. } Plots the cost-effectiveness
#' plane with a scatterplot of all the simulated values from the (posterior)
#' bivariate distribution of (Delta_e,Delta_c), the differentials of
#' effectiveness and costs; superimposes a contour of the distribution and
#' prints the value of the ICER, together with the sustainability area.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{ceplane.plot}},
#' \code{\link{contour.bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Bayesian model
#' @examples
#' 
#' ### create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m=bcea(e,c,ref=4,interventions=treats,Kmax=500)
#' ### produce the plot
#' contour2(m,wtp=200,graph="base")
#' \donttest{
#' ### or use ggplot2 to plot multiple comparisons
#' contour2(m,wtp=200,ICER.size=2,graph="ggplot2")
#' }
#' 
#' @export contour2
contour2 <- function(he,wtp=25000,xlim=NULL,ylim=NULL,comparison=NULL,graph=c("base","ggplot2"),...) {
  # Forces R to avoid scientific format for graphs labels
  options(scipen=10)
  
  # Additional/optional arguments
  exArgs <- list(...)
  if(!exists("xlab",where=exArgs)){xlab <- "Effectiveness differential"} else {xlab <- exArgs$xlab}
  if(!exists("ylab",where=exArgs)){ylab <- "Cost differential"} else {ylab <- exArgs$ylab}
  if(!exists("title",where=exArgs)){
    title <- paste("Cost effectiveness plane \n",he$interventions[he$ref]," vs ",he$interventions[he$comp],sep="")} 
  else {title <- exArgs$title
  }
  
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  
  if(base.graphics) {
    # Encodes characters so that the graph can be saved as ps or pdf
    ps.options(encoding="CP1250")
    pdf.options(encoding="CP1250")
    
    # Selects the first comparison by default if not selected
    if(is.null(comparison)){
      message("The first available comparison will be selected. To plot multiple comparisons together please use the ggplot2 version. Please see ?contour2 for additional details.")
      comparison <- 1
    }
    
    if(he$n.comparisons>1) {
      if(!exists("title",where=exArgs)){title <- paste("Cost effectiveness plane contour plot \n",he$interventions[he$ref]," vs ",
                                                       he$interventions[he$comp[comparison]],sep="")} 
      else {title <- exArgs$title}
      
      
      he$delta.e <- he$delta.e[,comparison]
      he$delta.c <- he$delta.c[,comparison]
      he$comp <- he$comp[comparison]
      he$ICER <- he$ICER[comparison]
    }
    
    m.e <- range(he$delta.e)[1]
    M.e <- range(he$delta.e)[2]
    m.c <- range(he$delta.c)[1]
    M.c <- range(he$delta.c)[2]
    step <- (M.e-m.e)/10
    
    m.e <- ifelse(m.e<0,m.e,-m.e)
    m.c <- ifelse(m.c<0,m.c,-m.c)
    
    x.pt <- .95*m.e
    y.pt <- ifelse(x.pt*wtp<m.c,m.c,x.pt*wtp)
    xx <- seq(100*m.c/wtp,100*M.c/wtp,step)
    yy <- xx*wtp          
    xx[1] <- ifelse(min(xx)<m.e,xx[1],2*m.e)
    yy[1] <- ifelse(min(yy)<m.c,yy[1],2*m.c)
    xx[length(xx)] <- ifelse(xx[length(xx)]<M.e,1.5*M.e,xx[length(xx)])
    
    # If the user has specified x- and/or y-limits then use those
    if(!is.null(xlim)) {m.e <- xlim[1]; M.e <- xlim[2]}
    if(!is.null(ylim)) {m.c <- ylim[1]; M.c <- ylim[2]}
    
    plot(xx,yy,col="white",xlim=c(m.e,M.e),ylim=c(m.c,M.c), 
         xlab=xlab,ylab=ylab,
         main=title,axes=F)
    polygon(c(min(xx),seq(min(xx),max(xx),step),max(xx)),
            c(min(yy),wtp*seq(min(xx),max(xx),step),min(yy)),
            col="grey95",border="black")
    #	polygon(c(xx,xx),c(yy,rev(yy)),col="grey95",border="black")
    axis(1); axis(2); box()
    points(he$delta.e,he$delta.c,pch=20,cex=.35,col="grey55")
    abline(h=0,col="dark grey")
    abline(v=0,col="dark grey")
    text(M.e,M.c,paste("\U2022"," ICER=",format(he$ICER,digits=6,nsmall=2),sep=""),cex=.95,pos=2,col="red")
    points(mean(he$delta.e),mean(he$delta.c),pch=20,col="red",cex=1)
    t1 <- paste("k==",format(wtp,digits=3,nsmall=2,scientific=F),sep="")
    text(x.pt,y.pt,parse(text=t1),cex=.8,pos=4)
    
    # And then plots the contour
    requireNamespace("MASS")
    offset <- 1.0
    nlevels <- 4
    scale <- 0.5
    
    density <- MASS::kde2d(he$delta.e,he$delta.c,n=300,h=c(sd(he$delta.e)/scale,sd(he$delta.c)/scale))
    
    m.c <- range(he$delta.c)[1]; M.c <- range(he$delta.c)[2]
    m.e <- range(he$delta.e)[1]; M.e <- range(he$delta.e)[2]
    
    # Changes the range so that the plot always shows the x and y axes
    ch1 <- ifelse(m.e>0,m.e<--m.e,m.e<-m.e)
    ch2 <- ifelse(M.e<0,M.e<--M.e,M.e<-M.e)
    ch3 <- ifelse(m.c>0,m.c<--m.c,m.c<-m.c)
    ch4 <- ifelse(M.c<0,M.c<--M.c,M.c<-M.c)
    
    par(new=TRUE)
    contour(density$x,density$y,density$z,add=TRUE,nlevels=nlevels,drawlabels=FALSE,lwd=1.5)
    return(invisible(NULL))
  } # end if base.graphics
  else{
    
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))){
      message("falling back to base graphics\n")
      contour2(he,comparison=comparison,xlim=xlim,ylim=ylim,wtp=wtp,graph="base"); return(invisible(NULL))
    }
    scale=0.5
    nlevels=5
    
    requireNamespace("MASS")
    
    ### no visible binding note
    z <- e <- NA_real_
    
    if(he$n.comparisons==1) {
      density <- with(he,MASS::kde2d(delta.e,delta.c,n=300,h=c(sd(delta.e)/scale,sd(delta.c)/scale)))
      density <- data.frame(expand.grid("e"=density$x,"c"=density$y),"z"=as.vector(density$z))
      contour <- ceplane.plot(he,wtp=wtp,graph="ggplot2",...) +
        ggplot2::geom_contour(ggplot2::aes(z=z,x=e,y=c),data=density,colour="black",bins=nlevels)
    }
    if(he$n.comparisons>1&is.null(comparison)) {
      densitydf <- data.frame()
      for(i in 1:he$n.comparisons) {
        density <- with(he,MASS::kde2d(delta.e[,i],delta.c[,i],n=300,h=c(sd(delta.e[,i])/scale,sd(delta.c[,i])/scale)))
        densitydf <- rbind(densitydf,cbind(expand.grid(density$x,density$y),as.vector(density$z)))
      }
      names(densitydf) <- c("e","c","z")
      densitydf <- cbind(densitydf,"comparison"=as.factor(sort(rep(1:he$n.comparisons,dim(densitydf)[1]/he$n.comparisons))))
      contour <- ceplane.plot(he,wtp=wtp,graph="ggplot2",...) +
        ggplot2::geom_contour(data=densitydf,ggplot2::aes(x=e,y=c,z=z,colour=comparison),bins=nlevels,linetype=1)
    }
    if(he$n.comparisons>1&!is.null(comparison)) {
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[,comparison]
      he$delta.c <- he$delta.c[,comparison]
      he$n.comparators=length(comparison)+1
      he$n.comparisons=length(comparison)
      he$interventions=he$interventions[sort(c(he$ref,he$comp))]
      he$ICER=he$ICER[comparison]
      he$ib=he$ib[,,comparison]
      he$eib=he$eib[,comparison]
      he$U=he$U[,,sort(c(he$ref,comparison+1))]
      he$ceac=he$ceac[,comparison]
      he$ref=rank(c(he$ref,he$comp))[1]
      he$comp=rank(c(he$ref,he$comp))[-1]
      he$mod <- TRUE #
      
      return(contour2(he,wtp=wtp,xlim=xlim,ylim=ylim,comparison=NULL,graph="ggplot2",...))
    }
    
    contour <- contour + ggplot2::coord_cartesian(xlim=xlim,ylim=ylim)
    return(contour)
  } # end if !base.graphics
  
}
