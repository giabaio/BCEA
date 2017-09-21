## Contour plots for the cost-effectiveness plane


#' Contour method for objects in the class \code{bcea}
#' 
#' Produces a scatterplot of the cost-effectiveness plane, with a contour-plot
#' of the bivariate density of the differentials of cost (y-axis) and
#' effectiveness (x-axis)
#' 
#' 
#' @param x A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation
#' @param comparison In case of more than 2 interventions being analysed,
#' selects which plot should be made.  By default the first comparison among
#' the possible ones will be plotted. If \code{graph="ggplot2"} any subset of
#' the possible comparisons can be selected, and \code{comparison=NULL} will
#' yield a plot of all the possible comparisons together.
#' @param scale Scales the plot as a function of the observed standard
#' deviation.
#' @param levels Numeric vector of levels at which to draw contour lines. Will
#' be ignored using \code{graph="ggplot2"}.
#' @param nlevels Number of levels to be plotted in the contour.
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom}, \code{top}, \code{left} or \code{right} for ggplot2. It can
#' be a two-elements vector, which specifies the relative position on the x and
#' y axis respectively, or alternatively it can be in form of a logical
#' variable, with \code{FALSE} indicating to use the default position and
#' \code{TRUE} to place the legend on the bottom of the plot. Default value is
#' \code{c(1,0)}, that is the bottomright corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param xlim The range of the plot along the x-axis. If NULL (default) it is
#' determined by the range of the simulated values for \code{delta.e}
#' @param ylim The range of the plot along the y-axis. If NULL (default) it is
#' determined by the range of the simulated values for \code{delta.c}
#' @param ...  Additional arguments to 'plot.window', 'title', 'Axis' and
#' 'box', typically graphical parameters such as 'cex.axis'. Will be ignored if
#' \code{graph="ggplot2"}.
#' @return \item{ceplane}{ A ggplot object containing the plot. Returned only
#' if \code{graph="ggplot2"}. } Plots the cost-effectiveness plane with a
#' scatterplot of all the simulated values from the (posterior) bivariate
#' distribution of (Delta_e,Delta_c), the differentials of effectiveness and
#' costs; superimposes a contour of the distribution and prints the estimated
#' value of the probability of each quadrant (combination of positive/negative
#' values for both Delta_e and Delta_c)
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{ceplane.plot}},
#' \code{\link{contour2}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Bayesian model
#' @export contour.bcea
contour.bcea <- function(x,comparison=1,scale=0.5,nlevels=4,levels=NULL,pos=c(1,0),
                         xlim=NULL,ylim=NULL,graph=c("base","ggplot2"),...) {
  requireNamespace("MASS")
  options(scipen=10)
  # comparison selects which plot should be made
  # by default it is the first possible
  
  # Additional/optional arguments
  exArgs <- list(...)
  if(!exists("xlab",where=exArgs)){xlab <- "Effectiveness differential"} else {xlab <- exArgs$xlab}
  if(!exists("ylab",where=exArgs)){ylab <- "Cost differential"} else {ylab <- exArgs$ylab}
  if(!exists("title",where=exArgs)){title <- paste("Cost effectiveness plane contour plot\n",x$interventions[x$ref]," vs ",x$interventions[x$comp],sep="")} 
  else {title <- exArgs$title}
  
  alt.legend <- pos
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  
  if(base.graphics){
    
    if(is.null(comparison) | length(comparison) > 1){
      message("The first available comparison will be selected. To plot multiple comparisons together please use the ggplot2 version. Please see ?contour.bcea for additional details.")
      comparison <- 1
    }
    
    if (x$n.comparisons==1) {
      density <- MASS::kde2d(x$delta.e,x$delta.c,n=300,h=c(sd(x$delta.e)/scale,sd(x$delta.c)/scale))
      offset <- 1.0
      
      p.ne <- sum(x$delta.e>0 & x$delta.c>0)/x$n.sim
      p.nw <- sum(x$delta.e<=0 & x$delta.c>0)/x$n.sim
      p.sw <- sum(x$delta.e<=0 & x$delta.c<=0)/x$n.sim
      p.se <- sum(x$delta.e>0 & x$delta.c<=0)/x$n.sim
      
      m.c <- range(x$delta.c)[1]; M.c <- range(x$delta.c)[2]
      m.e <- range(x$delta.e)[1]; M.e <- range(x$delta.e)[2]
      
      # Changes the range so that the plot always shows the x and y axes
      ch1 <- ifelse(m.e>0,m.e<--m.e,m.e<-m.e)
      ch2 <- ifelse(M.e<0,M.e<--M.e,M.e<-M.e)
      ch3 <- ifelse(m.c>0,m.c<--m.c,m.c<-m.c)
      ch4 <- ifelse(M.c<0,M.c<--M.c,M.c<-M.c)
      
      # If the user has specified the range of the graph, use those values
      if(!is.null(xlim)) {m.e <- xlim[1]; M.e <- xlim[2]}
      if(!is.null(ylim)) {m.c <- ylim[1]; M.c <- ylim[2]}
      
      plot(x$delta.e,x$delta.c,pch=20,cex=.3,col="dark grey",xlab=xlab,
           ylab=ylab,main=title,
           xlim=c(m.e,M.e),ylim=c(m.c,M.c))
      abline(h=0,col="dark grey")
      abline(v=0,col="dark grey")
      if(any(is.na(density$z))==FALSE) {
        if (is.null(levels)==FALSE){
          # Normalise the density and use levels in the contour
          density$z <- (density$z-min(density$z))/(max(density$z)-min(density$z))
          contour(density$x,density$y,density$z,add=TRUE,levels=levels,drawlabels=TRUE)
        }
        if (is.null(levels)==TRUE) {
          contour(density$x,density$y,density$z,add=TRUE,nlevels=nlevels,drawlabels=FALSE)
        }
      }
      t1 <- paste("Pr(Delta[e]>0, Delta[c]>0)==",format(p.ne,digits=4,nsmall=3),sep="")
      text(offset*M.e,offset*M.c,parse(text=t1),cex=.8,pos=2)
      t2 <- paste("Pr(Delta[e]<=0, Delta[c]>0)==",format(p.nw,digits=4,nsmall=3),sep="")
      text(offset*m.e,offset*M.c,parse(text=t2),cex=.8,pos=4)
      t3 <- paste("Pr(Delta[e]<=0, Delta[c]<=0)==",format(p.sw,digits=4,nsmall=3),sep="")
      text(offset*m.e,offset*m.c,parse(text=t3),cex=.8,pos=4)
      t4 <- paste("Pr(Delta[e]>0, Delta[c]<=0)==",format(p.se,digits=4,nsmall=3),sep="")
      text(offset*M.e,offset*m.c,parse(text=t4),cex=.8,pos=2)
    }
    
    if(x$n.comparisons>1) {
      if(!exists("title",where=exArgs)){title <- paste("Cost effectiveness plane contour plot \n",x$interventions[x$ref]," vs ",
                                                       x$interventions[x$comp[comparison]],sep="")} 
      else {title <- exArgs$title}
      
      density <- MASS::kde2d(x$delta.e[,comparison],x$delta.c[,comparison],n=300,
                             h=c(sd(x$delta.e[,comparison])/scale,sd(x$delta.c[,comparison])/scale))
      offset <- 1.0
      
      p.ne <- sum(x$delta.e[,comparison]>0 & x$delta.c[,comparison]>0)/x$n.sim
      p.nw <- sum(x$delta.e[,comparison]<=0 & x$delta.c[,comparison]>0)/x$n.sim
      p.sw <- sum(x$delta.e[,comparison]<=0 & x$delta.c[,comparison]<=0)/x$n.sim
      p.se <- sum(x$delta.e[,comparison]>0 & x$delta.c[,comparison]<=0)/x$n.sim
      
      m.c <- range(x$delta.c[,comparison])[1]; M.c <- range(x$delta.c[,comparison])[2]
      m.e <- range(x$delta.e[,comparison])[1]; M.e <- range(x$delta.e[,comparison])[2]
      
      # Changes the range so that the plot always shows the x and y axes
      ch1 <- ifelse(m.e>0,m.e<--m.e,m.e<-m.e)
      ch2 <- ifelse(M.e<0,M.e<--M.e,M.e<-M.e)
      ch3 <- ifelse(m.c>0,m.c<--m.c,m.c<-m.c)
      ch4 <- ifelse(M.c<0,M.c<--M.c,M.c<-M.c)
      
      # If the user has specified the range of the graph, use those values
      if(!is.null(xlim)) {m.e <- xlim[1]; M.e <- xlim[2]}
      if(!is.null(ylim)) {m.c <- ylim[1]; M.c <- ylim[2]}
      
      plot(x$delta.e[,comparison],x$delta.c[,comparison],pch=20,cex=.3,col="dark grey",
           xlab=xlab,ylab=ylab,
           main=title,xlim=c(m.e,M.e),ylim=c(m.c,M.c))	
      abline(h=0,col="dark grey")
      abline(v=0,col="dark grey")
      if(any(is.na(density$z))==FALSE) {
        contour(density$x,density$y,density$z,add=TRUE,drawlabels=TRUE)
        if (is.null(levels)==FALSE){
          # Normalise the density and use levels in the contour
          density$z <- (density$z-min(density$z))/(max(density$z)-min(density$z))
          contour(density$x,density$y,density$z,add=TRUE,levels=levels,drawlabels=TRUE)
        }
        if (is.null(levels)==TRUE) {
          contour(density$x,density$y,density$z,add=TRUE,nlevels=nlevels,drawlabels=FALSE)
        }
      }
      t1 <- paste("Pr(Delta[e]>0, Delta[c]>0)==",format(p.ne,digits=4,nsmall=3),sep="")
      text(offset*M.e,offset*M.c,parse(text=t1),cex=.8,pos=2)
      t2 <- paste("Pr(Delta[e]<=0, Delta[c]>0)==",format(p.nw,digits=4,nsmall=3),sep="")
      text(offset*m.e,offset*M.c,parse(text=t2),cex=.8,pos=4)
      t3 <- paste("Pr(Delta[e]<=0, Delta[c]<=0)==",format(p.sw,digits=4,nsmall=3),sep="")
      text(offset*m.e,offset*m.c,parse(text=t3),cex=.8,pos=4)
      t4 <- paste("Pr(Delta[e]>0, Delta[c]<=0)==",format(p.se,digits=4,nsmall=3),sep="")
      text(offset*M.e,offset*m.c,parse(text=t4),cex=.8,pos=2)
    }
  } # if base.graphics
  else{
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))){
      message("falling back to base graphics\n")
      contour.bcea(x,comparison=comparison,scale=scale,nlevels=nlevels,pos=alt.legend,levels=levels,graph="base",...)
      return(invisible(NULL))
    }
    
    if(!is.null(levels))
      message("option level will be ignored using ggplot2 graphics")
    
    # no visible binding note
    delta.e <- delta.c <- e <- z <- y <- hjust <- label <- NULL
    
    if(!is.null(nlevels)){
      nlevels <- round(nlevels)
      if(nlevels<0)
        nlevels <- 10
      if(nlevels==0)
        nlevels <- 1
    }
    
    if(x$n.comparisons==1) {
      kd <- with(x,data.frame("e"=delta.e,"c"=delta.c))
      
      # for scale_x_continuous(oob=)
      do.nothing=function(x,limits) return(x)
      # plot limits
      range.e <- range(kd$e)
      range.c <- range(kd$c)
      range.e[1] <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
      range.c[1] <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
      
      # labels
      p.ne <- sum(x$delta.e > 0 & x$delta.c > 0)/x$n.sim
      p.ne <- paste0("Pr(Delta[e]>0, Delta[c]>0)==",format(p.ne,digits=4,nsmall=3))
      p.nw <- sum(x$delta.e <= 0 & x$delta.c > 0)/x$n.sim
      p.nw <- paste0("Pr(Delta[e]<=0, Delta[c]>0)==",format(p.nw,digits=4,nsmall=3))
      p.sw <- sum(x$delta.e <= 0 & x$delta.c <= 0)/x$n.sim
      p.sw <- paste0("Pr(Delta[e]<=0, Delta[c]<=0)==",format(p.sw,digits=4,nsmall=3))
      p.se <- sum(x$delta.e > 0 & x$delta.c <= 0)/x$n.sim
      p.se <- paste0("Pr(Delta[e]>0, Delta[c]<=0)==",format(p.se,digits=4,nsmall=3))
      
      # labels dataframe
      labels.df <- data.frame(
        "x"=c(range.e[2],range.e[1],range.e[1],range.e[2]),
        "y"=c(rep(range.c[2],2),rep(range.c[1],2)),
        "label"=c(p.ne,p.nw,p.sw,p.se),
        "hjust"=as.factor(c(1,0,0,1)))
      
      # actual plot
      points.colour="grey"
      if(nlevels==1)
        points.colour="black"
      ceplane <- ggplot2::ggplot(kd, ggplot2::aes(e,c)) + ggplot2::geom_hline(ggplot2::aes(yintercept=0),colour="grey") + 
        ggplot2::geom_vline(ggplot2::aes(xintercept=0),colour="grey") + ggplot2::theme_bw() + 
        ggplot2::geom_point(size=1,color=points.colour) + ggplot2::scale_x_continuous(limits=range.e,oob=do.nothing) + 
        ggplot2::scale_y_continuous(limits=range.c,oob=do.nothing)
      if(!is.null(scale)&requireNamespace("MASS",quietly=TRUE)){
        density <- with(x,MASS::kde2d(delta.e,delta.c,n=300,h=c(sd(delta.e)/scale,sd(delta.c)/scale)))
        density <- data.frame(expand.grid("e"=density$x,"c"=density$y),"z"=as.vector(density$z))
        ceplane <- ceplane + ggplot2::geom_contour(ggplot2::aes(z=z),data=density,colour="black",bins=nlevels)
      }
      else{
        ceplane <- ceplane + ggplot2::stat_density2d(color="black")
      }
      
      
      ceplane <- ceplane + 
        ggplot2::geom_text(data=labels.df,ggplot2::aes(x=x,y=y,hjust=hjust,label=label),parse=TRUE,size=ggplot2::rel(3.5))
    }
    if(x$n.comparisons>1&is.null(comparison)==TRUE) {
      # creates dataframe for plotting
      kd <- with(x,data.frame(
        "delta.e"=c(delta.e),"delta.c"=c(delta.c),
        "comparison"=as.factor(sort(rep(1:n.comparisons,dim(delta.e)[1])))))
      
      # vector of values for color, take out white, get integer values
      colors.label <- paste0("gray",round(seq(0,100,length.out=(x$n.comparisons+1))[-(x$n.comparisons+1)]))
      comparisons.label <- paste0(x$interventions[x$ref]," vs ",x$interventions[x$comp])
      do.nothing=function(x,limits) return(x)
      # plot limits
      range.e <- range(kd$delta.e)
      range.c <- range(kd$delta.c)
      range.e[1] <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
      range.c[1] <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
      
      ceplane <-
        ggplot2::ggplot(kd,ggplot2::aes(x=delta.e,y=delta.c,col=comparison)) +
        ggplot2::geom_hline(yintercept=0,colour="grey") + ggplot2::geom_vline(xintercept=0,colour="grey") + ggplot2::theme_bw() +
        ggplot2::geom_point(size=1) +
        ggplot2::scale_color_manual(label=comparisons.label,values=colors.label,na.value="black") +
        ggplot2::scale_x_continuous(limits=range.e,oob=do.nothing) +
        ggplot2::scale_y_continuous(limits=range.c,oob=do.nothing)
      
      if(!is.null(scale)&requireNamespace("MASS",quietly=TRUE)) {
        densitydf <- data.frame()
        for(i in 1:x$n.comparison) {
          temp <- with(x,MASS::kde2d(delta.e[,i],delta.c[,i],n=300,h=c(sd(delta.e[,i])/scale,sd(delta.c[,i])/scale)))
          temp <- data.frame(expand.grid("e"=temp$x,"c"=temp$y),"z"=as.vector(temp$z))
          densitydf <- rbind(densitydf,cbind(temp,rep(i,dim(temp)[[1]])))
        }
        names(densitydf) <- c("delta.e","delta.c","z","comparison")
        densitydf$comparison <- as.factor(densitydf$comparison)
        ceplane <- ceplane + ggplot2::geom_contour(ggplot2::aes(z=z,colour=comparison),data=densitydf,bins=nlevels) +
          ggplot2::guides(colour=ggplot2::guide_legend(override.aes=list(linetype=0)))
      }
      else{
        ceplane <- ceplane + ggplot2::stat_density2d() + 
          ggplot2::guides(colour=ggplot2::guide_legend(override.aes=list(linetype=0)))
      }
    }
    if(x$n.comparisons>1&is.null(comparison)==FALSE) {
      # adjusts bcea object for the correct number of dimensions and comparators
      x$comp <- x$comp[comparison]
      x$delta.e <- x$delta.e[,comparison]
      x$delta.c <- x$delta.c[,comparison]
      x$n.comparators=length(comparison)+1
      x$n.comparisons=length(comparison)
      x$interventions=x$interventions[sort(c(x$ref,x$comp))]
      x$ICER=x$ICER[comparison]
      x$ib=x$ib[,,comparison]
      x$eib=x$eib[,comparison]
      x$U=x$U[,,sort(c(x$ref,comparison+1))]
      x$ceac=x$ceac[,comparison]
      x$ref=rank(c(x$ref,x$comp))[1]
      x$comp=rank(c(x$ref,x$comp))[-1]
      x$mod <- TRUE #
      
      return(contour.bcea(x,scale=scale,pos=alt.legend,nlevels=nlevels,graph="ggplot2",comparison=NULL))
    }
    
    if(!exists("title",where=exArgs)) {
      labs.title <- "Cost-Effectiveness Plane"
      labs.title <- paste0(labs.title,
                           ifelse(x$n.comparisons==1,
                                  paste0("\n",x$interventions[x$ref]," vs ",x$interventions[-x$ref]),
                                  paste0(
                                    ifelse(isTRUE(x$mod),
                                           paste0("\n",x$interventions[x$ref]," vs ",
                                                  paste0(x$interventions[x$comp],collapse=", ")),
                                           ""))))
    } else {labs.title <- exArgs$title}
    
    ceplane <- ceplane + ggplot2::labs(title=labs.title,x=xlab,y=ylab)
    
    jus <- NULL
    if(isTRUE(alt.legend)) {
      alt.legend="bottom"
      ceplane <- ceplane + ggplot2::theme(legend.direction="vertical")
    }
    else{
      if(is.character(alt.legend)) {
        choices <- c("left", "right", "bottom", "top")
        alt.legend <- choices[pmatch(alt.legend,choices)]
        jus="center"
        if(is.na(alt.legend))
          alt.legend=FALSE
      }
      if(length(alt.legend)>1)
        jus <- alt.legend
      if(length(alt.legend)==1 & !is.character(alt.legend)) {
        alt.legend <- c(1,0)
        jus <- alt.legend
      }
    }
    
    ceplane <- ceplane + 
      ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),
                     legend.background=ggplot2::element_blank(),text=ggplot2::element_text(size=11),
                     legend.key.size=grid::unit(.66,"lines"),legend.spacing=grid::unit(-1.25,"line"),
                     panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank(),legend.text.align=0,
                     plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5))
    return(ceplane)
  } # !base.graphics
}
