##########################################ceplane.plot########################################################
## Plots the CE Plane


#' Cost-effectiveness plane plot
#' 
#' Produces a scatter plot of the cost-effectiveness plane, together with the
#' sustainability area, as a function of the selected willingness to pay
#' threshold
#' 
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param comparison Selects the comparator, in case of more than two
#' interventions being analysed. Default as \code{NULL} plots all the
#' comparisons together. Any subset of the possible comparisons can be selected
#' (e.g., \code{comparison=c(1,3)} or \code{comparison=2}).
#' @param wtp The value of the willingness to pay parameter. Not used iff
#' \code{graph="base"} for multiple comparisons.
#' @param pos Parameter to set the position of the legend; for a single
#' comparison plot, the ICER legend position. Can be given in form of a string
#' \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#' which specifies the relative position on the x and y axis respectively, or
#' alternatively it can be in form of a logical variable, with \code{FALSE}
#' indicating to use the default position and \code{TRUE} to place it on the
#' bottom of the plot. Default value is \code{c(1,1)}, that is the topright
#' corner inside the plot area.
#' @param size Value (in millimetres) of the size of the willingness to pay
#' label. Used only if \code{graph="ggplot2"}, otherwise is ignored with a
#' message.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param xlim The range of the plot along the x-axis. If NULL (default) it is
#' determined by the range of the simulated values for \code{delta.e}
#' @param ylim The range of the plot along the y-axis. If NULL (default) it is
#' determined by the range of the simulated values for \code{delta.c}
#' @param col A vector of colours used to plot the cost-effectiveness plane.
#' This have to have length equal to the number of comparisons.
#' @param ...  If \code{graph="ggplot2"} and a named theme object is supplied,
#' it will be added to the ggplot object. In addition, if
#' \code{graph="ggplot2"}, \code{ICER.size} can be used to resize the red dot
#' representing the ICER (i.e. showing them if multiple comparisons are
#' selected), and \code{label.pos=FALSE} will place the willingness to pay
#' label in a different position at the bottom of the graph.
#' @return \item{ceplane}{ A ggplot object containing the plot. Returned only
#' if \code{graph="ggplot2"}. } The function produces a plot of the
#' cost-effectiveness plane. Grey dots show the simulated values for the joint
#' distribution of the effectiveness and cost differentials.  The larger red
#' dot shows the ICER and the grey area identifies the sustainability area,
#' i.e. the part of the plan for which the simulated values are below the
#' willingness to pay threshold. The proportion of points in the sustainability
#' area effectively represents the CEAC for a given value of the willingness to
#' pay. If the comparators are more than 2 and no pairwise comparison is
#' specified, all scatterplots are graphed using different colors.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Cost Effectiveness Plane
#' @examples
#' 
#' ### create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m <- bcea(e,c,ref=4,Kmax=500,interventions=treats)
#' ### produce the plot
#' ceplane.plot(m,wtp=200,graph="base")
#' ### select only one comparator
#' ceplane.plot(m,wtp=200,graph="base",comparator=3)
#' ### or use ggplot2 instead
#' if(requireNamespace("ggplot2")){
#' ceplane.plot(m,wtp=200,pos="right",ICER.size=2,graph="ggplot2")
#' }
#' 
#' @export ceplane.plot
ceplane.plot <- function(he,comparison=NULL,wtp=25000,pos=c(1,1),
                         size=NULL,graph=c("base","ggplot2"),
                         xlim=NULL,ylim=NULL,...) {
  ### hidden options for ggplot2 ###
  # ICER.size =                    # changes ICER point size
  # label.pos = FALSE              # uses alternate position for wtp label (old specification)
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  alt.legend <- pos
  
  # Forces R to avoid scientific format for graphs labels
  options(scipen=10)
  
  # Additional/optional arguments
  exArgs <- list(...)
  if(!exists("xlab",where=exArgs)){xlab <- "Effectiveness differential"} else {xlab <- exArgs$xlab}
  if(!exists("ylab",where=exArgs)){ylab <- "Cost differential"} else {ylab <- exArgs$ylab}
  if(!exists("ICER.col",where=exArgs)){ICER.col <- "red"} else {ICER.col <- exArgs$ICER.col}
  if(!exists("title",where=exArgs)){title <- paste("Cost effectiveness plane \n",he$interventions[he$ref]," vs ",he$interventions[he$comp],sep="")} 
  else {title <- exArgs$title}
  
  if(base.graphics) {
    if(!is.null(size))
      message("option size will be ignored using base graphics")
    
    if(is.numeric(alt.legend)&length(alt.legend)==2){
      temp <- ""
      if(alt.legend[2]==0)
        temp <- paste0(temp,"bottom")
      else
        temp <- paste0(temp,"top")
      if(alt.legend[1]==0)
        temp <- paste0(temp,"left")
      else
        temp <- paste0(temp,"right")
      alt.legend <- temp
      if(length(grep("^(bottom|top)(left|right)$",temp))==0)
        alt.legend <- FALSE
    }
    if(is.logical(alt.legend)){
      if(!alt.legend)
        alt.legend="topright"
      else
        alt.legend="topleft"
    }
    
    # Encodes characters so that the graph can be saved as ps or pdf
    ps.options(encoding="CP1250")
    pdf.options(encoding="CP1250")
    
    if(he$n.comparisons==1) {
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
      if(!is.null(xlim)) {m.e <- xlim[1]; M.e <- xlim[2]}
      if(!is.null(ylim)) {m.c <- ylim[1]; M.c <- ylim[2]}
      plot(xx,yy,col="white",xlim=c(m.e,M.e),ylim=c(m.c,M.c),
           xlab=xlab,ylab=ylab,main=title,axes=F)
      polygon(c(min(xx),seq(min(xx),max(xx),step),max(xx)),
              c(min(yy),wtp*seq(min(xx),max(xx),step),min(yy)),
              col="grey95",border="black")
      #  polygon(c(xx,xx),c(yy,rev(yy)),col="grey95",border="black")
      axis(1); axis(2); box()
      points(he$delta.e,he$delta.c,pch=20,cex=.35,col="grey55")
      abline(h=0,col="dark grey")
      abline(v=0,col="dark grey")
      text(M.e,M.c,paste("\U2022"," ICER=",format(he$ICER,digits=6,nsmall=2),sep=""),cex=.95,pos=2,col=ICER.col)
      points(mean(he$delta.e),mean(he$delta.c),pch=20,col=ICER.col,cex=1)
      t1 <- paste("k==",format(wtp,digits=3,nsmall=2,scientific=F),sep="")
      text(x.pt,y.pt,parse(text=t1),cex=.8,pos=4)
    }
    if(he$n.comparisons>1 & is.null(comparison)==TRUE) { 
      if(!exists("title",where=exArgs)){title <- "Cost-effectiveness plane"} else {title <- exArgs$title}
      if(!exists("col",where=exArgs)){
         cl <- colors()
         color <- cl[floor(seq(262,340,length.out=he$n.comparators))]  # gray scale
      } else {
         color=exArgs$col
         if(length(color)!=he$n.comparisons){stop("If you want to specify the vector of colours, make sure you select as many colours as the number of comparisons!")}
      }
      if(is.null(xlim)) {xlim <- range(he$delta.e)}
      if(is.null(ylim)) {ylim <- range(he$delta.c)}
      plot(he$delta.e[,1],he$delta.c[,1],pch=20,cex=.35,xlim=xlim,ylim=ylim,
           xlab=xlab,ylab=ylab,main=title)
      for (i in 2:he$n.comparisons) {
        points(he$delta.e[,i],he$delta.c[,i],pch=20,cex=.35,col=color[i])
      }
      abline(h=0,col="dark grey")
      abline(v=0,col="dark grey")
      text <- paste(he$interventions[he$ref]," vs ",he$interventions[he$comp])
      legend(alt.legend,text,col=color,cex=.7,bty="n",lty=1)
    }
    if(he$n.comparisons>1 & is.null(comparison)==FALSE & length(comparison)==1) { 
      if(!exists("title",where=exArgs)){title <- paste("Cost effectiveness plane \n",
                                                       he$interventions[he$ref]," vs ",he$interventions[he$comp[comparison]],sep="")} else {title <- exArgs$title}
      m.e <- range(he$delta.e[,comparison])[1]
      M.e <- range(he$delta.e[,comparison])[2]
      m.c <- range(he$delta.c[,comparison])[1]
      M.c <- range(he$delta.c[,comparison])[2]
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
      if(!is.null(xlim)) {m.e <- xlim[1]; M.e <- xlim[2]}
      if(!is.null(ylim)) {m.c <- ylim[1]; M.c <- ylim[2]}
      plot(xx,yy,col="white",xlim=c(m.e,M.e),ylim=c(m.c,M.c),
           xlab=xlab,ylab=ylab,main=title,axes=F)
      polygon(c(min(xx),seq(min(xx),max(xx),step),max(xx)),
              c(min(yy),wtp*seq(min(xx),max(xx),step),min(yy)),
              col="grey95",border="black")
      axis(1); axis(2); box()
      points(he$delta.e[,comparison],he$delta.c[,comparison],pch=20,cex=.35,col="grey55")
      abline(h=0,col="dark grey")
      abline(v=0,col="dark grey")
      text(M.e,M.c,paste("\U2022"," ICER=",format(he$ICER[comparison],digits=6,nsmall=2),sep=""),cex=.95,pos=2,col="red")
      points(mean(he$delta.e[,comparison]),mean(he$delta.c[,comparison]),pch=20,col="red",cex=1)
      t1 <- paste("k==",format(wtp,digits=3,nsmall=2,scientific=F),sep="")
      text(x.pt,y.pt,parse(text=t1),cex=.8,pos=4)
    }
    if(he$n.comparisons>1&is.null(comparison)==FALSE&length(comparison)!=1) {
      stopifnot(all(comparison %in% 1:he$n.comparisons))
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
      
      return(ceplane.plot(he,wtp=wtp,pos=alt.legend,graph="base",size=size,...))
    }
  } #if(base.graphics)
  else{
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE) & requireNamespace("grid",quietly=TRUE))){
      message("Falling back to base graphics\n")
      ceplane.plot(he,comparison=comparison,wtp=wtp,pos=alt.legend,graph="base"); return(invisible(NULL))
    }
    # no visible binding note
    
    if(isTRUE(requireNamespace("ggplot2",quietly=TRUE) & requireNamespace("grid",quietly=TRUE))){
      
      delta.e <- delta.c <- lambda.e <- lambda.c <- NULL
      
      if(is.null(size)) {size <- ggplot2::rel(3.5)}
      
      label.pos <- TRUE
      opt.theme <- ggplot2::theme()
      ICER.size <- ifelse(he$n.comparisons==1,2,0)
      exArgs <- list(...)
      if(length(exArgs)>=1){
        if(exists("ICER.size",where=exArgs))
          ICER.size <- exArgs$ICER.size
        if(exists("label.pos",where=exArgs))
          if(is.logical(exArgs$label.pos))
            label.pos <- exArgs$label.pos
          for(obj in exArgs)
            if(ggplot2::is.theme(obj))
              opt.theme <- opt.theme + obj
      }
      
      if(he$n.comparisons==1) {
        kd <- data.frame(he$delta.e,he$delta.c)
        names(kd) <- c("delta.e","delta.c")
        # for scale_x_continuous(oob=)
        do.nothing=function(x,limits) return(x)
        # plot limits
        range.e <- range(kd$delta.e)
        range.c <- range(kd$delta.c)
        range.e[1] <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
        range.c[1] <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
        # ce plane data
        x1 <- range.e[1]-2*abs(diff(range.e))
        x2 <- range.e[2]+2*abs(diff(range.e))
        x3 <- x2
        x <- c(x1,x2,x3)
        y <- x*wtp; y[3] <- x1*wtp
        plane <- data.frame(x=x,y=y)
        
        # build a trapezoidal plane instead of a triangle if the y value is less than the minimum difference on costs
        if(y[1]>1.2*range.c[1]) {
          plane <- rbind(plane,
                         c(x2,2*range.c[1]), #new bottom-right vertex
                         c(x1,2*range.c[1])) #new bottom-left vertex
        }
        
        # actual plot
        ceplane <- ggplot2::ggplot(kd, ggplot2::aes(delta.e,delta.c)) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(limits=range.e,oob=do.nothing) + 
          ggplot2::scale_y_continuous(limits=range.c,oob=do.nothing) +
          ggplot2::scale_color_manual("",labels=paste0("ICER = ",format(he$ICER,digits=6,nsmall=2),"  "),values="red") +     
          ggplot2::geom_line(data=plane[1:2,],ggplot2::aes(x=x,y=y),color="black",linetype=1) +
          ggplot2::geom_polygon(data=plane,ggplot2::aes(x=x,y=y),fill="light gray",alpha=.3) +
          ggplot2::geom_hline(ggplot2::aes(yintercept=0),colour="grey") + 
          ggplot2::geom_vline(ggplot2::aes(xintercept=0),colour="grey") +
          ggplot2::geom_point(size=1,colour="grey33") +
          ggplot2::geom_point(ggplot2::aes(mean(delta.e),mean(delta.c),color=as.factor(1)),size=ICER.size)
        
        if(!label.pos) {
          # moves the wtp label depending on whether the line crosses the y-axis
          ceplane <- ceplane + ggplot2::annotate(geom="text",x=ifelse(range.c[1]/wtp>range.e[1],range.c[1]/wtp,range.e[1]),
                                                 #y=(1+.085)*range.c[1],
                                                 y=range.c[1],
                                                 label=paste0("k = ",format(wtp,digits=6)),hjust=-.15,size=size)
        }
        else{
          m.e <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
          m.c <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
          x.pt <- .95*m.e
          y.pt <- ifelse(x.pt*wtp<m.c,m.c,x.pt*wtp)
          ceplane <- ceplane + ggplot2::annotate(geom="text",x=x.pt,y=y.pt,
                                                 label=paste0("k = ",format(wtp,digits=6)),hjust=.15,size=size)
        }
      }
      
      if(he$n.comparisons>1&is.null(comparison)==TRUE) {
        
        # create dataframe for plotting
        kd <- with(he,data.frame(c(delta.e),c(delta.c)))
        names(kd) <- c("delta.e","delta.c")
        kd$comparison <- as.factor(sort(rep(1:he$n.comparisons,dim(he$delta.e)[1])))
        
        # dataset for ICERs
        means <- matrix(NA_real_,nrow=he$n.comparisons,ncol=2)
        for(i in 1:he$n.comparisons)
          means[i,] <- colMeans(kd[kd$comparison==i,-3])
        means <- data.frame(means)
        means$comparison <- factor(1:he$n.comparisons)
        names(means) <- c("lambda.e","lambda.c","comparison")
        
        # labels for legend
        comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
        # vector of values for color, take out white, get integer values
        if(!exists("col",where=exArgs)){
           colors.label <- with(he,paste0("gray",round(seq(0,100,length.out=(n.comparisons+1))[-(n.comparisons+1)])))
        } else {
           colors.label=exArgs$col
        if(length(colors.label)!=he$n.comparisons){stop("If you want to specify the vector of colours, make sure you select as many colours as the number of comparisons!")}
        }


 ###       colors.label <- with(he,paste0("gray",round(seq(0,100,length.out=(n.comparisons+1))[-(n.comparisons+1)])))
        
        # polygon
        do.nothing=function(x,limits) return(x)
        # plot limits
        range.e <- range(kd$delta.e)
        range.c <- range(kd$delta.c)
        range.e[1] <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
        range.c[1] <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
        # ce plane data
        x1 <- range.e[1]-2*abs(diff(range.e))
        x2 <- range.e[2]+2*abs(diff(range.e))
        x3 <- x2
        x <- c(x1,x2,x3)
        y <- x*wtp; y[3] <- x1*wtp
        plane <- data.frame(x=x,y=y,comparison=factor(rep(he$n.comparisons+1,3)))
        
        # build a trapezoidal plane instead of a triangle if the y value is less than the minimum difference on costs
        if(y[1]>min(kd$delta.c)) {
          plane <- rbind(plane,
                         c(x2,2*min(kd$delta.c),he$n.comparisons+1), #new bottom-right vertex
                         c(x1,2*min(kd$delta.c),he$n.comparisons+1)) #new bottom-left vertex
        }
        
        ceplane <-
          ggplot2::ggplot(kd,ggplot2::aes(x=delta.e,y=delta.c,col=comparison)) +
          ggplot2::theme_bw() +
          ggplot2::scale_color_manual(labels=comparisons.label,values=colors.label,na.value="black") +
          ggplot2::scale_x_continuous(limits=range.e,oob=do.nothing) +
          ggplot2::scale_y_continuous(limits=range.c,oob=do.nothing) +
          ggplot2::annotate("line",x=plane[1:2,1],y=plane[1:2,2],colour="black") +
          ggplot2::annotate("polygon",plane$x,plane$y,fill="light grey",alpha=.3) +
          ggplot2::geom_hline(ggplot2::aes(yintercept=0),colour="grey") + ggplot2::geom_vline(ggplot2::aes(xintercept=0),colour="grey") +
          ggplot2::geom_point(size=1)+
          ggplot2::geom_point(data=means,ggplot2::aes(x=lambda.e,y=lambda.c),colour="red",size=ICER.size)
        
        # wtp label
        if(!label.pos){
          ceplane <- ceplane +
            ggplot2::annotate(geom="text",
                              x=ifelse(range.c[1]/wtp>range.e[1],range.c[1]/wtp,range.e[1]),
                              y=range.c[1],
                              label=paste0("k = ",format(wtp,digits=6),"  "),hjust=.15,size=size
            )
        }
        else{
          m.e <- ifelse(range.e[1]<0,range.e[1],-range.e[1])
          m.c <- ifelse(range.c[1]<0,range.c[1],-range.c[1])
          x.pt <- .95*m.e
          y.pt <- ifelse(x.pt*wtp<m.c,m.c,x.pt*wtp)
          ceplane <- ceplane + ggplot2::annotate(geom="text",x=x.pt,y=y.pt,
                                                 label=paste0("k = ",format(wtp,digits=6)),hjust=.15,size=size)
        }
      }
      
      if(he$n.comparisons>1&is.null(comparison)==FALSE) {
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
        
        return(ceplane.plot(he,wtp=wtp,pos=alt.legend,graph="ggplot2",size=size,...))
      }
      
      if(!exists("title",where=exArgs)) {
        labs.title <- "Cost-Effectiveness Plane"
        labs.title <- with(he,paste0(labs.title,
                                     ifelse(n.comparisons==1,
                                            paste0("\n",interventions[ref]," vs ",interventions[-ref]),
                                            paste0(
                                              ifelse(isTRUE(he$mod),
                                                     paste0("\n",interventions[ref]," vs ",
                                                            paste0(interventions[comp],collapse=", ")),
                                                     "")))))
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
          alt.legend <- c(1,1)
          jus <- alt.legend
        }
      }
      
      ceplane <- ceplane + 
        ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),legend.background=ggplot2::element_blank()) +
        ggplot2::theme(text=ggplot2::element_text(size=11),legend.key.size=grid::unit(.66,"lines"),legend.spacing=grid::unit(-1.25,"line"),panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank(),legend.text.align=0) +
        ggplot2::theme(plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5)) +
        opt.theme
      
      if(he$n.comparisons==1)
        ceplane  <- ceplane + ggplot2::theme(legend.key.size=grid::unit(.1,"lines")) + opt.theme
      
      return(ceplane)
    }
  } # !base.graphics
  
}

