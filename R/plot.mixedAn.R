###plot.mixedAn###############################################################################################
plot.mixedAn <- function(x,y.limits=NULL,pos=c(0,1),graph=c("base","ggplot2"),...) {
  ## Plot the EVPI and the mixed strategy
  options(scipen=10)
  
  alt.legend <- pos
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  
  if(is.null(y.limits)){
    y.limits=range(x$evi,x$evi.star)
  }
  
  if(base.graphics) {
    
    if(is.numeric(alt.legend)&length(alt.legend)==2){
      temp <- ""
      if(alt.legend[2]==0)
        temp <- paste0(temp,"bottom")
      else
        temp <- paste0(temp,"top")
      if(alt.legend[1]==1)
        temp <- paste0(temp,"right")
      else
        temp <- paste0(temp,"left")
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
    
    plot(x$k,x$evi,t="l",xlab="Willingness to pay",ylab="EVPI",
         main="Expected Value of Information",ylim=y.limits)
    polygon(c(x$k,rev(x$k)),c(x$evi.star,rev(x$evi)),density=20,col="grey")
    points(x$k,x$evi.star,t="l",col="red")
    points(x$k,x$evi,t="l",col="black")
    txt <- c("Optimal strategy","Mixed strategy:",
             paste("   ",x$interventions,"=",format(100*x$mkt.shares,digits=3,nsmall=2),"%",sep=""))
    cols <- c("black","red",rep("white",length(x$interventions)))
    legend(alt.legend,txt,col=cols,cex=.6,bty="n",lty=1)
  } # base.graphics
  else {
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))) {
      message("Falling back to base graphics\n")
      plot.mixedAn(x,y.limits=y.limits,pos=pos,graph="base")
      return(invisible(NULL))
    } 
    
    if(isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))) {
      ### no visible binding note
      k <- evi.star <- NA_real_
      
      # legend
      txt <- c("Optimal strategy",paste0("Mixed strategy:", paste0("\n   ",x$interventions,"=",format(100*x$mkt.shares,digits=3,nsmall=2),"%",collapse="")))
      colours=c("black","red")
      
      df <- data.frame("k"=x$k,"evi"=x$evi,"evi.star"=x$evi.star)
      evi <- ggplot2::ggplot(df,ggplot2::aes(x=k)) + ggplot2::theme_bw() +
        ggplot2::geom_ribbon(ggplot2::aes(x=k,ymin=evi,ymax=evi.star),colour="lightgrey",alpha=.2) +
        ggplot2::geom_line(ggplot2::aes(x=k,y=evi,colour=as.factor(1))) +
        ggplot2::geom_line(ggplot2::aes(x=k,y=evi.star,colour=as.factor(2))) +
        ggplot2::coord_cartesian(ylim=y.limits,xlim=c(0,max(df$k))) +
        ggplot2::scale_colour_manual("",labels=txt,values=colours) +
        ggplot2::labs(title="Expected Value of Information",x="Willingness to pay",y="EVPI") +
        ggplot2::theme(text=ggplot2::element_text(size=11),legend.key.size=grid::unit(.66,"lines"),
                       legend.spacing=grid::unit(-1.25,"line"),panel.grid=ggplot2::element_blank(),
                       legend.key=ggplot2::element_blank(),plot.title=ggplot2::element_text(face="bold",hjust=0.5))
      jus <- NULL
      if(isTRUE(alt.legend)) {
        alt.legend="bottom"
        evi <- evi + ggplot2::theme(legend.direction="vertical")
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
          alt.legend <- c(0,1)
          jus <- alt.legend
        }
      }
      
      evi <- evi + 
        ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),
                       legend.background=ggplot2::element_blank(),
                       legend.text.align=0,plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5))
      return(evi)
    }
  }
}
