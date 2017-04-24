###plot.CEriskav##################################################################################################
# Plots the EIB for the risk aversion case
plot.CEriskav <- function(x,pos=c(0,1),graph=c("base","ggplot2"),...) {
  options(scipen=10)
  
  alt.legend <- pos
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  
  howplot <- NULL
  exArgs <- list(...)
  if(length(exArgs)>=1)
    if(exists("plot",where=exArgs))
      howplot <- exArgs$plot
  
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
    
    plot(x$k,x$eibr[,1],t="l",xlab="Willingness to pay", ylab=" ", main="EIB as a function of the risk aversion parameter",ylim=range(x$eibr))
    linetype=seq(1,x$R)
    for (l in 2:x$R) {
      points(x$k,x$eibr[,l],t="l",lty=linetype[l])
    }
    text <- paste("r = ",x$r,sep=""); 
    # If the first value for r is small enough, consider it close to 0 and print the label accordingly
    if (x$r[1]<1e-8) {
      text[1] <- expression(r%->%0)
    }
    legend(alt.legend,text,lty=seq(1:x$R),cex=.9,box.lty=0)
    abline(h=0,col="grey")
    
    # Plots the EVPI for the risk aversion case
    if(is.null(howplot)) {
      if(!isTRUE(Sys.getenv("RSTUDIO")==1))
        dev.new()
    }
    else{
      opt <- c("x11","ask","dev.new")
      howplot <- ifelse(is.na(pmatch(howplot,opt)),"dev.new",opt[pmatch(howplot,opt)])
      if(howplot=="x11")
        dev.new()
      if(howplot=="dev.new")
        dev.new()
      if(howplot=="ask")
        devAskNewPage(ask=TRUE)
    }
    
    plot(x$k,x$evir[,1],t="l",ylim=range(x$evir),xlab="Willingness to pay",ylab=" ",main="EVPI as a function of the risk aversion parameter")
    for (l in 2:x$R) {
      points(x$k,x$evir[,l],t="l",lty=linetype[l])
    }
    legend(alt.legend,text,lty=seq(1:x$R),cex=.9,box.lty=0)
    abline(h=0,col="grey")
    
    if(!is.null(howplot))
      if(howplot=="ask")
        devAskNewPage(ask=FALSE)
    
  } # base.graphics
  else{
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))) {
      message("falling back to base graphics\n")
      plot.CEriskav(x,graph="base",pos=pos,...)
      return(invisible(NULL))
    }
    # no visible bindings note
    k <- r <- NA_real_
    
    linetypes <- rep(c(1,2,3,4,5,6),ceiling(x$R/6))[1:x$R]
    df <- data.frame(cbind(rep(x$k,x$R),c(x$eibr),c(x$evir)),as.factor(sort(rep(1:x$R,length(x$k)))))
    names(df) <- c("k","eibr","evir","r")
    
    # labels
    text <- paste0("r = ",x$r)
    # if the first value for r is small enough, consider it close to 0 and print the label accordingly
    if(x$r[1]<1e-8) {
      text[1] <- expression(r%->%0)
    }
    
    eibr <- ggplot2::ggplot(df,ggplot2::aes(x=k,y=eibr,linetype=r))+
      ggplot2::geom_hline(yintercept=0,linetype=1,colour="grey50")+ggplot2::geom_line()+
      ggplot2::scale_linetype_manual("",labels=text,values=linetypes)+ggplot2::theme_bw() +
      ggplot2::labs(title="EIB as a function of the risk aversion parameter",x="Willingness to pay",y="EIB") +
      ggplot2::theme(text=ggplot2::element_text(size=11),legend.key.size=grid::unit(.66,"line"),
                     legend.spacing=grid::unit(-1.25,"line"),panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank())
    
    ### evir ###
    evir <- ggplot2::ggplot(df,ggplot2::aes(x=k,y=evir,linetype=r))+ggplot2::geom_hline(yintercept=0,linetype=1,colour="grey50")+
      ggplot2::geom_line()+ggplot2::scale_linetype_manual("",labels=text,values=linetypes)+ggplot2::theme_bw() +
      ggplot2::labs(title="EVPI as a function of the risk aversion parameter",x="Willingness to pay",y="EVPI") +
      ggplot2::theme(text=ggplot2::element_text(size=11),legend.key.size=grid::unit(.66,"line"),
                     legend.spacing=grid::unit(-1.25,"line"),panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank())
    jus <- NULL
    if(isTRUE(alt.legend)) {
      alt.legend="bottom"
      eibr <- eibr + ggplot2::theme(legend.direction="vertical")
      evir <- evir + ggplot2::theme(legend.direction="vertical")
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
    
    eibr <- eibr + 
      ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),
                     legend.background=ggplot2::element_blank(),legend.text.align=0,
                     plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5))
    
    evir <- evir + 
      ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),
                     legend.background=ggplot2::element_blank(),
                     legend.text.align=0,plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5))
    plot(eibr)
    
    if(is.null(howplot)) {
      if(!isTRUE(Sys.getenv("RSTUDIO")==1))
        dev.new()
    }
    else{
      opt <- c("x11","ask","dev.new")
      howplot <- ifelse(is.na(pmatch(howplot,opt)),"dev.new",opt[pmatch(howplot,opt)])
      if(howplot=="x11")
        dev.new()
      if(howplot=="dev.new")
        dev.new()
      if(howplot=="ask")
        devAskNewPage(ask=TRUE)
    }
    
    plot(evir)
    
    if(!is.null(howplot))
      if(howplot=="ask")
        devAskNewPage(ask=FALSE)
    
    return(invisible(list("eib"=eibr,"evi"=evir)))
  }
  
}
