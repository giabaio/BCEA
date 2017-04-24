###eib.plot###################################################################################################
## Plots the EIB
eib.plot <- function(he,comparison=NULL,pos=c(1,0),size=NULL,plot.cri=NULL,graph=c("base","ggplot2"),...) {
  
  options(scipen=10)
  alt.legend <- pos
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  
  ### evaluate arguments. possibility to include different values of confidence as "alpha"
  exArgs <- list(...)
  alpha <- 0.05
  cri.quantile <- TRUE
  if(length(exArgs)>=1){
    if(exists("cri.quantile",where=exArgs))
      cri.quantile <- exArgs$cri.quantile
    if(exists("alpha",where=exArgs)){
      alpha <- exArgs$alpha
      if(alpha<0 | alpha>1) {
        warning("Argument alpha must be between 0 and 1. Reset to default at 0.95")
        alpha <- 0.05
      }
      if(alpha>0.80 & cri.quantile) {
        warning("It is recommended adopting the normal approximation of the credible interval for high values of alpha. Please set the argument cri.quantile=FALSE to use the normal approsimation.")
      }
    }
  }
  
  ### function to calculate the credible intervals
  eib.plot.cri <- function(he,alpha,cri.quantile) {
    if(alpha<0 | alpha>1) {
      warning("Argument alpha must be between 0 and 1. Reset to default at 0.95")
      alpha <- 0.05
    }
    margin <- 1
    if(he$n.comparison>1) margin <- c(1,3)
    cri <- data.frame("low"=c(apply(he$ib,margin,function(x) ifelse(cri.quantile,
                                                                    quantile(x,(alpha)/2),
                                                                    mean(x)-qnorm((alpha)/2)*sd(x)
    ))),
    "upp"=c(apply(he$ib,margin,function(x) ifelse(cri.quantile,
                                                  quantile(x,1-(alpha)/2),
                                                  mean(x)-qnorm(1-(alpha)/2)*sd(x)
    ))),
    "comp"=as.factor(sort(rep(1:he$n.comparisons,length(he$k)))))
    return(cri)
  }
  ### if plot.cri is null, if comp=1 plot them otherwise do not (clutter!)
  if(is.null(plot.cri) & isTRUE(he$n.comparisons==1 | is.null(comparison)))
    plot.cri <- ifelse(he$n.comparisons==1,TRUE,FALSE)
  
  ### calculate credible intervals if necessary
  if(isTRUE(plot.cri))
    cri <- eib.plot.cri(he,alpha,cri.quantile)
  
  ### calculate plot vertical limits
  yl <- ifelse(rep(!isTRUE(plot.cri),2),
               range(c(he$eib)),
               range(c(he$eib),c(cri[,1:2])))
  
  if(base.graphics) {
    if(!is.null(size)){
      if(!is.na(size)){
        message("Option size will be ignored using base graphics.")
        size <- NULL
      }
    }
    
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
        alt.legend="topleft"
      else
        alt.legend="topright"
    }
    
    if(he$n.comparisons==1) {
      plot(NULL,xlab="Willingness to pay",ylab="EIB",ylim=yl,xlim=range(he$k),
           main=paste0("Expected Incremental Benefit",ifelse(plot.cri,paste0("\nand ",format((1-alpha)*100,digits=4),"% credible intervals"),"")))
      ### x axis
      abline(h=0,col="grey")
      ### EIB
      lines(he$k,he$eib)
      ### CRI
      if(plot.cri){
        lines(he$k,cri$low,col="grey50",lty=2)
        lines(he$k,cri$upp,col="grey50",lty=2)
      }
      ### BEP
      if(length(he$kstar)>0 & is.null(size)) {
        abline(v=he$kstar,col="dark grey",lty="dotted")
        text(he$kstar,min(yl),paste("k* = ",he$kstar,sep=""))
      }
      if(isTRUE(he$mod))
        legend(alt.legend,paste0(he$interventions[he$ref]," vs ",he$interventions[he$comp]),cex=.7,bty="n",lty=1,lwd=1)
    }
    if(he$n.comparisons>1&is.null(comparison)) {
      lwd <- ifelse(he$n.comparisons>6,1.5,1)
      plot(NULL,xlab="Willingness to pay", ylab="EIB",ylim=yl,xlim=range(he$k),
           main=paste0("Expected Incremental Benefit",ifelse(plot.cri,paste0("\nand ",format((1-alpha)*100,digits=4),"% credible intervals"),"")))
      abline(h=0,col="grey")
      for (j in 1:he$n.comparisons){
        lines(he$k,he$eib[,j],lty=j,lwd=ifelse(plot.cri,lwd+1,lwd))
        if(plot.cri){
          lines(he$k,cri$low[cri$comp==j],lty=j,lwd=lwd,col="grey50")
          lines(he$k,cri$upp[cri$comp==j],lty=j,lwd=lwd,col="grey50")
        }
      }
      if(length(he$kstar)>0 & is.null(size)) {
        abline(v=he$kstar,col="dark grey",lty="dotted")
        text(he$kstar,min(yl),paste("k* = ",he$kstar,sep=""))
      }
      text <- paste0(he$interventions[he$ref]," vs ",he$interventions[he$comp])
      legend(alt.legend,text,cex=.7,bty="n",lty=1:he$n.comparisons,lwd=ifelse(plot.cri,lwd+1,lwd))
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
      
      eib.plot(he,pos=alt.legend,graph="base",size=size,comparison=NULL,plot.cri=plot.cri,alpha=alpha,cri.quantile=cri.quantile,...)
    }
  } # base.graphics
  else{
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))){
      message("falling back to base graphics\n")
      eib.plot(he,pos=alt.legend,graph="base"); return(invisible(NULL))
    }
    
    ### no visible binding note
    k <- kstar <- low <- upp <- NA_real_
    
    if(is.null(size))
      size=ggplot2::rel(3.5)
    
    opt.theme <- ggplot2::theme()
    for(obj in exArgs)
      if(ggplot2::is.theme(obj))
        opt.theme <- opt.theme + obj
    
    if(he$n.comparisons==1) {
      # data frame
      data.psa <- with(he,data.frame("k"=k,"eib"=eib,"comparison"=as.factor(sort(rep(1:n.comparisons,length(he$k))))))
      if(plot.cri)
        data.psa <- cbind(data.psa,cri)
      eib <- ggplot2::ggplot(data.psa, ggplot2::aes(k,eib)) +
        ggplot2::theme_bw() +
        ggplot2::geom_hline(ggplot2::aes(yintercept=0),colour="grey")
      if(!isTRUE(he$mod)){
        eib <- eib + ggplot2::geom_line()
      }
      else{
        eib <- eib + ggplot2::geom_line(ggplot2::aes(linetype=comparison)) +
          ggplot2::scale_linetype_manual("",values=1,labels=with(he,paste0(interventions[ref]," vs ",interventions[comp])))
      }
      
      if(!length(he$kstar)==0 & !is.na(size)) {
        # label
        label <- paste0("k* = ",format(he$kstar,digits=6))
        eib <- eib +
          ggplot2::geom_vline(ggplot2::aes(xintercept=kstar),data=data.frame("kstar"=he$kstar),colour="grey50",linetype=2,size=.5) +
          ggplot2::annotate("text",label=label,x=he$kstar,y=min(yl),hjust=ifelse((max(he$k)-he$kstar)/max(he$k)>1/6,-.1,1.1),size=size)
      }
      
      if(plot.cri){
        eib <- eib +
          ggplot2::geom_line(ggplot2::aes(y=low),colour="grey50",lty=2) +
          ggplot2::geom_line(ggplot2::aes(y=upp),colour="grey50",lty=2)
      }
    }
    if(he$n.comparisons>1&is.null(comparison)==TRUE) {
      data.psa <- with(he,data.frame("k"=c(k),"eib"=c(eib),"comparison"=as.factor(sort(rep(1:n.comparisons,length(he$k))))))
      if(plot.cri)
        data.psa <- cbind(data.psa,cri)
      # labels for legend
      comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
      
      # linetype is the indicator of the comparison.
      # 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
      linetypes <- rep(c(1,2,3,4,5,6),ceiling(he$n.comparisons/6))[1:he$n.comparisons]
      
      eib <- 
        ggplot2::ggplot(data.psa,ggplot2::aes(x=k,y=eib,linetype=comparison)) + 
        ggplot2::geom_hline(yintercept=0,linetype=1,color="grey") + 
        ggplot2::theme_bw() +
        ggplot2::geom_line(lwd=ifelse(!plot.cri,0.5,0.75)) +
        ggplot2::scale_linetype_manual("",labels=comparisons.label,values=linetypes)
      
      if(!length(he$kstar)==0 & !is.na(size)) {
        # label
        label <- paste0("k* = ",format(he$kstar,digits=6))
        eib <-eib +
          ggplot2::geom_vline(ggplot2::aes(xintercept=kstar),data=data.frame("kstar"=he$kstar),colour="grey50",linetype=2,size=.5) + 
          ggplot2::annotate("text",label=label,x=he$kstar,y=min(yl),hjust=ifelse((max(he$k)-he$kstar)/max(he$k)>1/6,-.1,1.1),size=size,vjust=1)
      }
      
      if(plot.cri){
        eib <- eib +
          ggplot2::geom_line(ggplot2::aes(y=low),colour="grey50",show_guide=F)+
          ggplot2::geom_line(ggplot2::aes(y=upp),colour="grey50",show_guide=F)
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
      
      return(eib.plot(he,pos=alt.legend,graph="ggplot2",size=size,comparison=NULL,plot.cri=plot.cri,alpha=alpha,cri.quantile=cri.quantile,...))
    }
    
    eib <- eib + 
      ggplot2::labs(x="Willingness to pay",y="EIB",
                    title=paste0("Expected Incremental Benefit",
                                 ifelse(plot.cri,paste0("\nand ",format((1-alpha)*100,digits=4),"% credible intervals"),"")))
    
    jus <- NULL
    if(isTRUE(alt.legend)) {
      alt.legend="bottom"
      eib <- eib + ggplot2::theme(legend.direction="vertical")
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
    eib <- eib + 
      ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),
                     legend.background=ggplot2::element_blank(),text=ggplot2::element_text(size=11),
                     legend.key.size=grid::unit(.66,"lines"),legend.spacing=grid::unit(-1.25,"line"),
                     panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank(),legend.text.align=0,
                     plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5)) +
      opt.theme
    
    return(eib)
  } # !base.graphics
}


