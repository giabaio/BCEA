###eib.plot###################################################################################################
## Plots the EIB


#' Expected Incremental Benefit (EIB) plot
#' 
#' Produces a plot of the Expected Incremental Benefit (EIB) as a function of
#' the willingness to pay
#' 
#' 
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param comparison Selects the comparator, in case of more than two
#' interventions being analysed.  Default as \code{NULL} plots all the
#' comparisons together. Any subset of the possible comparisons can be selected
#' (e.g., \code{comparison=c(1,3)} or \code{comparison=2}).
#' @param pos Parameter to set the position of the legend; for a single
#' comparison plot, the ICER legend position. Can be given in form of a string
#' \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#' which specifies the relative position on the x and y axis respectively, or
#' alternatively it can be in form of a logical variable, with \code{FALSE}
#' indicating to use the default position and \code{TRUE} to place it on the
#' bottom of the plot. Default value is \code{c(1,0)}, that is the bottomright
#' corner inside the plot area.
#' @param size Value (in millimetres) of the size of the willingness to pay
#' label. Used only if \code{graph="ggplot2"}, otherwise it will be ignored
#' with a message. If set to \code{NA}, the break-even point line(s) and
#' label(s) are suppressed, with both base graphics and ggplot2.
#' @param plot.cri Logical value. Should the credible intervals be plotted
#' along with the expected incremental benefit? Default as \code{NULL} draws
#' the 95\% credible intervals if only one comparison is selected, and does not
#' include them for multiple comparisons.  Setting \code{plot.cri=TRUE} or
#' \code{plot.cri=FALSE} forces the function to add the intervals or not. The
#' level of the intervals can be also set, see \ldots{} for more details.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  If \code{graph="ggplot2"} and a named theme object is supplied,
#' it will be added to the ggplot object. If \code{plot.cri=TRUE} the level of
#' the interval can be set using the argument \code{alpha}, with default at
#' \code{alpha=0.05}. Additionally the method of calculation of the credible
#' intervals can be chosen with the option \code{cri.quantile}: the default
#' value \code{TRUE} indicates that the credible intervals are defined as the
#' interval between the \code{alpha/2}-th and \code{1-alpha/2}-th quantiles of
#' the IB distribution. Setting \code{cri.quantile=FALSE} will use a normal
#' approximation on the IB distribution to calculate the intervals.
#' @return \item{eib}{ A ggplot object containing the requested plot. Returned
#' only if \code{graph="ggplot2"}. } The function produces a plot of the
#' Expected Incremental Benefit as a function of the discrete grid
#' approximation of the willingness to pay parameter. The break even point
#' (i.e. the point in which the EIB=0, ie when the optimal decision changes
#' from one intervention to another) is also showed by default. The value k* is
#' the discrete grid approximation of the ICER.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{ib.plot}},
#' \code{\link{ceplane.plot}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Expected Incremental Benefit
#' @export eib.plot
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


