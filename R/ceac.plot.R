###ceac.plot##################################################################################################
## Plots the CEAC
ceac.plot <- function(he,comparison=NULL,pos=c(1,0),graph=c("base","ggplot2")) {
  options(scipen=10)
  
  alt.legend <- pos
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  
  if(base.graphics) {
    if(is.numeric(alt.legend)&length(alt.legend)==2){
      temp <- ""
      if(alt.legend[2]==1)
        temp <- paste0(temp,"top")
      else
        temp <- paste0(temp,"bottom")
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
        alt.legend="bottomright"
      else
        alt.legend="bottomleft"
    }
    
    if(he$n.comparisons==1) {
      plot(he$k,he$ceac,t="l",xlab="Willingness to pay",ylab="Probability of cost effectiveness",
           ylim=c(0,1),main="Cost Effectiveness\nAcceptability Curve")
    }
    if(he$n.comparisons>1&is.null(comparison)) {
      color <- rep(1,he$n.comparisons); lwd <- 1
      if (he$n.comparisons>6) {
        cl <- colors()
        color <- cl[floor(seq(262,340,length.out=he$n.comparators))]	# gray scale
        lwd <- 1.5
      }
      
      plot(he$k,he$ceac[,1],t="l",xlab="Willingness to pay",ylab="Probability of cost effectiveness",
           ylim=c(0,1),main="Cost Effectiveness\nAcceptability Curve",lty=1,lwd=lwd)
      for (j in 2:he$n.comparisons) {
        points(he$k,he$ceac[,j],t="l",col=color[j],lty=j,lwd=lwd)
      }
      text <- paste(he$interventions[he$ref]," vs ",he$interventions[he$comp])
      legend(alt.legend,text,col=color,cex=.7,bty="n",lty=1:he$n.comparisons)
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
      
      ceac.plot(he,pos=alt.legend,graph="base")
    }
  } # base.graphics
  else{
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))){
      message("falling back to base graphics\n")
      ceac.plot(he,pos=alt.legend,graph="base"); return(invisible(NULL))
    }
    
    # no visible binding note
    k = NA_real_
    
    if(he$n.comparisons==1) {
      data.psa <- with(he,data.frame("k"=k,"ceac"=ceac))
      ceac <- ggplot2::ggplot(data.psa, ggplot2::aes(k,ceac)) + ggplot2::geom_line() 
    }
    if(he$n.comparisons>1 & is.null(comparison)==TRUE) {
      data.psa <- with(he,data.frame("k"=c(k),"ceac"=c(ceac),"comparison"=as.factor(sort(rep(1:n.comparisons,length(k))))))
      
      # labels for legend
      comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
      
      # linetype is the indicator
      linetypes <- rep(c(1,2,3,4,5,6),ceiling(he$n.comparisons/6))[1:he$n.comparisons]
      
      ceac <- ggplot2::ggplot(data.psa,ggplot2::aes(k,ceac,linetype=comparison)) +
        ggplot2::geom_line() +
        ggplot2::scale_linetype_manual("",labels=comparisons.label,values=linetypes)
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
      
      return(ceac.plot(he,pos=alt.legend,graph="ggplot2"))
    }
    
    ceac <- ceac + ggplot2::theme_bw() + 
      ggplot2::scale_y_continuous(limits=c(0,1)) +
      ggplot2::labs(title="Cost-Effectiveness Acceptability Curve",x="Willingness to pay",y="Probability of cost-effectiveness") 
    
    jus <- NULL
    if(isTRUE(alt.legend)) {
      alt.legend="bottom"
      ceac <- ceac + ggplot2::theme(legend.direction="vertical")
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
    
    ceac <- ceac + 
      ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),
                     legend.background=ggplot2::element_blank(),text=ggplot2::element_text(size=11),
                     legend.key.size=grid::unit(.66,"lines"),legend.spacing=grid::unit(-1.25,"line"),
                     panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank(),legend.text.align=0,
                     plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5))
    return(ceac)
  } # !base.graphics
}