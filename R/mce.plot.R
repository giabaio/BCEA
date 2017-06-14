############mce.plot###################################
mce.plot <- function(mce,pos=c(1,0.5),graph=c("base","ggplot2"),...){
  alt.legend <- pos
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  
  exArgs <- list(...)
  # Allows to specify colours for the plots
  # If the user doesn't specify anything, use defaults
  if(!exists("color",exArgs)) {
    color <- rep(1,(mce$n.comparators+1)); lwd <- 1
    if (mce$n.comparators>7) {
      cl <- colors()
      color <- cl[floor(seq(262,340,length.out=mce$n.comparators))]	# gray scale
      lwd <- 1.5
    }  
  } 
  # If the user specify colours, then use but check they are the right number
  if(exists("color",exArgs)) {
     color <- exArgs$color; lwd <- 1
     if(mce$n.comparators>7) {lwd=1.5}
     if (length(color)!=(mce$n.comparators))	 {
        message(paste0("You need to specify ",(mce$n.comparators)," colours. Falling back to default\n"))
     }
  } 

  if(base.graphics) {
    
    if(is.numeric(alt.legend)&length(alt.legend)==2){
      temp <- ""
      if(alt.legend[2]==0)
        temp <- paste0(temp,"bottom")
      else if(alt.legend[2]!=0.5)
        temp <- paste0(temp,"top")
      if(alt.legend[1]==1)
        temp <- paste0(temp,"right")
      else
        temp <- paste0(temp,"left")
      alt.legend <- temp
      if(length(grep("^((bottom|top)(left|right)|right)$",temp))==0)
        alt.legend <- FALSE
    }
    if(is.logical(alt.legend)){
      if(!alt.legend)
        alt.legend="topright"
      else
        alt.legend="right"
    }
    
#    color <- rep(1,(mce$n.comparators+1)); lwd <- 1
#    if (mce$n.comparators>7) {
#      cl <- colors()
#      color <- cl[floor(seq(262,340,length.out=mce$n.comparators))]	# gray scale
#      lwd <- 1.5
#    }
    
    plot(mce$k,mce$m.ce[,1],t="l",col=color[1],lwd=lwd,lty=1,xlab="Willingness to pay",
         ylab="Probability of most cost effectiveness",ylim=c(0,1),
         main="Cost-effectiveness acceptability curve \nfor multiple comparisons")
    for (i in 2:mce$n.comparators) {
      points(mce$k,mce$m.ce[,i],t="l",col=color[i],lwd=lwd,lty=i)
    }
    legend(alt.legend,mce$interventions,col=color,cex=.7,bty="n",lty=1:mce$n.comparators)
  } # base graphics
  else{
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))) {
      message("Falling back to base graphics\n")
      mce.plot(mce,pos=pos,graph="base")
      return(invisible(NULL))
    }
    
    if(isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))) {
      # no visible bindings note
      ceplane <- k <- ce <- comp <- NA_real_
      
      alt.legend <- pos
      lty <- rep(1:6,ceiling(mce$n.comparators/6))[1:mce$n.comparators]
      label <- paste0(mce$interventions)
      
      df <- cbind("k"=rep(mce$k,mce$n.comparators),"ce"=c(mce$m.ce))
      df <- data.frame(df,"comp"=as.factor(sort(rep(1:mce$n.comparators,length(mce$k)))))
      names(df) <- c("k","ce","comp")
      
      mceplot <- ggplot2::ggplot(df,ggplot2::aes(x=k,y=ce)) + ggplot2::theme_bw() +
        ggplot2::geom_line(ggplot2::aes(linetype=comp)) + 
        ggplot2::scale_linetype_manual("",labels=label,values=lty) +
        ggplot2::labs(title="Cost-effectiveness acceptability curve\nfor multiple comparisons",x="Willingness to pay",y="Probability of most cost effectiveness") +
        ggplot2::theme(text=ggplot2::element_text(size=11),legend.key.size=grid::unit(.66,"lines"),
                       legend.spacing=grid::unit(-1.25,"line"),panel.grid=ggplot2::element_blank(),
                       legend.key=ggplot2::element_blank())
      
      jus <- NULL
      if(isTRUE(alt.legend)) {
        alt.legend="bottom"
        mceplot <- mceplot + ggplot2::theme(legend.direction="vertical")
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
          alt.legend <- c(1,0.5)
          jus <- alt.legend
        }
      }
      
      mceplot <- mceplot + ggplot2::coord_cartesian(ylim=c(-0.05,1.05)) +
        ggplot2::theme(legend.position=alt.legend,legend.justification=jus,legend.title=ggplot2::element_blank(),
                       legend.background=ggplot2::element_blank(),
                       legend.text.align=0,plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5))
      return(mceplot)
    }
  }
}
