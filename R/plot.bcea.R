###plot.bcea##################################################################################################
## Plots the main health economics outcomes in just one graph
plot.bcea <- function(x,comparison=NULL,wtp=25000,pos=FALSE,graph=c("base","ggplot2"),...) {
  options(scipen=10)
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  
  if(base.graphics) {
    op <- par(mfrow=c(2,2))
    ceplane.plot(x,comparison=comparison,wtp=wtp,pos=pos,graph="base",...)
    eib.plot(x,comparison=comparison,pos=pos,graph="base",...)
    ceac.plot(x,comparison=comparison,pos=pos,graph="base")
    evi.plot(x,graph="base")
    par(op)
  }
  else{
    
    if(!requireNamespace("ggplot2",quietly=TRUE) & !requireNamespace("grid",quietly=TRUE)){
      message("falling back to base graphics\n")
      plot.bcea(x,comparison=comparison,wtp=wtp,pos=pos,graph="base",...)
      return(invisible(NULL))
    }
    
    ####### multiplot ###### 
    # source: R graphics cookbook
    if(requireNamespace("ggplot2",quietly=TRUE) & requireNamespace("grid",quietly=TRUE)){
      multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        plots <- c(list(...),plotlist)
        numPlots = length(plots)
        if(is.null(layout)) {
          layout <- matrix(seq(1,cols*ceiling(numPlots/cols)),
                           ncol=cols, nrow=ceiling(numPlots/cols))
        }
        if(numPlots==1) {
          print(plots[[1]])
        } else {
          grid::grid.newpage()
          grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow(layout),ncol(layout))))
          
          for(i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout==i,arr.ind=TRUE))
            print(plots[[i]],vp=grid::viewport(layout.pos.row=matchidx$row,
                                               layout.pos.col=matchidx$col))
          }
        }
      } #### multiplot end ####
      
      theme.multiplot <- 
        ggplot2::theme(text=ggplot2::element_text(size=9),legend.key.size=grid::unit(.5,"lines"),
                       legend.spacing=grid::unit(-1.25,"line"),panel.grid=ggplot2::element_blank(),
                       legend.key=ggplot2::element_blank(),plot.title=ggplot2::element_text(lineheight=1,face="bold",size=11.5,hjust=0.5))
      
      exArgs <- list(...)
      for(obj in exArgs)
        if(ggplot2::is.theme(obj))
          theme.multiplot <- theme.multiplot + obj
      
      ceplane.pos <- pos
      if(isTRUE(pos==FALSE)){
        ceplane.pos <- c(1,1.025)
      }
      ceplane <- ceplane.plot(x,wtp=wtp,pos=ceplane.pos,comparison=comparison,graph="ggplot2",...) +
        theme.multiplot
      eib <- eib.plot(x,pos=pos,comparison=comparison,graph="ggplot2",...) +
        theme.multiplot
      ceac <- ceac.plot(x,pos=pos,comparison=comparison,graph="ggplot2") +
        theme.multiplot
      evi <- evi.plot(x,graph="ggplot2") +
        theme.multiplot
      # then call multiplot
      multiplot(ceplane,ceac,eib,evi,cols=2)
    } # !base.graphics
  }
}
