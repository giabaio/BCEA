#######################ceaf.plot##################################
ceaf.plot <- function(mce,graph=c("base","ggplot2")){
  base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  if(base.graphics) {
    plot(mce$k,mce$ceaf,t="l",lty=1,
         ylim=c(0,1),xlab="Willingness to pay",
         ylab="Probability of most cost effectiveness",
         main="Cost-effectiveness acceptability frontier")
  }
  else{
    if(!isTRUE(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("grid",quietly=TRUE))){
      message("Falling back to base graphics\n")
      ceaf.plot(mce,graph="base")
      return(invisible(NULL))
    }
    
    # no visible binding note
    k  <- NA_real_
    
    df <- data.frame("k"=mce$k,"ceaf"=mce$ceaf)
    ceaf <- ggplot2::ggplot(df,ggplot2::aes(x=k,y=ceaf)) + ggplot2::theme_bw() +
      ggplot2::geom_line() + ggplot2::coord_cartesian(ylim=c(-0.05,1.05)) +
      ggplot2::theme(text=ggplot2::element_text(size=11),legend.key.size=grid::unit(.66,"lines"),legend.spacing=grid::unit(-1.25,"line"),
                     panel.grid=ggplot2::element_blank(),legend.key=ggplot2::element_blank()) +
      ggplot2::labs(title="Cost-effectiveness acceptability frontier",x="Willingness to pay",y="Probability of most cost-effectiveness") +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight=1.05, face="bold",size=14.3,hjust=0.5))
    return(ceaf)
  }
}