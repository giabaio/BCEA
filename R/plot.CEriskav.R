###plot.CEriskav##################################################################################################
# Plots the EIB for the risk aversion case


#' Summary plot of the health economic analysis when risk aversion is included
#' 
#' Plots the EIB and the EVPI when risk aversion is included in the utility
#' function
#' 
#' 
#' @param x An object of the class \code{CEriskav}, containing the results of
#' the economic analysis performed accounting for a risk aversion parameter
#' (obtained as output of the function \code{\link{CEriskav}}).
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#' which specifies the relative position on the x and y axis respectively, or
#' alternatively it can be in form of a logical variable, with \code{FALSE}
#' indicating to use the default position and \code{TRUE} to place it on the
#' bottom of the plot. Default value is \code{c(0,1)}, that is in the topleft
#' corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  Arguments to be passed to methods, such as graphical parameters
#' (see \code{\link{par}}).
#' @return \item{list(eib,evi)}{A two-elements named list of the ggplot objects
#' containing the requested plots. Returned only if \code{graph="ggplot2"}.}
#' The function produces two plots for the risk aversion analysis. The first
#' one is the EIB as a function of the discrete grid approximation of the
#' willingness parameter for each of the possible values of the risk aversion
#' parameter, r. The second one is a similar plot for the EVPI.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{CEriskav}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Risk aversion
#' @examples
#' 
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#' #
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#' # 
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(e=e,c=c,          # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e,c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0,Kmax)
#'       plot=FALSE            # inhibits graphical output
#' )
#' #
#' # Define the vector of values for the risk aversion parameter, r, eg:
#' r <- c(0.000000000001,0.005,0.020,0.035) 
#' #
#' # Run the cost-effectiveness analysis accounting for risk aversion
#' \donttest{
#' cr <- CEriskav(m,     # uses the results of the economic evalaution 
#'                       #  (a "bcea" object)
#'         r=r,          # defines the vector of values for the risk 
#'                       #  aversion parameter
#'         comparison=1  # if more than 2 interventions, selects the 
#'                       #  pairwise comparison 
#' )
#' }
#' #
#' # Now produce the plots
#' \donttest{
#' plot(cr # uses the results of the risk aversion 
#'         #  analysis (a "CEriskav" object)
#' )
#' }
#' ### Alternative options, using ggplot2
#' \donttest{
#' plot(cr,
#'   graph="ggplot2",
#'   plot="ask"          # plot option can be specified as
#'                       #  "dev.new" (default), "x11" or "ask"
#'   )
#' }
#' 
#' @export plot.CEriskav
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
