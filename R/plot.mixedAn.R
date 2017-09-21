###plot.mixedAn###############################################################################################


#' Summary plot of the health economic analysis when the mixed analysis is
#' considered
#' 
#' Compares the optimal scenario to the mixed case in terms of the EVPI
#' 
#' 
#' @param x An object of class \code{mixedAn}, given as output of the call to
#' the function \code{\link{mixedAn}}.
#' @param y.limits Range of the y-axis for the graph. The default value is
#' \code{NULL}, in which case the maximum range between the optimal and the
#' mixed analysis scenarios is considered.
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
#' @return \item{evi}{ A ggplot object containing the plot. Returned only if
#' \code{graph="ggplot2"}. } The function produces a graph showing the
#' difference between the ''optimal'' version of the EVPI (when only the most
#' cost-effective intervention is included in the market) and the mixed
#' strategy one (when more than one intervention is considered in the market).
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{mixedAn}}
#' @references Baio, G. and Russo, P. (2009).A decision-theoretic framework for
#' the application of cost-effectiveness analysis in regulatory processes.
#' Pharmacoeconomics 27(8), 645-655 doi:10.2165/11310250
#' 
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health
#' Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Mixed analysis
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
#' ma <- mixedAn(m,        # uses the results of the mixed strategy 
#'                         #  analysis (a "mixedAn" object)
#'        mkt.shares=NULL  # the vector of market shares can be defined 
#'                         #  externally. If NULL, then each of the T 
#'                         #  interventions will have 1/T market share
#' )
#' #
#' # Can also plot the summary graph
#' plot(ma,graph="base")
#' #
#' # Or with ggplot2
#' if(require(ggplot2)){
#' plot(ma,graph="ggplot2")
#' }
#' 
#' @export plot.mixedAn
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
