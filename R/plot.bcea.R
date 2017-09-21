###plot.bcea##################################################################################################
## Plots the main health economics outcomes in just one graph


#' Summary plot of the health economic analysis
#' 
#' Plots in a single graph the Cost-Effectiveness plane, the Expected
#' Incremental Benefit, the CEAC and the EVPI
#' 
#' The default position of the legend for the cost-effectiveness plane
#' (produced by \code{\link{ceplane.plot}}) is set to \code{c(1,1.025)}
#' overriding its default for \code{pos=FALSE}, since multiple ggplot2 plots
#' are rendered in a slightly different way than single plots.
#' 
#' For more information see the documentation of each individual plot function.
#' 
#' @param x A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param comparison Selects the comparator, in case of more than two
#' interventions being analysed. The value is passed to
#' \code{\link{ceplane.plot}}, \code{\link{eib.plot}} and
#' \code{\link{ceac.plot}}.
#' @param wtp The value of the willingness to pay parameter. It is passed to
#' \code{\link{ceplane.plot}}.
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string, a single logical value, or a two-element vector with the
#' respective relative positions on the x and y axis. Default as \code{FALSE}
#' sets the legend position to the default one for each plot (see the details
#' section), while \code{TRUE} puts it on the bottom of each plot.  Changes
#' will affect all the individual plots.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  Arguments to be passed to the methods \code{\link{ceplane.plot}}
#' and \code{\link{eib.plot}}. Please see the manual pages for the individual
#' functions.  Arguments like \code{size}, \code{ICER.size} and \code{plot.cri}
#' can be supplied to the functions in this way. In addition if
#' \code{graph="ggplot2"} and the arguments are named theme objects they will
#' be added to each plot.
#' @return The function produces a plot with four graphical summaries of the
#' health economic evaluation.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{ceplane.plot}},
#' \code{\link{eib.plot}}, \code{\link{ceac.plot}}, \code{\link{evi.plot}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation
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
#'       plot=FALSE            # does not produce graphical outputs
#' )
#' #
#' # Plots the summary plots for the "bcea" object m using base graphics
#' plot(m,graph="base")
#' 
#' # Plots the same summary plots using ggplot2
#' if(require(ggplot2)){
#' plot(m,graph="ggplot2")
#' 
#' ##### Example of a customized plot.bcea with ggplot2
#' plot(m,
#'   graph="ggplot2",                                      # use ggplot2
#'   theme=theme(plot.title=element_text(size=rel(1.25))), # theme elements must have a name
#'   ICER.size=1.5,                                        # hidden option in ceplane.plot
#'   size=rel(2.5)                                         # modifies the size of k= labels
#' )                                                       #  in ceplane.plot and eib.plot
#' }
#' 
#' @export plot.bcea
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
