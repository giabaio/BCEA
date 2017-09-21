#######################ceaf.plot##################################


#' Cost-Effectiveness Acceptability Frontier (CEAF) plot
#' 
#' Produces a plot the Cost-Effectiveness Acceptability Frontier (CEAF) against
#' the willingness to pay threshold
#' 
#' 
#' @param mce The output of the call to the function \code{\link{multi.ce}}
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @return \item{ceaf}{ A ggplot object containing the plot. Returned only if
#' \code{graph="ggplot2"}. }
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{multi.ce}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Multiple comparison
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
#' \donttest{
#' mce <- multi.ce(m)          # uses the results of the economic analysis 
#' }
#' #
#' \donttest{
#' ceaf.plot(mce)              # plots the CEAF 
#' }
#' #
#' \donttest{
#' ceaf.plot(mce, graph="g")   # uses ggplot2 
#' }
#' 
#' \donttest{
#' # Use the smoking cessation dataset
#' data(Smoking)
#' m <- bcea(e,c,ref=4,intervention=treats,Kmax=500,plot=FALSE)
#' mce <- multi.ce(m)
#' ceaf.plot(mce)
#' }
#' 
#' @export ceaf.plot
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
