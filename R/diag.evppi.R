######diag.evppi################################################################################################


#' diag.evppi
#' 
#' Performs diagnostic plots for the results of the EVPPI
#' 
#' 
#' @param x A \code{evppi} object obtained by running the function \code{evppi}
#' on a \code{bcea} model.
#' @param y A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param diag The type of diagnostics to be performed. It can be the 'residual
#' plot' or the 'qqplot plot'.
#' @param int Specifies the interventions for which diagnostic tests should be
#' performed (if there are many options being compared)
#' @return The function produces either a residual plot comparing the fitted
#' values from the INLA-SPDE Gaussian Process regression to the residuals. This
#' is a scatter plot of residuals on the y axis and fitted values (estimated
#' responses) on the x axis. The plot is used to detect non-linearity, unequal
#' error variances, and outliers. A well-behaved residual plot supporting the
#' appropriateness of the simple linear regression model has the following
#' characteristics: 1) The residuals bounce randomly around the 0 line. This
#' suggests that the assumption that the relationship is linear is reasonable.
#' 2) The residuals roughly form a horizontal band around the 0 line. This
#' suggests that the variances of the error terms are equal. 3) None of the
#' residual stands out from the basic random pattern of residuals. This
#' suggests that there are no outliers.
#' 
#' The second possible diagnostic is the qqplot for the fitted value. This is a
#' graphical method for comparing the fitted values distributions with the
#' assumed underlying normal distribution by plotting their quantiles against
#' each other. First, the set of intervals for the quantiles is chosen. A point
#' (x,y) on the plot corresponds to one of the quantiles of the second
#' distribution (y-coordinate) plotted against the same quantile of the first
#' distribution (x-coordinate). If the two distributions being compared are
#' identical, the Q-Q plot follows the 45 degrees line.
#' @author Gianluca Baio, Anna Heath
#' @seealso \code{\link{bcea}}, \code{\link{evppi}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation, Value of Information
#' @export diag.evppi
diag.evppi <- function(x,y,diag=c("residuals","qqplot"),int=1){
  # x = an evppi object
  # y = a bcea object
  # diag = the type of diagnostics required
  # int = the comparison to be assessed (default determined by the BCEA object)
  if (int>1 & dim(x$fitted.costs)[2]==1) {stop("There is only one comparison possible, so 'int' should be set to 1 (default)")}
  res <- ifelse(isTRUE(pmatch(diag,c("residuals","qqplot"))==2),FALSE,TRUE)
  if(res){
    op <- par(mfrow=c(1,2))
    n <- dim(x$fitted.costs)[1]
    fitted <- x$fitted.costs[,int]
    residual <- as.matrix(y$delta.c)[x$select,int]-fitted
    plot(fitted,residual,xlab="Fitted values",
         ylab="Residuals",main="Residual plot for costs",cex=.8);abline(h=0)
    fitted <- x$fitted.effects[,int]
    residual <- as.matrix(y$delta.e)[x$select,int]-fitted
    plot(fitted,residual,xlab="Fitted values",
         ylab="Residuals",main="Residual plot for effects",cex=.8);abline(h=0)
    par(op)
  }else{
    op <- par(mfrow=c(1,2))
    qqnorm(x$fitted.costs[,int],main="Normal Q-Q plot \n(costs)"); qqline(x$fitted.costs[,int])
    qqnorm(x$fitted.effects[,int],main="Normal Q-Q plot \n(effects)"); qqline(x$fitted.effects[,int])
    par(op)
  }
  
}
