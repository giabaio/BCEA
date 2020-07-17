
#' Diagnostic plots for the results of the EVPPI
#' 
#' The function produces either a residual plot comparing the fitted
#' values from the INLA-SPDE Gaussian Process regression to the residuals.
#' This is a scatter plot of residuals on the y axis and fitted values (estimated
#' responses) on the x axis. The plot is used to detect non-linearity, unequal
#' error variances, and outliers. A well-behaved residual plot supporting the
#' appropriateness of the simple linear regression model has the following
#' characteristics:
#' 1) The residuals bounce randomly around the 0 line. This suggests that
#' the assumption that the relationship is linear is reasonable.
#' 2) The residuals roughly form a horizontal band around the 0 line. This
#' suggests that the variances of the error terms are equal.
#' 3) None of the residual stands out from the basic random pattern of residuals.
#' This suggests that there are no outliers.
#' 
#' The second possible diagnostic is the qqplot for the fitted value. This is a
#' graphical method for comparing the fitted values distributions with the
#' assumed underlying normal distribution by plotting their quantiles against
#' each other. First, the set of intervals for the quantiles is chosen. A point
#' (x,y) on the plot corresponds to one of the quantiles of the second
#' distribution (y-coordinate) plotted against the same quantile of the first
#' distribution (x-coordinate). If the two distributions being compared are
#' identical, the Q-Q plot follows the 45 degrees line. 
#' 
#' @param evppi A \code{evppi} object obtained by running the function \code{evppi}
#' on a \code{bcea} model.
#' @param he A \code{bcea} object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @param plot_type The type of diagnostics to be performed. It can be the 'residual
#' plot' or the 'qqplot plot'.
#' @param interv Specifies the interventions for which diagnostic tests should be
#' performed (if there are many options being compared)
#' @return plot
#' 
#' @author Gianluca Baio, Anna Heath
#' @seealso \code{\link{bcea}}, \code{\link{evppi}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation, Value of Information
#' 
#' @export
#' 
#' @examples
#'  
diag.evppi <- function(evppi,
                       he,
                       plot_type = c("residuals", "qqplot"),
                       interv = 1) {
  
  if (int > 1 & dim(evppi$fitted.costs)[2] == 1) {
    stop("There is only one comparison possible, so 'interv' should be set to 1 (default)", call. = FALSE)}
  
  plot_type <- match.arg(plot_type)
  is_residual <- pmatch(plot_type, c("residuals", "qqplot")) !=2
  
  if (is_residual) {
    evppi_residual_plot(evppi, he, interv)
  } else {
    evppi_qq_plot(evppi, he, interv)
  }
}

#
evppi_residual_plot <- function(evppi,
                                he,
                                interv) {
  
  op <- par(mfrow = c(1, 2))
  n <- dim(evppi$fitted.costs)[1]
  fitted <- evppi$fitted.costs[, interv]
  residual <- as.matrix(he$delta.c)[evppi$select, interv] - fitted
  
  plot(fitted,
       residual,
       xlab = "Fitted values",
       ylab = "Residuals",
       main = "Residual plot for costs",
       cex = 0.8)
  abline(h = 0)
  fitted <- evppi$fitted.effects[, interv]
  residual <- as.matrix(he$delta.e)[evppi$select, interv] - fitted
  
  plot(fitted,
       residual,
       xlab = "Fitted values",
       ylab = "Residuals",
       main = "Residual plot for effects",
       cex = 0.8)
  abline(h = 0)
  par(op)
}

#
evppi_qq_plot <- function(evppi,
                          he,
                          interv) {
  
  op <- par(mfrow = c(1, 2))
  qqnorm(evppi$fitted.costs[, interv],
         main = "Normal Q-Q plot \n(costs)")
  qqline(evppi$fitted.costs[, interv])
  qqnorm(evppi$fitted.effects[, interv],
         main = "Normal Q-Q plot \n(effects)")
  qqline(evppi$fitted.effects[, interv])
  par(op)
}
