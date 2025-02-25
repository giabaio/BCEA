
#' Diagnostic Plots For The Results Of The EVPPI
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
#' The second possible diagnostic is the Q-Q plot for the fitted value. This is a
#' graphical method for comparing the fitted values distributions with the
#' assumed underlying normal distribution by plotting their quantiles against
#' each other. First, the set of intervals for the quantiles is chosen. A point
#' (x,y) on the plot corresponds to one of the quantiles of the second
#' distribution (y-coordinate) plotted against the same quantile of the first
#' distribution (x-coordinate). If the two distributions being compared are
#' identical, the Q-Q plot follows the 45 degrees line. 
#' 
#' @param evppi A `evppi` object obtained by running the function `evppi`
#' on a `bcea` model.
#' @template args-he
#' @param plot_type The type of diagnostics to be performed. It can be the 'residual
#' plot' (`residuals`) or the Q-Q (quantile-quantile) plot (`qqplot`).
#' @param interv Specifies the interventions for which diagnostic tests should be
#' performed (if there are many options being compared)
#' @return Plot
#' 
#' @author Gianluca Baio, Anna Heath
#' @seealso [bcea()], [evppi()]
#' @importFrom Rdpack reprompt
#' @import voi
#' 
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords internal hplot 
#' @export
#'  
diag.evppi <- function(evppi,
                       he,
                       plot_type = c("residuals", "qqplot"),
                       interv = 1) {
  
  if (interv > 1 && dim(evppi$fitted.costs)[2] == 1) {
    stop("There is only one comparison possible, so 'interv' set to 1 (default)",
         call. = FALSE)}
  
  plot_type <- match.arg(plot_type)
  is_residual <- pmatch(plot_type, c("residuals", "qqplot")) != 2
  
  if (is_residual) {
    evppi_residual_plot(evppi, he, interv)
  } else {
    evppi_qq_plot(evppi, he, interv)
  }
}


#' Residual Plot
#' @keywords internal hplot 
#' 
evppi_residual_plot <- function(evppi,
                                he,
                                interv) {
  
  fitted <- list(cost = evppi$fitted.costs[, interv],
                 eff = evppi$fitted.effects[, interv])
  
  residual <-
    list(cost = as.matrix(he$delta_c)[evppi$select, interv] - fitted$cost,
         eff = as.matrix(he$delta_e)[evppi$select, interv] - fitted$eff)
  
  cex <- 0.8
  
  op <- par(mfrow = c(1, 2))
  
  plot(fitted$cost,
       residual$cost,
       xlab = "Fitted values",
       ylab = "Residuals",
       main = "Residual plot for costs",
       cex = cex)
  abline(h = 0)
  
  plot(fitted$eff,
       residual$eff,
       xlab = "Fitted values",
       ylab = "Residuals",
       main = "Residual plot for effects",
       cex = cex)
  abline(h = 0)
  
  par(op)
}


#' Q-Q Plot
#' @keywords internal hplot 
#' 
#' @importFrom graphics par
#' @importFrom stats qqnorm qqline
#' 
evppi_qq_plot <- function(evppi,
                          he,
                          interv) {
  
  op <- par(mfrow = c(1, 2))
  
  fit_cost <- evppi$fitted.costs[, interv]
  fit_eff <- evppi$fitted.effects[, interv] 
  
  qqnorm(fit_cost, main = "Normal Q-Q plot \n(costs)")
  qqline(fit_cost)
  
  qqnorm(fit_eff, main = "Normal Q-Q plot \n(effects)")
  qqline(fit_eff)
  
  par(op)
}

