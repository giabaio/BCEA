
#' @rdname contour2
#' @importFrom stats sd
#' @importFrom graphics par contour
#' 
#' @export
#' 
contour2.bcea <- function(he,
                          wtp = 25000,
                          xlim = NULL,
                          ylim = NULL,
                          comparison = NULL,
                          graph_type = c("base", "ggplot2"),
                          ...) {
  
  graph_type <- match.arg(graph_type)
  
  he <- setComparisons(he, comparison)
  
  if (is_baseplot(graph_type)) {
    # Encodes characters so that the graph can
    # be saved as postscript or pdf
    ps.options(encoding = "CP1250")
    pdf.options(encoding = "CP1250")
    
    # Selects the first comparison by default if not selected
    if (is.null(comparison) || length(comparison) > 1 ) {
      message("The first available comparison will be selected.
              To plot multiple comparisons together please use the ggplot2 version.
              Please see ?contour2 for additional details.")
      comparison <- min(he$comp)
    }
    
    he <- setComparisons(he, comparison)
    ceplane.plot(he, comparison = NULL, wtp, graph_type = "base")
    
    # plot contours ----
    
    ##TODO: where is this used?
    # offset <- 1.0
    
    nlevels <- 4
    scale <- 0.5
    
    density <- MASS::kde2d(as.matrix(he$delta_e),
                           as.matrix(he$delta_c),
                           n = 300,
                           h = c(sd(as.matrix(he$delta_e)) / scale,
                                 sd(as.matrix(he$delta_c)) / scale))
    
    ##TODO: should we use these?
    # m.c <- range(he$delta_c)[1]
    # M.c <- range(he$delta_c)[2]
    # m.e <- range(he$delta_e)[1]
    # M.e <- range(he$delta_e)[2]
    
    # Changes the range so that the plot always shows the x and y axes
    # ch1 <- ifelse(m.e > 0, m.e <- -m.e, m.e <- m.e)
    # ch2 <- ifelse(M.e < 0, M.e <- -M.e, M.e <- M.e)
    # ch3 <- ifelse(m.c > 0, m.c <- -m.c, m.c <- m.c)
    # ch4 <- ifelse(M.c < 0, M.c <- -M.c, M.c <- M.c)
    
    par(new = TRUE)
    graphics::contour(
      density$x,
      density$y,
      density$z,
      add = TRUE,
      nlevels = nlevels,
      drawlabels = FALSE,
      lwd = 1.5)
    
    return(invisible(NULL))
  } else {
    
    if (!(requireNamespace("ggplot2", quietly = TRUE) &
          requireNamespace("grid", quietly = TRUE))){
      
      message("falling back to base graphics\n")
      contour2(he,
               comparison = comparison,
               xlim = xlim,
               ylim = ylim,
               wtp = wtp,
               graph = "base")
      return(invisible(NULL))
    }
    
    scale <- 0.5
    nlevels <- 5
    
    densitydf <- data.frame()
    
    for (i in seq_along(he$comp)) {
      density <-
        MASS::kde2d(
          as.matrix(he$delta_e[, i]),
          as.matrix(he$delta_c[, i]),
          n = 300,
          h = c(sd(as.matrix(he$delta_e[, i])) / scale,
                sd(as.matrix(he$delta_c[, i])) / scale))
      
      grid_density <-
        data.frame(
          expand.grid(
            x = density$x,
            y = density$y),
          z = as.vector(density$z),
          comparison = i)
      
      densitydf <-
        rbind(densitydf, grid_density)
    }
    
    densitydf$comparison <- as.factor(densitydf$comparison)
    
    contour <-
      ceplane.plot(he, wtp = wtp, graph = "ggplot2", ...) +
      geom_contour(
        data = densitydf, aes(x = .data$x, y = .data$y, z = .data$z, group = .data$comparison),
        colour = "black",
        bins = nlevels,
        linetype = 1, inherit.aes = FALSE)
    
    contour <-
      contour +
      coord_cartesian(xlim = xlim,
                      ylim = ylim)
    
    return(contour)
  }
}


#' Specialised CE-plane Contour Plot
#' 
#' Produces a scatterplot of the cost-effectiveness plane, with a contour-plot
#' of the bivariate density of the differentials of cost (y-axis) and
#' effectiveness (x-axis).  Also adds the sustainability area (i.e. below the
#' selected value of the willingness-to-pay threshold).
#' 
#' @template args-he
#' @param wtp The selected value of the willingness-to-pay. Default is
#' \code{25000}.
#' @param xlim Limits on the x-axis (default=\code{NULL}, so that R will select
#' appropriate limits).
#' @param ylim Limits on the y-axis (default=\code{NULL}, so that R will select
#' appropriate limits).
#' @param comparison The comparison being plotted. Default to \code{NULL}
#' chooses the first comparison if \code{graph_type="base"}. If
#' \code{graph_type="ggplot2"} the default value will choose all the possible
#' comparisons. Any subset of the possible comparisons can be selected (e.g.,
#' \code{comparison=c(1,3)}).
#' @param graph_type A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  Arguments to be passed to \code{\link{ceplane.plot}}. See the
#' relative manual page for more details.
#' 
#' @return \item{contour}{ A ggplot item containing the requested plot.
#' Returned only if \code{graph_type="ggplot2"}. } Plots the cost-effectiveness
#' plane with a scatterplot of all the simulated values from the (posterior)
#' bivariate distribution of (\eqn{\Delta_e, \Delta_c}), the differentials of
#' effectiveness and costs; superimposes a contour of the distribution and
#' prints the value of the ICER, together with the sustainability area.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}},
#'          \code{\link{ceplane.plot}},
#'          \code{\link{contour.bcea}}
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics. Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' @keywords "Health economic evaluation" "Bayesian model"
#' @import ggplot2
#' @importFrom grDevices ps.options pdf.options
#' @importFrom MASS kde2d
#' 
#' @examples
#' ## create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)
#' 
#' ## produce the plot
#' contour2(m,
#'          wtp = 200,
#'          graph_type = "base")
#' 
#' \donttest{
#' ## or use ggplot2 to plot multiple comparisons
#' contour2(m,
#'          wtp = 200,
#'          ICER_size = 2,
#'          graph_type = "ggplot2")
#' }
#' 
#' @rdname contour2
#' @export
#' 
contour2 <- function(he, ...) {
  UseMethod('contour2', he)
}

