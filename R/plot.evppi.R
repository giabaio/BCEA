######plot.evppi################################################################################################


#' plot.evppi
#' 
#' Plots a graph of the Expected Value of Partial Information with respect to a
#' set of parameters
#' 
#' 
#' @param x An object in the class \code{evppi}, obtained by the call to the
#' function \code{\link{evppi}}.
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
#' @param col Sets the color for the lines depicted in the graph.
#' @param ...  Arguments to be passed to methods, such as graphical parameters
#' (see \code{\link{par}}).
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{evppi}}
#' @references Baio G. (2012). Bayesian Methods in Health Economics.
#' CRC/Chapman Hall, London
#' @keywords Health economic evaluation Expected value of information
#' @export plot.evppi
plot.evppi<-function (x, pos = c(0, 0.8), graph = c("base", "ggplot2"), col = NULL, 
                      ...) 
{
  options(scipen = 10)
  alt.legend <- pos
  base.graphics <- ifelse(isTRUE(pmatch(graph, c("base", "ggplot2")) == 
                                   2), FALSE, TRUE)
  stopifnot(isTRUE(class(x) == "evppi"))
  if (base.graphics) {
    if (is.numeric(alt.legend) & length(alt.legend) == 2) {
      temp <- ""
      if (alt.legend[2] == 0) 
        temp <- paste0(temp, "bottom")
      else if (alt.legend[2] != 0.5) 
        temp <- paste0(temp, "top")
      if (alt.legend[1] == 1) 
        temp <- paste0(temp, "right")
      else temp <- paste0(temp, "left")
      alt.legend <- temp
      if (length(grep("^((bottom|top)(left|right)|right)$", 
                      temp)) == 0) 
        alt.legend <- FALSE
    }
    if (is.logical(alt.legend)) {
      if (!alt.legend) 
        alt.legend = "topright"
      else alt.legend = "topleft"
    }
    plot(x$k, x$evi, t = "l", xlab = "Willingness to pay", 
         ylab = "", main = "Expected Value of Perfect Partial Information", 
         lwd = 2, ylim = range(range(x$evi), range(x$evppi)))
    if (is.null(col)) {
      cols <- colors()
      gr <- floor(seq(from = 261, to = 336, length.out = length(x$index)))
      col <- cols[gr]
    }
    else {
      if (length(col) != length(x$parameters)) {
        message("The vector 'col' must have the same number of elements as the number of parameters. Forced to black\n")
        col <- rep("black", length(x$parameters))
      }
    }
    if (length(x$index) == 1 | length(x$index) > 1 & (class(x$method)=="list")) {
      col = "black"
      points(x$k, x$evppi, t = "l", col = col, lty = 1)
    }
    cmd <- "EVPPI for the selected\nsubset of parameters"
    if (nchar(x$parameters[1]) <= 25) {
      cmd <- paste("EVPPI for ", x$parameters, sep = "")
    }
    if (length(x$index) > 1 & (x$method == "Strong & Oakley (univariate)" || 
                               x$method == "Sadatsafavi et al")) {
      for (i in 1:length(x$index)) {
        points(x$k, x$evppi[[i]], t = "l", col = col[i], 
               lty = i)
        text(par("usr")[2], x$evppi[[i]][length(x$k)], 
             paste("(", i, ")", sep = ""), cex = 0.7, pos = 2)
      }
      cmd <- paste("(", paste(1:length(x$index)), ") EVPPI for ", 
                   x$parameters, sep = "")
    }
    legend(alt.legend, c("EVPI", cmd), col = c("black", col), 
           cex = 0.7, bty = "n", lty = c(1, 1:length(x$parameters)), 
           lwd = c(2, rep(1, length(x$parameters))))
    return(invisible(NULL))
  }
  else {
    if (!isTRUE(requireNamespace("ggplot2", quietly = TRUE) & 
                requireNamespace("grid", quietly = TRUE))) {
      message("Falling back to base graphics\n")
      plot.evppi(x, pos = c(0, 0.8), graph = "base", col)
      return(invisible(NULL))
    }
    else {
      message("ggplot2 method not yet implemented for this function: falling back to base graphics\n")
      plot.evppi(x, pos = c(0, 0.8), graph = "base", col)
      return(invisible(NULL))
    }
  }
}

