
#' EVI Plot of the Health Economic Analysis For Mixed Analysis
#' 
#' Compares the optimal scenario to the mixed case in terms of the EVPI.
#'  
#' @param he An object of class `mixedAn`, a subclass of `bcea`,
#' given as output of the call to the function [mixedAn()].
#' @param y.limits Range of the y-axis for the graph. The default value is
#' `NULL`, in which case the maximum range between the optimal and the
#' mixed analysis scenarios is considered.
#' @template args-pos
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options `"base"` or
#' `"ggplot2"`. Default value is `"base"`.
#' @param ...  Arguments to be passed to methods, such as graphical parameters
#' (see [par()]).
#' 
#' @return \item{evi}{ A ggplot object containing the plot. Returned only if
#' `graph="ggplot2"`. } The function produces a graph showing the
#' difference between the ''optimal'' version of the EVPI (when only the most
#' cost-effective intervention is included in the market) and the mixed
#' strategy one (when more than one intervention is considered in the market).
#' 
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()],
#'          [mixedAn()]
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom Rdpack reprompt
#'          
#' @references
#' 
#' \insertRef{Baio2009}{BCEA}
#' 
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @examples
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#' #
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#'
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(e=eff, c=cost,    # defines the variables of 
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
#'
#' mixedAn(m) <- NULL      # uses the results of the mixed strategy 
#'                         #  analysis (a "mixedAn" object)
#'                         # the vector of market shares can be defined 
#'                         #  externally. If NULL, then each of the T 
#'                         #  interventions will have 1/T market share
#'                         # produces the plots
#' evi.plot(m)
#' 
#' evi.plot(m, graph="base")
#'
#' # Or with ggplot2
#' if (require(ggplot2)) {
#'    evi.plot(m, graph="ggplot2")
#' }
#' 
#' @export
#' 
evi.plot.mixedAn <- function(he,
                             y.limits = NULL,
                             pos = c(0, 1),
                             graph = c("base", "ggplot2"),
                             ...) {
  alt_legend <- pos
  base.graphics <- all(pmatch(graph, c("base", "ggplot2")) != 2)
  
  y.limits <- y.limits %||% range(he$evi, he$evi.star)
  
  if (base.graphics) {
    
    if (is.numeric(alt_legend) &&
        length(alt_legend) == 2) {
      temp <- ""
      if (alt_legend[2] == 0)
        temp <- paste0(temp,"bottom")
      else
        temp <- paste0(temp,"top")
      if (alt_legend[1] == 1)
        temp <- paste0(temp, "right")
      else
        temp <- paste0(temp, "left")
      alt_legend <- temp
      if (length(grep("^(bottom|top)(left|right)$", temp)) == 0)
        alt_legend <- FALSE
    }
    if (is.logical(alt_legend)) {
      alt_legend <- 
        if (!alt_legend)
          "topright"
      else
        "topleft"
    }
    
    plot(he$k,
         he$evi,
         type = "l",
         xlab = "Willingness to pay",
         ylab = "EVPI",
         main = "Expected Value of Information",
         ylim = y.limits)
    polygon(c(he$k, rev(he$k)),
            c(he$evi.star, rev(he$evi)),
            density = 20,
            col = "grey")
    points(he$k, he$evi.star, type = "l", col = "red")
    points(he$k, he$evi, type = "l", col = "black")
    txt <- c(
      "Optimal strategy",
      "Mixed strategy:",
      paste(
        "   ",
        he$interventions,
        "=",
        format(100 * he$mkt.shares, digits = 3, nsmall = 2),
        "%",
        sep = ""))
    cols <- c("black", "red", rep("white", length(he$interventions)))
    legend(
      alt_legend,
      txt,
      col = cols,
      cex = 0.6,
      bty = "n",
      lty = 1)
  } else {
    # base.graphics
    if (!isTRUE(requireNamespace("ggplot2", quietly = TRUE) &&
                requireNamespace("grid", quietly = TRUE))) {
      message("Falling back to base graphics\n")
      
      evi.plot.mixedAn(he,
                       y.limits = y.limits,
                       pos = pos,
                       graph = "base")
      
      return(invisible(NULL))
    } 
    
    if (isTRUE(requireNamespace("ggplot2", quietly = TRUE) &&
               requireNamespace("grid", quietly = TRUE))) {
      
      # legend
      txt <-
        c("Optimal strategy",
          paste0("Mixed strategy:",
                 paste0("\n   ",he$interventions,"=",
                        format(100*he$mkt.shares, digits = 3, nsmall = 2), "%", collapse = "")))
      colors <- c("black","red")
      
      df <- data.frame("k" = he$k,
                       "evi" = he$evi,
                       "evi.star" = he$evi.star)
      
      evi <- ggplot(df, aes(x = .data$k)) +
        theme_bw() +
        geom_ribbon(aes(x = .data$k, ymin = .data$evi, ymax = .data$evi.star),
                    color = "lightgrey",
                    alpha = 0.2) +
        geom_line(aes(x = .data$k, y = .data$evi,
                      color = as.factor(1))) +
        geom_line(aes(x = .data$k, y = .data$evi.star,
                      color = as.factor(2))) +
        coord_cartesian(ylim = y.limits, xlim = c(0, max(df$k))) +
        scale_color_manual("", labels = txt, values = colors) +
        labs(title = "Expected Value of Information",
             x = "Willingness to pay",
             y = "EVPI") +
        theme(
          text = element_text(size = 11),
          legend.key.size = unit(0.66, "lines"),
          legend.spacing = unit(-1.25, "line"),
          panel.grid = element_blank(),
          legend.key = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5))
      
      jus <- NULL
      
      if (isTRUE(alt_legend)) {
        alt_legend <- "bottom"
        evi <- evi + theme(legend.direction = "vertical")
      } else {
        if (is.character(alt_legend)) {
          choices <- c("left", "right", "bottom", "top")
          alt_legend <- choices[pmatch(alt_legend,choices)]
          jus <- "center"
          if (is.na(alt_legend))
            alt_legend <- FALSE
        }
        if (length(alt_legend) > 1)
          jus <- alt_legend
        if (length(alt_legend) == 1 &&
            !is.character(alt_legend)) {
          alt_legend <- c(0,1)
          jus <- alt_legend
        }
      }
      
      evi <- evi + 
        theme(
          legend.position = alt_legend,
          legend.justification = jus,
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(hjust = 0),
          plot.title = element_text(
            lineheight = 1.05,
            face = "bold",
            size = 14.3,
            hjust = 0.5))
      
      return(evi)
    }
  }
}

