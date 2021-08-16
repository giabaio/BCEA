
#' Plots EIB for the Risk Aversion Case
#' 
#' Summary plot of the health economic analysis when risk aversion is included.
#' 
#' Plots the EIB and the EVPI when risk aversion is included in the utility
#' function.
#' 
#' @param he An object of the class \code{CEriskav}, a subclass of \code{bcea},
#' containing the results of the economic analysis performed accounting for a
#' risk aversion parameter (obtained as output of the function \code{\link{CEriskav}}).
#' @template args-pos
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  Arguments to be passed to methods, such as graphical parameters
#' (see \code{\link{par}}).
#' 
#' @return \item{list(eib,evi)}{A two-elements named list of the ggplot objects
#' containing the requested plots. Returned only if \code{graph="ggplot2"}.}
#' The function produces two plots for the risk aversion analysis. The first
#' one is the EIB as a function of the discrete grid approximation of the
#' willingness parameter for each of the possible values of the risk aversion
#' parameter, \code{r}. The second one is a similar plot for the EVPI.
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{CEriskav}}
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
#' @keywords Health economic evaluation Risk aversion
#' 
#' @importFrom grDevices dev.new devAskNewPage
#' @importFrom grid unit
#' @import ggplot2
#' 
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
#' cr <- CEriskav(m,     # uses the results of the economic evaluation 
#'                       #  (a "bcea" object)
#'         r=r,          # defines the vector of values for the risk 
#'                       #  aversion parameter
#'         comparison=1  # if more than 2 interventions, selects the 
#'                       #  pairwise comparison 
#'       )
#' }
#' #
#' # produce the plots
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
#' @export
#' 
plot.CEriskav <- function(he,
                          pos = c(0, 1),
                          graph = c("base", "ggplot2"),
                          ...) {
  
  alt.legend <- pos
  base.graphics <-
    ifelse(isTRUE(pmatch(graph, c("base","ggplot2")) == 2), FALSE, TRUE) 
  
  howplot <- NULL
  exArgs <- list(...)
  if (length(exArgs) >= 1)
    if (exists("plot", where = exArgs))
      howplot <- exArgs$plot
  
  if (base.graphics) {
    
    if (is.numeric(alt.legend) && length(alt.legend) == 2) {
      legend_txt <- ""
      if (alt.legend[2] == 0)
        legend_txt <- paste0(legend_txt, "bottom")
      else
        legend_txt <- paste0(legend_txt, "top")
      if (alt.legend[1] == 1)
        legend_txt <- paste0(legend_txt, "right")
      else
        legend_txt <- paste0(legend_txt, "left")
      alt.legend <- legend_txt
      if (length(grep("^(bottom|top)(left|right)$", legend_txt)) == 0)
        alt.legend <- FALSE
    }
    if (is.logical(alt.legend)) {
      alt.legend <- 
        if (!alt.legend) "topright"
      else "topleft"
    }
    
    plot(x = he$k, y = he$eibr[, 1],
         type = "l",
         xlab = "Willingness to pay",
         ylab = " ",
         main = "EIB as a function of the risk aversion parameter",
         ylim = range(he$eibr))
    linetype <- seq(1,he$R)
    
    for (l in 2:he$R) {
      points(he$k, he$eibr[, l], type = "l", lty = linetype[l])
    }
    text <- paste("r = ", he$r, sep = "") 
    # If the first value for r is small enough, consider it close to 0 and print the label accordingly
    if (he$r[1] < 1e-8) {
      text[1] <- expression(r%->%0)
    }
    legend(alt.legend, text, lty = seq(1:he$R), cex = 0.9, box.lty = 0)
    abline(h = 0, col = "grey")
    
    # Plots the EVPI for the risk aversion case
    if (is.null(howplot)) {
      if (!isTRUE(Sys.getenv("RSTUDIO") == 1))
        dev.new()
    }
    else {
      opt <- c("x11", "ask", "dev.new")
      howplot <- ifelse(is.na(pmatch(howplot, opt)),
                        "dev.new",
                        opt[pmatch(howplot, opt)])
      if (howplot == "x11")
        dev.new()
      if (howplot == "dev.new")
        dev.new()
      if (howplot == "ask")
        devAskNewPage(ask = TRUE)
    }
    
    plot(he$k,
         he$evir[, 1],
         type = "l",
         ylim = range(he$evir),
         xlab = "Willingness to pay",
         ylab = " ",
         main = "EVPI as a function of the risk aversion parameter")
    for (l in 2:he$R) {
      points(he$k, he$evir[, l], type = "l", lty = linetype[l])
    }
    legend(alt.legend, text, lty = seq(1:he$R), cex = 0.9, box.lty = 0)
    abline(h = 0, col = "grey")
    
    if (!is.null(howplot))
      if (howplot == "ask")
        devAskNewPage(ask = FALSE)
    
  } else {
    # base.graphics
    if (!isTRUE(requireNamespace("ggplot2", quietly = TRUE) &&
                requireNamespace("grid", quietly = TRUE))) {
      message("falling back to base graphics\n")
      plot.CEriskav(x, graph = "base", pos = pos,...)
      return(invisible(NULL))
    }
    # no visible bindings note
    k <- r <- NA_real_
    
    linetypes <- rep(c(1,2,3,4,5,6), ceiling(he$R/6))[1:he$R]
    df <- data.frame(cbind(rep(he$k,he$R), c(he$eibr), c(he$evir)),
                     as.factor(sort(rep(1:he$R, length(he$k)))))
    names(df) <- c("k","eibr","evir","r")
    
    # labels
    text <- paste0("r = ", he$r)
    # if the first value for r is small enough, consider it close to 0 and print the label accordingly
    if (he$r[1] < 1e-8) {
      text[1] <- expression(r%->%0)
    }
    
    eibr <-
      ggplot(df, aes(x = k, y = eibr, linetype = r)) +
      geom_hline(yintercept = 0, linetype = 1, colour = "grey50") +
      geom_line()+
      scale_linetype_manual("", labels = text, values = linetypes) + 
      theme_bw() +
      labs(title = "EIB as a function of the risk aversion parameter",
           x = "Willingness to pay",
           y = "EIB") +
      theme(
        text = element_text(size = 11),
        legend.key.size = unit(0.66, "line"),
        legend.spacing = unit(-1.25, "line"),
        panel.grid = element_blank(),
        legend.key = element_blank())
    
    ### evir ###
    evir <-
      ggplot(df, aes(x = k, y = evir, linetype = r)) + 
      geom_hline(yintercept = 0, linetype = 1, colour = "grey50")+
      geom_line() + 
      scale_linetype_manual("", labels = text, values = linetypes) + 
      theme_bw() +
      labs(title = "EVPI as a function of the risk aversion parameter",
           x = "Willingness to pay",
           y = "EVPI") +
      theme(
        text = element_text(size = 11),
        legend.key.size = unit(0.66, "line"),
        legend.spacing = unit(-1.25, "line"),
        panel.grid = element_blank(),
        legend.key = element_blank())
    jus <- NULL
    if (isTRUE(alt.legend)) {
      alt.legend <- "bottom"
      eibr <- eibr + theme(legend.direction = "vertical")
      evir <- evir + theme(legend.direction = "vertical")
    } else {
      if (is.character(alt.legend)) {
        choices <- c("left", "right", "bottom", "top")
        alt.legend <- choices[pmatch(alt.legend,choices)]
        jus <- "center"
        if (is.na(alt.legend))
          alt.legend <- FALSE
      }
      if (length(alt.legend) > 1)
        jus <- alt.legend
      if (length(alt.legend) == 1 && !is.character(alt.legend)) {
        alt.legend <- c(0,1)
        jus <- alt.legend
      }
    }
    
    eibr <-
      eibr + 
      theme(
        legend.position = alt.legend,
        legend.justification = jus,
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text.align = 0,
        plot.title = element_text(
          lineheight = 1.05,
          face = "bold",
          size = 14.3,
          hjust = 0.5))
    
    evir <- evir + 
      ggplot2::theme(
        legend.position = alt.legend,
        legend.justification = jus,
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text.align = 0,
        plot.title = element_text(
          lineheight = 1.05,
          face = "bold",
          size = 14.3,
          hjust = 0.5))
    plot(eibr)
    
    if (is.null(howplot)) {
      if (!isTRUE(Sys.getenv("RSTUDIO") == 1))
        dev.new()
    } else {
      opt <- c("x11", "ask", "dev.new")
      howplot <- ifelse(is.na(pmatch(howplot, opt)),
                        "dev.new",
                        opt[pmatch(howplot, opt)])
      if (howplot == "x11")
        dev.new()
      if (howplot == "dev.new")
        dev.new()
      if (howplot == "ask")
        devAskNewPage(ask = TRUE)
    }
    
    plot(evir)
    
    if (!is.null(howplot))
      if (howplot == "ask")
        devAskNewPage(ask = FALSE)
    
    return(invisible(list("eib" = eibr, "evi" = evir)))
  }
}

