
#' @rdname inf_rank
#' @title Information-Rank Plot for bcea Class
#' 
#' @param parameter A vector of parameters for which the individual EVPPI
#' should be calculated. This can be given as a string (or vector of strings)
#' of names or a numeric vector, corresponding to the column numbers of
#' important parameters.
#' @param input A matrix containing the simulations for all the parameters
#' monitored by the call to JAGS or BUGS. The matrix should have column names
#' matching the names of the parameters and the values in the vector parameter
#' should match at least one of those values.
#' @param wtp A value of the wtp for which the analysis should be performed. If
#' not specified then the break-even point for the current model will be used.
#' @param howManyPars Optional maximum number of parameters to be included in the bar plot. 
#' Includes all parameters by default. 
#' @param graph A string used to select the graphical enging to use for plotting.
#' Should (partial-)match one of the two options "base" or "plotly". Default value is "base"
#' 
#' @author Anna Heath, Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}},
#'          \code{\link{evppi}}
#' 
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics. Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' @keywords dplot models
#' @importFrom rlang .data
#' @importFrom dplyr slice desc
#' @importFrom graphics barplot
#' 
#' @export
info.rank.bcea <- function(he,
                           parameter,
                           input,
                           wtp = he$k[min(which(he$k >= he$ICER))],
                           howManyPars = NULL,
                           graph = c("base", "plotly"),
                           ...) {
  
  base.graphics <- all(pmatch(graph, c("base", "plotly")) != 2)
  
  if (!requireNamespace("plotly", quietly = FALSE)) {
    base.graphics <- TRUE
    warning("Package plotly not found; falling back to base graphics.")
  }
  
  # function to create the bar plot
  make.barplot <- 
    if (base.graphics) {
      make.barplot_base
    } else {
      make.barplot_plotly
    }
  
  # Prevents BCEA::evppi from throwing messages
  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }
  
  extra_args <- list(...)
  if (is.null(wtp)) wtp = he$k[min(which(he$k >= he$ICER))]
  
  if (class(parameter[1]) == "character") {
    parameters <- array()
    for (i in seq_along(parameter)) {
      parameters[i] <- which(colnames(input) == parameter[i])
    }
  } else {
    parameters <- parameter
  }
  parameter <- colnames(input)[parameters]
  
  # needs to exclude parameters with weird behaviour (ie all 0s)
  w <- unlist(lapply(parameter, function(x) which(colnames(input) == x)))
  
  if (length(w) == 1) {
  } else {
    input <- input[, w]
    chk1 <- which(apply(input, 2, "var") > 0)   # only takes those with var > 0
    # check those with <5 possible values (would break GAM)
    tmp <- lapply(1:dim(input)[2], function(x) table(input[, x]))
    chk2 <- which(unlist(lapply(tmp, function(x) length(x) >= 5)) == TRUE)
    names(chk2) <- colnames(input[, chk2])
    
    # Can do the analysis on a smaller number of PSA runs
    if (exists("N", where = extra_args)) {
      N <- extra_args$N
    } else {N <- he$n_sim}
    
    if (any(!is.na(N)) & length(N) > 1) {
      select <- N
    } else {
      N <- min(he$n_sim,N, na.rm = TRUE)
      select <- 
        if (N == he$n_sim) {
          1:he$n_sim
        } else {
          sample(1:he$n_sim, size = N, replace = FALSE)} 
    }
    
    m <- he
    m$k <- wtp
    x <- list()
    
    for (i in seq_along(chk2)) {
      x[[i]] <- quiet(
        evppi(he = m, param_idx = chk2[i], input = input, N = N))
    }
    scores <- unlist(lapply(x,
                            function(x) x$evppi/x$evi[which(he$k == wtp)]))
    
    # Optional inputs
    if (base.graphics) {
      ca <- 
        if (exists("ca", where = extra_args)) {
          extra_args$ca
        } else {0.7}
      
      cn <- 
        if (exists("cn", where = extra_args)) {
          extra_args$cn
        } else {0.7}
      
      xlab <- "Proportion of total EVPI"
      if (exists("rel", where = extra_args)) {
        if (!extra_args$rel) {
          scores <- unlist(lapply(x, function(x) x$evppi))
          xlab <- "Absolute value of the EVPPI"
        }
      }
      
      xlim <- 
        if (exists("xlim", where = extra_args)) {
          extra_args$xlim
        } else {c(0, range(scores)[2])}
      
      mai <- 
        if (exists("mai", where = extra_args)) {
          extra_args$mai
        } else {c(1.36, 1.5, 1, 1)}
      
      tit <- 
        if (exists("tit", where = extra_args)) {
          extra_args$tit
        } else {paste0("Info-rank plot for willingness to pay = ", wtp)}
      
      space <- 
        if (exists("space", where = extra_args)) {
          extra_args$space
        } else {0.5}
    } else {
      ca <- NULL
      if (exists("ca", where = extra_args)) {
        warning("Argument ca was specified in info.rank.plotly but is not an accepted argument.
                Parameter will be ignored.")}
      cn <- NULL
      if (exists("cn", where = extra_args)) {
        warning("Argument cn was specified in info.rank.plotly but is not an accepted argument.
                Parameter will be ignored.")}
      xlab <- "Proportion of total EVPI"
      if (exists("rel", where = extra_args)) {
        if (!extra_args$rel) {
          scores <- unlist(lapply(x, function(x) x$evppi))
          xlab <- "Absolute value of the EVPPI"
        }
      }
      
      xlim <- 
        if (exists("xlim", where = extra_args)) {
          extra_args$xlim
        } else {NULL}
      
      mai <- 
        if (exists("mai", where = extra_args)) {
          extra_args$mai
        } else {NULL}
      
      tit <-
        if (exists("tit", where = extra_args)) {
          extra_args$tit
        } else {
          paste0("Info-rank plot for willingness to pay = ", wtp)}
      
      space <- 
        if (exists("space", where = extra_args)) {
          extra_args$space
        } else {NULL}
    }
    
    make.barplot(
      scores = scores,
      chk2 = chk2,
      tit = tit,
      xlab = xlab,
      xlim = xlim,
      ca,
      cn,
      mai = mai,
      space,
      howManyPars) 
  }
}


#' @title Information-Rank Plot
#' 
#' @description Produces a plot similar to a Tornado-plot, but based on the analysis of the
#' EVPPI. For each parameter and value of the willingness-to-pay threshold, a
#' barchart is plotted to describe the ratio of EVPPI (specific to that
#' parameter) to EVPI. This represents the relative `importance' of each
#' parameter in terms of the expected value of information.
#' 
#' @template args-he
#' @param ... Additional options. These include graphical parameters that the
#'   user can specify:
#'   \itemize{
#'   \item \code{xlim} = limits of the x-axis; ca = font size for the axis
#'   label (default = 0.7 of full size).
#'   \item \code{cn} = font size for the parameter names
#'   vector (default = 0.7 of full size) - base graphics only.
#'   \item \code{mai} = margins of the graph (default = c(1.36, 1.5, 1,1)) - base graphics only.
#'   \item \code{rel} = logical argument that specifies whether the ratio of
#'   EVPPI to EVPI (\code{rel = TRUE}, default) or the absolute value of the EVPPI
#'   should be used for the analysis.
#'   }
#' @return \item{res}{With base graphics: A data.frame containing the ranking of the parameters
#'   with the value of the selected summary, for the chosen wtp; with plotly: a plotly object, 
#'   incorporating in the $rank element the data.frame as above.}
#'   The function produces a 'Info-rank' plot. This is an extension of standard 'Tornado
#'   plots' and presents a ranking of the model parameters in terms of their
#'   impact on the expected value of information. For each parameter, the
#'   specific individual EVPPI is computed and used to measure the impact of
#'   uncertainty in that parameter over the decision-making process, in terms of
#'   how large the expected value of gaining more information is.
#' @name info_rank
#' 
#' @export
info.rank <- function(he, ...) {
  UseMethod('info.rank', he)
}

