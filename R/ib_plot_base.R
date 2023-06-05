
#' IB plot base R version
#' @rdname ib_plot_graph
#' 
#' @template args-he
#' @param comparison Comparison interventions
#' @param wtp Willingness to pay
#' @param bw Band width
#' @param n Number
#' @param xlim x-axis limits
#' @importFrom stats density
#' 
ib_plot_base <- function(he,
                         comparison,
                         wtp,
                         bw,
                         n,
                         xlim) {
  
  if (max(he$k) < wtp) {
    wtp <- max(he$k)
    cat(paste("NB: k (wtp) is defined in the interval [", min(he$k)," - ", wtp,"]\n", sep = ""))
  }
  
  if (!is.element(wtp, he$k)) {
    if (!is.na(he$step)) {
      # The user has selected a non-acceptable value for wtp,
      # but has not specified wtp in the call to bcea
      stop(paste("The willingness to pay parameter is defined in the interval [0-", he$Kmax,
                 "], with increments of ", he$step, "\n", sep = ""),
           call. = FALSE)
    } else { # The user has actually specified wtp as input in the call to bcea
      tmp <- paste(he$k, collapse = " ")
      stop(paste0("The willingness to pay parameter is defined as:\n  [",tmp,
                  "]\n  Please select a suitable value",
                  collapse = " "),
           call. = FALSE)
    }
  }
  
  w <- which(he$k == wtp)
  
  if (he$n_comparisons == 1) {
    
    ##TODO: where should this be used?
    # nbw <- sd(he$ib[w, , 1])/1.5
    
    d <- density(he$ib[w, , 1], bw = bw, n = n)
    txt <- paste("Incremental Benefit distribution\n",
                 he$interventions[he$ref],
                 " vs ", he$interventions[he$comp],
                 sep = "")
  }
  if (he$n_comparisons > 1) {
    
    comparison <- comparison %||% 1
    
    # nbw <- sd(he$ib[w, , comparison])/1.5
    
    d <- density(he$ib[w, , comparison], bw = bw, n = n)
    txt <- paste("Incremental Benefit distribution\n",
                 he$interventions[he$ref],
                 " vs ",
                 he$interventions[he$comp[comparison]],
                 sep = "")
  }
  
  xlim <- xlim %||% range(d$x)
  
  plot(
    d$x,
    d$y,
    type = "l",
    ylab = "Density",
    xlab = expression(paste("IB(", bold(theta), ")", sep = "")),
    main = txt,
    axes = FALSE,
    col = "white",
    xlim = xlim)
  box()
  axis(1)
  
  ypt <- 0.95*max(d$y)
  xpt <- d$x[max(which(d$y >= ypt))]
  
  text(xpt, ypt,
       parse(text = paste("p(IB(",expression(bold(theta)),")>0,k==",
                          format(wtp, digits = 8, nsmall = 2), ")",
                          sep = "")),
       cex = 0.85, pos = 4)
  
  xplus <- d$x[d$x >= 0]
  yplus <- d$y[d$x >= 0]
  
  polygon(c(0, xplus), c(0, yplus), density = 20, border = "white")
  points(d$x, d$y, type = "l")
  abline(v = 0, h = 0, col = "black")
}

