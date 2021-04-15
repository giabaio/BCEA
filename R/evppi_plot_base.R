
#' evppi_plot_base
#'
#' @param he 
#' @param pos_legend 
#' @param graph_params
#' @param col
#'
#' @return NULL
#' @export
#'
#' @examples
#' 
evppi_plot_base <- function(he,
                            pos_legend = pos,
                            # graph_params,
                            col) {
  
  if (is.numeric(pos_legend) &&
      length(pos_legend) == 2) {
    temp <- ""
    
    if (pos_legend[2] == 0) 
      temp <- paste0(temp, "bottom")
    else if (pos_legend[2] != 0.5) 
      temp <- paste0(temp, "top")
    
    if (pos_legend[1] == 1) 
      temp <- paste0(temp, "right")
    else temp <- paste0(temp, "left")
    
    pos_legend <- temp
    if (length(grep("^((bottom|top)(left|right)|right)$", temp)) == 0) 
      pos_legend <- FALSE
  }
  if (is.logical(pos_legend)) {
    pos_legend <- 
      if (!pos_legend) "topright"
    else "topleft"
  }
  
  plot(
    he$k,
    he$evi,
    type = "l",
    xlab = "Willingness to pay",
    ylab = "",
    main = "Expected Value of Perfect Partial Information",
    lwd = 2,
    ylim = range(range(he$evi),
                 range(he$evppi)))
  
  # colours
  if (is.null(col)) {
    cols <- colors()
    gr <- floor(seq(from = 261, to = 336, length.out = length(he$index)))
    col <- cols[gr]
  } else {
    if (length(col) != length(he$parameters)) {
      message("The vector 'col' must have the same number of elements as the number of parameters.
                Forced to black\n")
      col <- rep("black", length(he$parameters))
    }
  }
  if (length(he$index) == 1 ||
      length(he$index) > 1 &&
      class(he$method) == "list") {
    col <- "black"
    points(he$k, he$evppi, type = "l", col = col, lty = 1)
  }
  
  # legend
  cmd <- "EVPPI for the selected\nsubset of parameters"
  
  if (nchar(he$params[1]) <= 25) {
    cmd <- paste0("EVPPI for ", he$params)
  }
  
  if (length(he$index) > 1 &&
      (("Strong & Oakley (univariate)" %in% he$method) || 
       ("Sadatsafavi et al" %in% he$method))) {
    for (i in seq_along(he$index)) {
      points(he$k,
             he$evppi[[i]],
             type = "l",
             col = col[i], 
             lty = i)
      text(par("usr")[2], he$evppi[[i]][length(he$k)], 
           paste0("(", i, ")"), cex = 0.7, pos = 2)
    }
    cmd <-
      paste0("(", paste(1:length(he$index)), ") EVPPI for ", he$params)
  }
  legend(
    pos_legend,
    c("EVPI", cmd),
    col = c("black", col),
    cex = 0.7,
    bty = "n",
    lty = c(1, 1:length(he$params)),
    lwd = c(2, rep(1, length(he$params)))
  )
  return(invisible(NULL))
}

