
##TODO: how are these different to eib_plot_*, evi_plot_*?
##      look at plots in book and examples
##      look at original code
##      can we just use existing code?

#'
CEriskav_plot_base <- function(x, alt.legend) {
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
  
  plot(x = x$k, y = x$eibr[, 1],
       type = "l",
       xlab = "Willingness to pay",
       ylab = " ",
       main = "EIB as a function of the risk aversion parameter",
       ylim = range(x$eibr))
  
  linetype <- seq(1, x$R)
  
  ##TODO: use multiplot() instead?
  for (l in 2:x$R) {
    points(x$k, x$eibr[, l], type = "l", lty = linetype[l])
  }
  
  text <- paste("r = ", x$r, sep = "") 
  
  # if the first value for r is small enough,
  # consider close to 0 and print label accordingly
  if (x$r[1] < 1e-8) {
    text[1] <- expression(r%->%0)
  }
  legend(alt.legend, text, lty = seq(1:x$R), cex = 0.9, box.lty = 0)
  abline(h = 0, col = "grey")
  
  plot(x$k,
       x$evir[, 1],
       type = "l",
       ylim = range(x$evir),
       xlab = "Willingness to pay",
       ylab = " ",
       main = "EVPI as a function of the risk aversion parameter")
  for (l in 2:x$R) {
    points(x$k, x$evir[, l], type = "l", lty = linetype[l])
  }
  
  legend(alt.legend,
         legend = text,
         lty = seq(1:x$R),
         cex = 0.9,
         box.lty = 0)
  abline(h = 0, col = "grey")
}



#'
CEriskav_plot_ggplot <- function(x, alt.legend) {
  
  # no visible bindings note
  k <- r <- NA_real_
  
  linetypes <- rep(c(1,2,3,4,5,6), ceiling(x$R/6))[1:x$R]
  
  df <- data.frame(cbind(rep(x$k,x$R), c(x$eibr), c(x$evir)),
                   as.factor(sort(rep(1:x$R, length(x$k)))))
  names(df) <- c("k", "eibr", "evir", "r")
  
  # labels
  text <- paste0("r = ", x$r)
  
  # if the first value for r is small enough,
  # consider close to 0 and print label accordingly
  if (x$r[1] < 1e-8) {
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
  plot(evir)
  
  return(
    invisible(list(eib = eibr,
                   evi = evir)))
}


