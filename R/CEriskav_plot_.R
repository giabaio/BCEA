
##TODO: how are these different to eib_plot_*, evi_plot_*?
##      look at plots in book and examples
##      look at original code
##      can we just use existing code?


#' CEriskav base R version
#' 
CEriskav_plot_base <- function(he, pos_legend) {
  
  pos_legend <- where_legend(he, pos_legend)
    
  matplot(x = he$k,
          y = he$eibr[, 1, ],
          type = "l",
          col = 1,
          lty = 1:he$R,
          xlab = "Willingness to pay",
          ylab = " ",
          main = "EIB as a function of the risk aversion parameter",
          ylim = range(he$eibr))
  
  text <- paste("r = ", he$r, sep = "") 
  
  # if the first value for r is small enough,
  # consider close to 0 and print label accordingly
  if (he$r[1] < 1e-8) {
    text[1] <- expression(r%->%0)
  }
  
  legend(pos_legend,
         legend = text,
         lty = 1:he$R,
         cex = 0.9,
         box.lty = 0)
  abline(h = 0, col = "grey")
  
  matplot(x = he$k,
          y = he$evir,
          type = "l",
          col = 1,
          lty = 1:he$R,
          ylim = range(he$evir),
          xlab = "Willingness to pay",
          ylab = " ",
          main = "EVPI as a function of the risk aversion parameter")
  
  legend(pos_legend,
         legend = text,
         lty = 1:he$R,
         cex = 0.9,
         box.lty = 0)
  abline(h = 0, col = "grey")
}


#' CEriskav ggplot2 version
#' 
CEriskav_plot_ggplot <- function(he, pos_legend) {
  
  # no visible bindings note
  k <- r <- NA_real_
  
  linetypes <- rep(c(1,2,3,4,5,6), ceiling(he$R/6))[1:he$R]
  
  df <- data.frame(cbind(rep(he$k, he$R), 
                         c(he$eibr), c(he$evir)),
                   as.factor(sort(rep(1:he$R, length(he$k)))))
  names(df) <- c("k", "eibr", "evir", "r")
  
  # labels
  text <- paste0("r = ", he$r)
  
  # if the first value for r is small enough,
  # consider close to 0 and print label accordingly
  if (he$r[1] < 1e-8) {
    text[1] <- expression(r%->%0)
  }
  
  eib_dat <-
    melt(he$eibr[, 1,],
         value.name = "eibr",
         variable.name = "k") %>% 
    mutate(r = as.factor(Var2))
  
  eibr <-
    ggplot(eib_dat, aes(x = k, y = eibr, linetype = r)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = 1, colour = "grey50") +
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
  
  ##TODO: use where_legend()?
  if (isTRUE(pos_legend)) {
    pos_legend <- "bottom"
    eibr <- eibr + theme(legend.direction = "vertical")
    evir <- evir + theme(legend.direction = "vertical")
  } else {
    if (is.character(pos_legend)) {
      choices <- c("left", "right", "bottom", "top")
      pos_legend <- choices[pmatch(pos_legend,choices)]
      jus <- "center"
      if (is.na(pos_legend))
        pos_legend <- FALSE
    }
    if (length(pos_legend) > 1)
      jus <- pos_legend
    if (length(pos_legend) == 1 && !is.character(pos_legend)) {
      pos_legend <- c(0,1)
      jus <- pos_legend
    }
  }
  
  eibr <-
    eibr + 
    theme(
      legend.position = pos_legend,
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
      legend.position = pos_legend,
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

