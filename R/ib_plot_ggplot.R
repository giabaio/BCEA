
#' ib_plot_ggplot
#' 
#' @import ggplot2 grid
#' 
ib_plot_ggplot <- function(he,
                           comparison,
                           wtp,
                           bw,
                           n,
                           xlim) {
                             
  if (!(requireNamespace("ggplot2", quietly = TRUE) &&
        requireNamespace("grid", quietly = TRUE))) {
    message("falling back to base graphics\n")
    
    ib.plot(
      he,
      comparison = comparison,
      wtp = wtp,
      bw = bw,
      n = n,
      xlim = xlim,
      graph = "base")
    return(invisible(NULL))
  }
  
  ### no visible binding note
  x <- y <- NA_real_
  
  if (is.null(comparison)) {
    comparison <- 1
  }
  
  if (max(he$k) < wtp) {
    wtp <- max(he$k)
    message(paste0("NB: k (wtp) is defined in the interval [", min(he$k)," - ", wtp,"]\n"))
  }
  
  if (!is.element(wtp,he$k)) {
    if (!is.na(he$step)) {
      # The user has selected a non-acceptable value for wtp,
      # but has not specified wtp in the call to bcea
      stop(paste("The willingness to pay parameter is defined in the interval [0-", he$Kmax,
                 "], with increments of ",he$step,"\n", sep = ""),
           call. = FALSE)
    } else {
      # The user has actually specified wtp as input in the call to bcea
      tmp <- paste(he$k, collapse = " ")
      stop(paste0("The willingness to pay parameter is defined as:\n  [",tmp,
                  "]\n  Please select a suitable value", collapse = " "),
           call. = FALSE)
    }
  }
  
  w <- which(he$k == wtp)
  
  if (he$n_comparisons == 1) {
    nbw <- sd(he$ib[w, , 1])/1.5
    density <- density(he$ib[w, ,1], bw = bw, n = n)
    df <- data.frame(x = density$x,
                     y = density$y)
  }
  if (he$n_comparisons > 1) {
    nbw <- sd(he$ib[w, , comparison])/1.5
    
    density <- density(he$ib[w, , comparison], bw = bw, n = n)
    
    df <- data.frame(x = density$x,
                     y = density$y)
  }
  if (is.null(xlim)) {
    xlim <- range(df$x)
  }
  ib <-
    ggplot(df, aes(.data$x, .data$y)) +
    theme_bw() +
    geom_vline(xintercept = 0,
               colour = "grey50",
               size = 0.5) +
    geom_hline(yintercept = 0,
               colour = "grey50",
               size = 0.5) +
    geom_ribbon(
      data = subset(df, x > 0),
      aes(ymax = y),
      ymin = 0,
      fill = "grey50",
      alpha = 0.2) +
    geom_line() +
    annotate(
      geom = "text",
      label = paste0("p(IB(theta)>0,k==", wtp, ")"),
      parse = TRUE,
      x = df$x[which.max(df$y)],
      y = max(df$y),
      hjust = -0.5,
      vjust = 1,
      size = 3.5) +
    coord_cartesian(xlim = xlim)
  
  labs.title <- paste0("Incremental Benefit Distribution\n",
                       he$interventions[he$ref]," vs ",
                       he$interventions[he$comp[comparison]],"")
  
  ib +
    theme(
      text = element_text(size = 11),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()) +
    labs(title = labs.title,
         x = parse(text = "IB(theta)"),
         y = "Density") +
    theme(plot.title = element_text(
      lineheight = 1.05,
      face = "bold",
      size = 14.3,
      hjust = 0.5))
}

