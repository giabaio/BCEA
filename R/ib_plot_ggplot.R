
#' Incremental Benefit Plot By Graph Device
#'
#' Choice of base R, ggplot2, plotly
#' @name ib_plot_graph
#' 
NULL


#' IB plot ggplot2 version
#' @rdname ib_plot_graph
#' 
#' @template args-he
#' @param comparison Comparison intervention
#' @param wtp Willingness to pay
#' @param bw band width
#' @param n Number
#' @param xlim x-axis limits
#' 
#' @import ggplot2 grid
#' @importFrom dplyr filter
#' 
ib_plot_ggplot <- function(he,
                           comparison,
                           wtp,
                           bw,
                           n,
                           xlim) {
  
  comparison <- comparison %||% 1
  
  if (max(he$k) < wtp) {
    wtp <- max(he$k)
    message(
      paste0("NB: k (wtp) is defined in the interval [",
             min(he$k), " - ", wtp, "]\n"))
  }
  
  if (!is.element(wtp, he$k)) {
    if (!is.na(he$step)) {
      # The user has selected a non-acceptable value for wtp,
      # but has not specified wtp in the call to bcea
      stop(
        paste("The willingness to pay parameter is defined in the interval [0-", he$Kmax,
              "], with increments of ", he$step,"\n", sep = ""),
        call. = FALSE)
    } else {
      # The user has actually specified wtp as input in the call to bcea
      tmp <- paste(he$k, collapse = " ")
      stop(paste0("The willingness to pay parameter is defined as:\n  [", tmp,
                  "]\n  Please select a suitable value", collapse = " "),
           call. = FALSE)
    }
  }
  
  w <- which(he$k == wtp)
  
  if (he$n_comparisons == 1) {
    
    ##TODO: where should this be used?
    # nbw <- sd(he$ib[w, , 1])/1.5
    
    density <- density(he$ib[w, ,1], bw = bw, n = n)
    df <- data.frame(x = density$x,
                     y = density$y)
  }
  if (he$n_comparisons > 1) {
    
    # nbw <- sd(he$ib[w, , comparison])/1.5
    
    density <- density(he$ib[w, , comparison], bw = bw, n = n)
    
    df <- data.frame(x = density$x,
                     y = density$y)
  }
  
  xlim <- xlim %||% range(df$x)
  
  ib <-
    ggplot(df, aes(.data$x, .data$y)) +
    theme_bw() +
    geom_vline(xintercept = 0,
               color = "grey50",
               size = 0.5) +
    geom_hline(yintercept = 0,
               color = "grey50",
               size = 0.5) +
    geom_ribbon(
      data = dplyr::filter(df, .data$x > 0),
      aes(ymax = .data$y),
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
                       he$interventions[he$comp[comparison]], "")
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

#' IB plot plotly version
#' @rdname ib_plot_graph
#' 
#' @template args-he
#' @param comparison Comparison intervention
#' @param wtp Willingness to pay
#' @param bw band width
#' @param n Number
#' @param xlim x-axis limits
#' 
#' @importFrom plotly plot_ly add_lines TeX toRGB layout config
#' @importFrom dplyr filter
#' 
ib_plot_plotly <- function(he,
                           comparison,
                           wtp,
                           bw,
                           n,
                           xlim) {
  comparison <- comparison %||% 1
  x <- NULL
  
  if (max(he$k) < wtp) {
    wtp <- max(he$k)
    message(
      paste0("NB: k (wtp) is defined in the interval [",
             min(he$k), " - ", wtp, "]\n"))
  }
  
  if (!is.element(wtp, he$k)) {
    if (!is.na(he$step)) {
      # The user has selected a non-acceptable value for wtp,
      # but has not specified wtp in the call to bcea
      stop(
        paste("The willingness to pay parameter is defined in the interval [0-", he$Kmax,
              "], with increments of ", he$step,"\n", sep = ""),
        call. = FALSE)
    } else {
      # The user has actually specified wtp as input in the call to bcea
      tmp <- paste(he$k, collapse = " ")
      stop(paste0("The willingness to pay parameter is defined as:\n  [", tmp,
                  "]\n  Please select a suitable value", collapse = " "),
           call. = FALSE)
    }
  }
  
  w <- which(he$k == wtp)
  
  if (he$n_comparisons == 1) {
    
    ##TODO: where should this be used?
    # nbw <- sd(he$ib[w, , 1])/1.5
    
    density <- density(he$ib[w, ,1], bw = bw, n = n)
    df <- data.frame(x = density$x,
                     y = density$y)
  } else if (he$n_comparisons > 1) {
    
    # nbw <- sd(he$ib[w, , comparison])/1.5
    density <- density(he$ib[w, , comparison], bw = bw, n = n)
    df <- data.frame(x = density$x,
                     y = density$y)
  }
  
  xlim <- xlim %||% range(df$x)
  
  ib <- plotly::plot_ly(data = df) |>
    plotly::add_lines(
      name = "Incremental benefit",
      x = ~x,
      y = ~y,
      line = list(
        color = "black"
      )
    ) |>
    plotly::add_lines(
      name = plotly::TeX(paste0("p(IB(\\theta)>0,k=",wtp,")")),
      # name = paste0("p(IB(theta)>0,k==", wtp, ")") |> str2expression(),
      data = df |> dplyr::filter(x >= 0),
      x = ~x,
      y = ~y,
      line = list(
        color = "black"
      ),
      fill = "tozerox",
      fillcolor =  plotly::toRGB("grey50")
    ) |>
    plotly::layout(
      title = paste0(
        "Incremental Benefit Distribution\n",
        paste0(he$interventions[he$ref]," vs ", he$interventions[he$comp[comparison]], "")
        ),
      xaxis = list(
        hoverformat = ".2f",
        title = plotly::TeX("IB(\\theta)")
      ),
      yaxis = list(
        hoverformat = ".2f",
        title = "Density"
      )
    )
  
  plotly::config(ib, mathjax = "cdn",
                 displayModeBar = FALSE)
}