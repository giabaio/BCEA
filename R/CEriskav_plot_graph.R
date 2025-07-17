##TODO: how are these different to eib_plot_*, evi_plot_*?
##      look at plots in book and examples
##      look at original code
##      can we just use existing code?


#' @name CEriskav_plot_graph
#' @title Cost-effectiveness Plot Including a Parameter of Risk Aversion
#'
#' @description Choice of base R, \pkg{ggplot2} or \pkg{plotly}.
#'
#' @template args-he
#' @param pos_legend Legend position
NULL


#' @rdname CEriskav_plot_graph
#' @title CEriskav base R version
#'
CEriskav_plot_base <- function(he, pos_legend) {

  default_comp <- 1
  pos_legend <- where_legend(he, pos_legend)

  matplot(x = he$k,
          y = he$eibr[, default_comp, ],
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
          main = "EVI as a function of the risk aversion parameter")

  legend(pos_legend,
         legend = text,
         lty = 1:he$R,
         cex = 0.9,
         box.lty = 0)
  abline(h = 0, col = "grey")
}


#' @rdname CEriskav_plot_graph
#' @title CEriskav ggplot2 version
#'
CEriskav_plot_ggplot <- function(he, pos_legend) {
  default_comp <- 1
  linetypes <- rep(c(1, 2, 3, 4, 5, 6), ceiling(he$R / 6))[1:he$R]
  
  # labels
  text <- paste0("r = ", he$r)
  if (he$r[1] < 1e-8) {
    text[1] <- expression(r %->% 0)
  }
  
  legend_params <- make_legend_ggplot(he, pos_legend)
  
  ## Reshape eibr data
  eib_dat <- he$eibr[, default_comp, , drop = FALSE] |>
    as_tibble() |>
    mutate(k = row_number()) |>
    pivot_longer(
      cols = -"k",
      names_to = "r",
      values_to = "eibr",
      names_transform = list(r = as.integer)
    ) |>
    mutate(r = factor(.data$r))
  
  ## Create EIB plot
  eibr_plot <- ggplot(eib_dat, aes(x = .data$k, y = .data$eibr, linetype = .data$r)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = 1, color = "grey50") +
    scale_linetype_manual("", labels = text, values = linetypes) +
    theme_bw() +
    labs(
      title = "EIB as a function of the risk aversion parameter",
      x = "Willingness to pay",
      y = "EIB"
    ) +
    theme(
      text = element_text(size = 11),
      legend.key.size = unit(0.66, "line"),
      legend.spacing = unit(-1.25, "line"),
      panel.grid = element_blank(),
      legend.key = element_blank(),
      legend.position = legend_params$legend.position,
      legend.justification = legend_params$legend.justification,
      legend.direction = legend_params$legend_direction,
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(hjust = 0),
      plot.title = element_text(
        lineheight = 1.05,
        face = "bold",
        size = 14.3,
        hjust = 0.5
      )
    )
  
  ## Reshape evir data
  evi_dat <- he$evir |>
    as_tibble() |>
    mutate(k = row_number()) |>
    pivot_longer(
      cols = -"k",
      names_to = "r",
      values_to = "evir",
      names_transform = list(r = as.integer)
    ) |>
    mutate(r = factor(.data$r))
  
  ## Create EVI plot
  evir_plot <- ggplot(evi_dat, aes(x = .data$k, y = .data$evir, linetype = .data$r)) +
    geom_hline(yintercept = 0, linetype = 1, color = "grey50") +
    geom_line() +
    scale_linetype_manual("", labels = text, values = linetypes) +
    theme_bw() +
    labs(
      title = "EVI as a function of the risk aversion parameter",
      x = "Willingness to pay",
      y = "EVI"
    ) +
    theme(
      text = element_text(size = 11),
      legend.key.size = unit(0.66, "line"),
      legend.spacing = unit(-1.25, "line"),
      panel.grid = element_blank(),
      legend.key = element_blank(),
      legend.position = legend_params$legend.position,
      legend.justification = legend_params$legend.justification,
      legend.direction = legend_params$legend.direction,
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(hjust = 0),
      plot.title = element_text(
        lineheight = 1.05,
        face = "bold",
        size = 14.3,
        hjust = 0.5
      )
    )
  
  plot(eibr_plot)
  plot(evir_plot)
  
  invisible(list(eib = eibr_plot, evi = evir_plot))
}


#' @rdname CEriskav_plot_graph
#' @title CEriskav plotly version
#'
CEriskav_plot_plotly <- function(he, pos_legend) {
  default_comp <- 1
  linetypes <- rep(c(1, 2, 3, 4, 5, 6), ceiling(he$R / 6))[1:he$R]
  
  # labels
  text <- paste0("r = ", he$r)
  if (he$r[1] < 1e-8) {
    text[1] <- paste("r", "\U2B62", "0")
  }
  
  legend_params <- make_legend_plotly(pos_legend)
  
  ## Reshape eibr data
  eib_dat <- he$eibr[, default_comp, , drop = FALSE] |>
    as_tibble() |>
    mutate(k = row_number()) |>
    pivot_longer(
      cols = -"k",
      names_to = "r",
      values_to = "eibr",
      names_transform = list(r = as.integer)
    ) |>
    mutate(r = factor(.data$r, labels = text))
  
  ## Reshape evir data
  evi_dat <- he$evir |>
    as_tibble() |>
    mutate(k = row_number()) |>
    pivot_longer(
      cols = -"k",
      names_to = "r",
      values_to = "evir",
      names_transform = list(r = as.integer)
    ) |>
    mutate(r = factor(.data$r, labels = text))
  
  ## EIB plot
  eibr_plot <-
    plotly::plot_ly(data = eib_dat, linetype = ~r, x = ~k) |>
    plotly::add_trace(
      y = ~eibr,
      type = "scatter",
      mode = "lines",
      linetypes = linetypes,
      line = list(color = "black")
    ) |>
    plotly::layout(
      title = "EIB as a function of the risk aversion parameter",
      xaxis = list(title = "Willingness to pay"),
      yaxis = list(title = "EIB"),
      legend = list(title = list(text = "Risk aversion"))
    ) |>
    plotly::config(displayModeBar = FALSE)
  
  ## EVI plot
  evir_plot <-
    plotly::plot_ly(data = evi_dat, linetype = ~r, x = ~k) |>
    plotly::add_trace(
      y = ~evir,
      type = "scatter",
      mode = "lines",
      linetypes = linetypes,
      line = list(color = "black")
    ) |>
    plotly::layout(
      title = "EVI as a function of the risk aversion parameter",
      xaxis = list(title = "Willingness to pay"),
      yaxis = list(title = "EVI"),
      legend = list(title = list(text = "Risk aversion"))
    ) |>
    plotly::config(displayModeBar = FALSE)
  
  print(list(eibr_plot, evir_plot))
  
  invisible(list(eib = eibr_plot, evi = evir_plot))
}

