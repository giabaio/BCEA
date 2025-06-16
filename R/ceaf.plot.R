
#' @rdname ceaf.plot
#' 
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-) match the two options `"base"` or
#' `"ggplot2"`. Default value is `"base"`.
#' 
#' @return \item{ceaf}{A ggplot object containing the plot. Returned only if
#' `graph="ggplot2"`.}
#' 
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()],
#'          [multi.ce()]
#' 
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' 
#' @import ggplot2 grid
#' @importFrom graphics lines
#' @importFrom Rdpack reprompt
#' 
#' @examples
#' 
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#'
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#' 
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(
#'       e=eff,
#'       c=cost,               # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e, c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0, Kmax)
#'       plot=FALSE            # inhibits graphical output
#' )
#'
#' \donttest{
#' mce <- multi.ce(m)          # uses the results of the economic analysis 
#' }
#'
#' \donttest{
#' ceaf.plot(mce)              # plots the CEAF 
#' }
#'
#' \donttest{
#' ceaf.plot(mce, graph = "g") # uses ggplot2 
#' }
#' 
#' \donttest{
#' # Use the smoking cessation dataset
#' data(Smoking)
#' m <- bcea(eff, cost, ref = 4, intervention = treats, Kmax = 500, plot = FALSE)
#' mce <- multi.ce(m)
#' ceaf.plot(mce)
#' }
#'  
#' @export
#' 
ceaf.plot.pairwise <- function(mce,
                               graph = c("base", "ggplot2", "plotly"),
                               ...) {
  
  graph <- match.arg(graph)
  
  if (!(requireNamespace("ggplot2", quietly = TRUE) &&
        requireNamespace("grid", quietly = TRUE))) {
    message("Falling back to base graphics\n")
    base_graphics <- TRUE
  }
  
  if (is_baseplot(graph)) {
    plot(NULL,
         ylim = c(0, 1),
         xlim = c(0, max(mce$k)),
         xlab = "Willingness to pay",
         ylab = "Probability of most cost effectiveness",
         main = "Cost-effectiveness acceptability frontier")
    # matplot(x = mce$k,              # underlying ceac curves
    #         mce$p_best_interv,
    #         type = "l",
    #         lty = 3,
    #         add = TRUE)
    lines(x = mce$k,
          y = mce$ceaf,
          type = "l",
          lty = 1,
          lwd = 4)
  } else if (is_ggplot(graph)) {
    df <- data.frame(k = mce$k,
                     ceaf = mce$ceaf)

    ggplot(df, aes(x = .data$k, y = .data$ceaf)) +
      theme_bw() +
      geom_line() +
      coord_cartesian(ylim = c(-0.05, 1.05)) +
      labs(title = "Cost-effectiveness acceptability frontier",
           x = "Willingness to pay",
           y = "Probability of most cost-effectiveness") +
      theme(text = element_text(size = 11),
            legend.key.size = grid::unit(0.66, "lines"),
            legend.spacing = grid::unit(-1.25, "line"),
            panel.grid = element_blank(),
            legend.key = element_blank(),
            plot.title = element_text(
              lineheight = 1.05,
              face = "bold",
              size = 14.3,
              hjust = 0.5))
  } else if (is_plotly(graph)) {
    df <- data.frame(k = mce$k, ceaf = mce$ceaf)
    
    ceaf_plot <- plotly::plot_ly(df, x = ~k, y = ~ceaf)
    ceaf_plot <-
      plotly::add_lines(ceaf_plot)
    
    ceaf_plot <-
      plotly::layout(
        ceaf_plot,
        title = "Cost-effectiveness acceptability frontier",
        xaxis = list(
          hoverformat = ".2f",
          title = "Willingness to pay"),
        yaxis = list(
          title = "Probability of most cost-effectiveness",
          range = c(0, 1.005))) |>
      plotly::hide_colorbar()
    
    plotly::config(ceaf_plot, displayModeBar = FALSE)
  }
}


#' Cost-Effectiveness Acceptability Frontier (CEAF) plot
#' 
#' Produces a plot the Cost-Effectiveness Acceptability Frontier (CEAF)
#' against the willingness to pay threshold.
#' 
#' @param mce The output of the call to the function [multi.ce()]
#' @param ... Additional arguments
#' 
#' @export
#' 
ceaf.plot <- function(mce, ...) {
  UseMethod('ceaf.plot', mce)
}


