
#' Summary Plot of the Health Economic Analysis
#' 
#' Plots in a single graph the Cost-Effectiveness plane, the Expected
#' Incremental Benefit, the CEAC and the EVPI.
#' 
#' The default position of the legend for the cost-effectiveness plane
#' (produced by \code{\link{ceplane.plot}}) is set to \code{c(1, 1.025)}
#' overriding its default for \code{pos=FALSE}, since multiple ggplot2 plots
#' are rendered in a slightly different way than single plots.
#' 
#' @template args-he 
#' @template args-comparison
#' @param wtp The value of the willingness to pay parameter. It is passed to
#' \code{\link{ceplane.plot}}.
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string, a single logical value, or a two-element vector with the
#' respective relative positions on the x and y axis. Default as \code{FALSE}
#' sets the legend position to the default one for each plot (see the details
#' section), while \code{TRUE} puts it on the bottom of each plot.  Changes
#' will affect all the individual plots.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  Arguments to be passed to the methods \code{\link{ceplane.plot}}
#' and \code{\link{eib.plot}}. Please see the manual pages for the individual
#' functions.  Arguments like \code{size}, \code{ICER.size} and \code{plot.cri}
#' can be supplied to the functions in this way. In addition if
#' \code{graph="ggplot2"} and the arguments are named theme objects they will
#' be added to each plot.
#' 
#' @return A plot with four graphical summaries of the health economic evaluation.
#' 
#' @author Gianluca Baio, Andrea Berardi
#' 
#' @seealso \code{\link{bcea}},
#'          \code{\link{ceplane.plot}},
#'          \code{\link{eib.plot}},
#'          \code{\link{ceac.plot}},
#'          \code{\link{evi.plot}}
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics. Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' 
#' @keywords "Health economic evaluation" hplot
#' 
#' @examples
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#'
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#' 
#' # Runs the health economic evaluation using BCEA
#' he <- bcea(
#'        e=e, c=c,             # defines the variables of 
#'                              #  effectiveness and cost
#'        ref=2,                # selects the 2nd row of (e,c) 
#'                              #  as containing the reference intervention
#'        interventions=treats, # defines the labels to be associated 
#'                              #  with each intervention
#'        Kmax=50000,           # maximum value possible for the willingness 
#'                              #  to pay threshold; implies that k is chosen 
#'                              #  in a grid from the interval (0,Kmax)
#'        plot=FALSE            # does not produce graphical outputs
#'       )
#'
#' # Plots the summary plots for the "bcea" object m using base graphics
#' plot(he, graph = "base")
#' 
#' # Plots the same summary plots using ggplot2
#' if(require(ggplot2)){
#' plot(he, graph = "ggplot2")
#' 
#' ##### Example of a customized plot.bcea with ggplot2
#' plot(he,
#'   graph = "ggplot2",                                      # use ggplot2
#'   theme = theme(plot.title=element_text(size=rel(1.25))), # theme elements must have a name
#'   ICER.size = 1.5,                                        # hidden option in ceplane.plot
#'   size = rel(2.5)                                         # modifies the size of k = labels
#'   )                                                       # in ceplane.plot and eib.plot
#' }
#' 
#' @import ggplot2
#' @export
#' 
plot.bcea <- function(he,
                      comparison = NULL,
                      wtp = 25000,
                      pos = FALSE,
                      graph = c("base", "ggplot2"),
                      ...) {
  
  named_args <- c(as.list(environment()), list(...))
  graph <- match.arg(graph)
  use_base_graphics <- pmatch(graph, c("base", "ggplot2")) != 2
  extra_args <- list(...)
  
  if (use_base_graphics) {
    op <- par(mfrow = c(2,2))
    
    ceplane.plot(he,
                 comparison = comparison,
                 wtp = wtp,
                 pos = pos,
                 graph = "base",...)
    
    eib.plot(he,
             comparison = comparison,
             pos = pos,
             graph = "base",...)
    
    ceac.plot(he,
              pos = pos,
              graph = "base")
    
    evi.plot(he,
             graph = "base")
    par(op)
  } else {
    
    is_req_pkgs <- map_lgl(c("ggplot2","grid"), requireNamespace, quietly = TRUE)
    
    if (!all(is_req_pkgs)) {
      message("falling back to base graphics\n")
      plot.bcea(
        he,
        comparison = comparison,
        wtp = wtp,
        pos = pos,
        graph = "base", ...)
      return(invisible(NULL))
    }
    
    if (all(is_req_pkgs)) {

      theme_params <- 
        list(text = element_text(size = 9),
             legend.key.size = grid::unit(0.5, "lines"),
             legend.spacing = grid::unit(-1.25, "line"),
             panel.grid = element_blank(),
             legend.key = element_blank(),
             plot.title = element_text(
               lineheight = 1,
               face = "bold",
               size = 11.5,
               hjust = 0.5))
      
      theme_add <- purrr::keep(extra_args, is.theme)
      
      ceplane.pos <- ifelse(pos, pos, c(1, 1.025))
      
      #TODO: warnings...
      ceplane <-
        ceplane.plot(he,
                     wtp = wtp,
                     pos = ceplane.pos,
                     comparison = comparison,
                     graph = "ggplot2", ...) +
        do.call(theme, theme_params) +
        theme_add
      
      eib <-
        eib.plot(he,
                 pos = pos,
                 comparison = comparison,
                 graph = "ggplot2", ...) +
        do.call(theme, theme_params) +
        theme_add
      
      ceac <-
        ceac.plot(he,
                  pos = pos,
                  comparison = comparison,
                  graph = "ggplot2") +
        do.call(theme, theme_params) +
        theme_add
      
      evi <-
        evi.plot(he, graph = "ggplot2") +
        do.call(theme, theme_params) +
        theme_add
      
      multiplot(list(ceplane, ceac, eib, evi),
                cols = 2,
                extra_args = extra_args)
    }
  }
}


#' multiplot
#' 
#' Arrange plots in grid. Source from R graphics cookbook.
#' 
#' @param plotlist List of ggplot objects
#' @param cols Number of columns
#' @param layout Matrix
#' @param extra_args List of plotting arguments
#' @param ... Potential further arguments.
#' @return Plot
#' 
multiplot <- function(plotlist = NULL,
                      cols = 1,
                      layout_config = NULL,
                      extra_args, ...) {
  
  n_plots <- length(plotlist)

  if (is.null(layout_config)) {
    layout_config <- matrix(seq(1, cols*ceiling(n_plots/cols)),
                     ncol = cols,
                     nrow = ceiling(n_plots/cols))
  }
  if (n_plots == 1) {
    print(plotlist[[1]])
  } else {
    grid::grid.newpage()
    
    grid::pushViewport(
      grid::viewport(layout =
                       grid::grid.layout(nrow(layout_config),
                                         ncol(layout_config))))
    
    for (i in seq_len(n_plots)) {
      matchidx <- as.data.frame(which(layout_config == i, arr.ind = TRUE))
      print(plotlist[[i]],
            vp = grid::viewport(layout.pos.row = matchidx$row,
                                layout.pos.col = matchidx$col))
    }
  }
}

