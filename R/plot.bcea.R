
#' Summary Plot of the Health Economic Analysis
#' 
#' Plots in a single graph the Cost-Effectiveness plane, the Expected
#' Incremental Benefit, the CEAC and the EVPI.
#' 
#' The default position of the legend for the cost-effectiveness plane
#' (produced by [ceplane.plot()]) is set to `c(1, 1.025)`
#' overriding its default for `pos=FALSE`, since multiple ggplot2 plots
#' are rendered in a slightly different way than single plots.
#' 
#' @param x A `bcea` object containing the results of the Bayesian
#' modelling and the economic evaluation.
#' @template args-comparison
#' @param wtp The value of the willingness to pay parameter. It is passed to
#' [ceplane.plot()].
#' @template args-pos
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options `"base"` or
#' `"ggplot2"`. Default value is `"base"`.
#' @param ...  Arguments to be passed to the methods [ceplane.plot()]
#' and [eib.plot()]. Please see the manual pages for the individual
#' functions.  Arguments like `size`, `ICER.size` and `plot.cri`
#' can be supplied to the functions in this way. In addition if
#' `graph="ggplot2"` and the arguments are named theme objects they will
#' be added to each plot.
#' 
#' @return A plot with four graphical summaries of the health economic evaluation.
#' 
#' @author Gianluca Baio, Andrea Berardi
#' 
#' @seealso [bcea()],
#'          [ceplane.plot()],
#'          [eib.plot()],
#'          [ceac.plot()],
#'          [evi.plot()]
#' @importFrom Rdpack reprompt
#'          
#' @references
#' 
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
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
#'        e=eff, c=cost,        # defines the variables of 
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
#' # Example of a customized plot.bcea with ggplot2
#' plot(he,
#'   graph = "ggplot2",          # use ggplot2
#'   ICER_size = 1.5,            # extra options modifies the mean point size
#'   text = list(size=rel(1.25)) # modifies the text size
#'   )                                                       
#' }
#' 
#' @import ggplot2
#' @export
#' 
plot.bcea <- function(x,
                      comparison = NULL,
                      wtp = 25000,
                      pos = FALSE,
                      graph = c("base", "ggplot2"),
                      ...) {
  
  ##TODO: where should this be used?
  # named_args <- c(as.list(environment()), list(...))
  
  graph <- match.arg(graph)
  use_base_graphics <- pmatch(graph, c("base", "ggplot2")) != 2
  extra_args <- list(...)
  
  # consistent colours across plots
  if (is.element("point", names(extra_args))) {
    if (is.element("color", names(extra_args$point))) {
      extra_args$line$color <- extra_args$point$color   
    }}
  
  if (use_base_graphics) {
    withr::with_par(list(mfrow = c(2,2)), {
      ceplane.plot(x,
                   comparison = comparison,
                   wtp = wtp,
                   pos = pos,
                   graph = "base",...)
      
      do.call(eib.plot,
              c(he = list(x),
                pos = pos,
                comparison = comparison,
                graph = "base",
                extra_args))
      
      do.call(ceac.plot,
              c(he = list(x),
                pos = pos,
                graph = "base",
                extra_args))
      
      evi.plot(x,
               graph = "base", ...)
    })
  } else {
    
    is_req_pkgs <- unname(sapply(c("ggplot2", "grid"),
                                 requireNamespace, quietly = TRUE))
    
    if (!all(is_req_pkgs)) {
      message("falling back to base graphics\n")
      plot.bcea(
        x,
        comparison = comparison,
        wtp = wtp,
        pos = pos,
        graph = "base", ...)
      
      return(invisible(NULL))
    }
    
    if (all(is_req_pkgs)) {
      
      default_params <- 
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
      
      keep_param <-
        names(default_params)[names(default_params) %in% names(extra_args)]
      
      extra_params <- extra_args[keep_param]
      
      global_params <-
        modifyList(default_params,
                   extra_params,
                   keep.null = TRUE)
      
      theme_add <- purrr::keep(extra_args, is.theme)
      
   ceplane.pos <- ifelse(pos, pos, c(1, 1.025))
      
      ##TODO: warnings...
      ceplane <-
        ceplane.plot(x,
                     wtp = wtp,
                     pos = ceplane.pos,
                     comparison = comparison,
                     graph = "ggplot2", ...) +
        do.call(theme, global_params) +
        theme_add
      
      eib <-
        do.call(eib.plot,
                c(he = list(x),
                  pos = pos,
                  comparison = comparison,
                  graph = "ggplot2",
                  extra_args)) +
        do.call(theme, global_params) +
        theme_add
      
      ceac <-
        do.call(ceac.plot,
                c(he = list(x),
                  pos = pos,
                  comparison = comparison,
                  graph = "ggplot2",
                  extra_args)) +
        do.call(theme, global_params) +
        theme_add
      
      evi <-
        evi.plot(x, graph = "ggplot2", ...) +
        do.call(theme, global_params) +
        theme_add
      
      multiplot(list(ceplane, ceac, eib, evi),
                cols = 2)
    }
  }
}

