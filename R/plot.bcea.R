
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
#' plotting. Should (partial-)match the options `"base"`,
#' `"ggplot2"` or `"plotly"`. Default value is `"base"`.
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
                      graph = c("base", "ggplot2", "plotly"),
                      ...) {
  
  
  graph <- match.arg(graph)
  extra_args <- list(...)
  
  # consistent colours across plots
  if (is.element("point", names(extra_args))) {
    if (is.element("color", names(extra_args$point))) {
      extra_args$line$color <- extra_args$point$color   
    }}
  
  if (is_baseplot(graph)) {
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
  } else if (is_ggplot(graph)) {
    
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
      
      updated_args <- update_theme_args(extra_args)
      
      ceplane <-
        ceplane.plot(x,
                     wtp = wtp,
                     pos = pos,
                     comparison = comparison,
                     graph = "ggplot2", ...) + 
        do.call(ggplot2::theme, updated_args)
      
      eib <-
        do.call(eib.plot,
                c(he = list(x),
                  pos = pos,
                  comparison = comparison,
                  graph = "ggplot2",
                  extra_args)) +
        do.call(ggplot2::theme, updated_args)
      
      ceac <-
        do.call(ceac.plot,
                c(he = list(x),
                  pos = pos,
                  comparison = comparison,
                  graph = "ggplot2",
                  extra_args)) + 
        do.call(ggplot2::theme, updated_args)
      
      evi <-
        evi.plot(x, graph = "ggplot2", ...) + 
        do.call(ggplot2::theme, updated_args)
      
      multiplot(list(ceplane, ceac, eib, evi),
                cols = 2)
    }
  } else if (is_plotly(graph)) {
    
    extract_plotly_title = function(p) {
      if (exists("layoutAttrs", p$x)) {
        return(p$x$layoutAttrs[[1]]$title)
      } else if (exists("layout", p$x)) {
        return(p$x$layout$title)
      } else {
        message("Plotly title not found in extract_plotly_title()")
        return("Plotly title not found")
      }
    }
    
    p1 <- ceplane.plot(x, graph = "plotly", wtp = wtp, ...)
    p2 <- eib.plot(x, graph = "plotly", ...)
    p3 <- ceac.plot(x, graph = "plotly", ...)
    p4 <- evi.plot(x, graph = "plotly", ...)
    
    ppp <- plotly::subplot(
      p1, p2, p3, p4, nrows = 2,
      titleX = TRUE, titleY = TRUE, margin = 0.1)
    
    annotations <- list(
      list(
        x = 0.2, y = 1.0,
        text = extract_plotly_title(p1),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.8, y = 1.0,
        text = extract_plotly_title(p2),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.2, y = 0.4,
        text = extract_plotly_title(p3),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.8, y = 0.4,
        text = extract_plotly_title(p4),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      )
    )
    
    ppp <-
      layout(ppp, annotations = annotations, title = "")
    
    return(ppp)
  }
}

