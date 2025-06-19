
#' A Custom ggplot2 Theme for BCEA
#'
#' This theme is designed for use with plots created using the `BCEA` package. 
#' It builds on `theme_bw()` and customizes axis text, titles, 
#' plot background, and legend styling.
#'
#' @return A ggplot2 theme object that can be added to a ggplot.
#' @examples
#' library(ggplot2)
#' library(BCEA)
#' ggplot(mtcars, aes(wt, mpg)) +
#'  geom_point() +
#'  theme_BCEA() 
#' @export
#' 
theme_BCEA <- function() {
  theme_bw() +
    theme(
      text = element_text(size = 13),
      legend.key.size = grid::unit(0.5, "lines"),
      legend.spacing = grid::unit(-1.25, "line"),
      panel.grid = element_blank(),
      legend.key = element_blank(),
      plot.title = element_text(
        lineheight = 1,
        face = "bold",
        size = 11.5,
        hjust = 0.5
      )
    )
}

#' Utility function to modify simply the `ggplot` `theme_BCEA()` theme
#' using inputs inside the call to `plot.bcea`
#'
#' @noRd
#' @keywords internal
#' 
process_element_arg <- function(arg, constructor) {
  
  # Define a safe resolve_rel_size() helper
  resolve_rel_size <- function(size, base = 9) {
    if (inherits(size, "rel") && !is.null(attr(size, "val"))) {
      return(attr(size, "val") * base)
    } else if (is.numeric(size)) {
      return(size)
    } else {
      stop("Invalid size value: must be numeric or rel().")
    }
  }
  
  if (inherits(arg, class(constructor()))) {
    return(arg)
  } else if (is.list(arg)) {
    if ("size" %in% names(arg)) {
      arg$size <- resolve_rel_size(arg$size, base = base_size)
    }
    return(do.call(constructor, arg))
  } else if (is.null(arg)) {
    return(NULL)
  } else {
    stop("Invalid theme element.")
  }
}


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
      
      # Map theme element names to their constructors
      element_constructors <- list(
        text = ggplot2::element_text,
        axis.text = ggplot2::element_text,
        axis.title = ggplot2::element_text,
        legend.title = ggplot2::element_text,
        legend.text = ggplot2::element_text,
        plot.title = ggplot2::element_text,
        strip.text = ggplot2::element_text,
        line = ggplot2::element_line,
        axis.line = ggplot2::element_line,
        rect = ggplot2::element_rect,
        panel.background = ggplot2::element_rect
        # Add more as needed
      )
      
      # Process only matching theme elements
      update_theme_args <- list()
      for (nm in names(extra_args)) {
        if (nm %in% names(element_constructors)) {
          constructor <- element_constructors[[nm]]
          update_theme_args[[nm]] <- process_element_arg(
            extra_args[[nm]], constructor
          )
        }
      }
      
      ceplane <-
        ceplane.plot(x,
                     wtp = wtp,
                     pos = pos,
                     comparison = comparison,
                     graph = "ggplot2", ...) + 
        do.call(ggplot2::theme, update_theme_args)
      
      eib <-
        do.call(eib.plot,
                c(he = list(x),
                  pos = pos,
                  comparison = comparison,
                  graph = "ggplot2",
                  extra_args)) +
        do.call(ggplot2::theme, update_theme_args)
      
      ceac <-
        do.call(ceac.plot,
                c(he = list(x),
                  pos = pos,
                  comparison = comparison,
                  graph = "ggplot2",
                  extra_args)) + 
        do.call(ggplot2::theme, update_theme_args)
      
      evi <-
        evi.plot(x, graph = "ggplot2", ...) + 
        do.call(ggplot2::theme, update_theme_args)
      
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
    
    p1 = ceplane.plot(x, graph = "p", wtp = wtp, ...)
    p2 = eib.plot(x, graph = "p", ...)
    p3 = ceac.plot(x, graph = "p", ...)
    p4 = evi.plot(x, graph = "p", ...)
    
    ppp = plotly::subplot(
      p1, p2, p3, p4, nrows = 2,
      titleX = TRUE, titleY = TRUE, margin = 0.1
    )
    
    annotations = list(
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
    
    ppp = ppp |>
      layout(annotations = annotations, title = "")
    return(ppp)
  }
}

