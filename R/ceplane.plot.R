
#' @rdname ceplane.plot
#' 
#' @template args-he
#' @param comparison Selects the comparator, in case of more than two
#'   interventions being analysed. Default as `NULL` plots all the
#'   comparisons together. Any subset of the possible comparisons can be selected
#'   (e.g., `comparison = c(1,3)` or `comparison = 2`).
#' @param wtp The value of the willingness to pay parameter. Not used if
#'   `graph = "base"` for multiple comparisons. For \pkg{ggplot2} and \pkg{plotly} can also provide
#'   a list of arguments for more options (see below).
#' @param pos Parameter to set the position of the legend; for a single
#'   comparison plot, the ICER legend position. Can be given in form of a string
#'   `(bottom|top)(right|left)` for base graphics and
#'   `bottom|top|left|right` for \pkg{ggplot2}. It can be a two-elements vector,
#'   which specifies the relative position on the x and y axis respectively, or
#'   alternatively it can be in form of a logical variable, with `FALSE`
#'   indicating to use the default position and `TRUE` to place it on the
#'   bottom of the plot. Default value is `c(1,1)`, that is the topright
#'   corner inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#'   plotting. Should (partial-) match the three options `"base"`,
#'   `"ggplot2"` or `"plotly"`. Default value is `"base"`.
#' @param ...  If `graph = "ggplot2"` and a named theme object is supplied,
#'   it will be passed to the \pkg{ggplot2} object. The usual ggplot2 syntax is used.
#'   Additional graphical arguments:
#'   
#' - `label.pos = FALSE`: Places the willingness-to-pay label in a 
#'   different position at the bottom of the graph. Applies to base and 
#'   \pkg{ggplot2} only (no label in \pkg{plotly}).
#' - `line = list(color)`: A colour specifying the colour of the 
#'   willingness-to-pay line (not available in the \pkg{plotly} version).
#' - `point = list(color)`: A vector of colours specifying the colour(s) 
#'   associated with the cloud of points. Should be of length 1 or equal 
#'   to the number of comparisons.
#' - `point = list(size)`: A vector specifying the size(s) of the points. 
#'   Should be of length 1 or equal to the number of comparisons.
#' - `point = list(shape)`: A vector specifying the shape(s) of the points. 
#'   Should be of length 1 or equal to the number of comparisons.
#' - `icer = list(color)`: A vector of colours specifying the colour(s) of 
#'   the ICER points. Should be of length 1 or equal to the number of comparisons.
#' - `icer = list(size)`: A vector specifying the size(s) of the ICER points. 
#'   Should be of length 1 or equal to the number of comparisons.
#' - `area_include`: Logical. Include or exclude the cost-effectiveness 
#'   acceptability area (default is `TRUE`).
#' - `wtp = list(value)`: Equivalent to using `wtp = value` but for when 
#'   multiple arguments are passed in list form.
#' - `area = list(color)`: A colour specifying the colour of the 
#'   cost-effectiveness acceptability area.
#' - `wtp = list(color)`: A colour specifying the colour of the 
#'   willingness-to-pay text.
#' - `wtp = list(size)`: A value specifying the size of the 
#'   willingness-to-pay text.
#' - `wtp = list(x=..., y=...)`: Values specifying the x and y coordinates 
#'   of the willingness-to-pay text (not displayed in the \pkg{plotly} version).
#' - `currency`: Currency prefix to cost differential values. Applies to 
#'   \pkg{ggplot2} only.
#' - `icer_annot`: Annotates each ICER point with a text label. Applies to 
#'   \pkg{ggplot2} only.
#'  
#' @return If `graph = "ggplot2"` a ggplot object, or if `graph = "plotly"` 
#'   a plotly object containing the requested plot. Nothing is returned when
#'   `graph = "base"`, the default.
#'    
#'   Grey dots show the simulated values for the joint
#'   distribution of the effectiveness and cost differentials. The larger red
#'   dot shows the ICER and the grey area identifies the sustainability area,
#'   i.e. the part of the plan for which the simulated values are below the
#'   willingness to pay threshold. The proportion of points in the sustainability
#'   area effectively represents the CEAC for a given value of the willingness to
#'   pay. If the comparators are more than 2 and no pairwise comparison is
#'   specified, all scatterplots are graphed using different colours.
#'   
#' @details In the \pkg{plotly} version, colors can also
#' be specified as rgba colours using either the `[plotly]toRGB`
#' function or a rgba colour string, e.g. `'rgb(1, 1, 1, 1)'`.
#'   
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()],
#'          [ceplane_plot_graph()]
#' 
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' @importFrom Rdpack reprompt
#' @export
#' 
#' @examples
#' ## create the bcea object for the smoking cessation example
#' data(Smoking)
#' 
#' m <- bcea(eff, cost, ref = 4, Kmax = 500, interventions = treats)
#' 
#' ## produce the base plot
#' ceplane.plot(m, wtp = 200, graph = "base")
#' 
#' ## select only one comparator
#' ceplane.plot(m, wtp = 200, graph = "base", comparison = 3)
#' 
#' ## use ggplot2
#' if (requireNamespace("ggplot2")) {
#'    ceplane.plot(m, wtp = 200, pos = "right", icer = list(size = 2), graph = "ggplot2")
#' }
#' 
#' ## plotly
#' ceplane.plot(m, wtp = 200, graph = "plotly")
#' ceplane.plot(m, wtp = 200, comparison = 1, graph = "plotly")
#'  
ceplane.plot.bcea <- function(he,
                              comparison = NULL,
                              wtp = 25000,
                              pos = "topleft",
                              graph = c("base", "ggplot2", "plotly"),
                              ...) {
  graph <- match.arg(graph)
  
  he <- setComparisons(he, comparison)
  
  graph_params <- prep_ceplane_params(he, wtp, ...)
  
  if (is_baseplot(graph)) {
    
    ceplane_plot_base(he,
                      pos_legend = pos,
                      graph_params = graph_params)
    
  } else if (is_ggplot(graph)) {
    
    ceplane_plot_ggplot(he,
                        pos_legend = pos,
                        graph_params = graph_params, ...)
    
  } else if (is_plotly(graph)) {
    
    ceplane_plot_plotly(he,
                        wtp = wtp,
                        pos_legend = pos,
                        graph_params, ...)
  }
}


#' Cost-effectiveness Plane Plot
#' 
#' Produces a scatter plot of the cost-effectiveness plane,
#' together with the sustainability area, as a function of
#' the selected willingness to pay threshold.
#' 
#' @template args-he
#' 
#' @export
#' @aliases ceplane.plot
#' 
ceplane.plot <- function(he, ...) {
  UseMethod('ceplane.plot', he)
}

