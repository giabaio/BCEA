#' @param graph A string used to select the graphical engine to use for
#'   plotting. Should (partial-) match the three options `"base"`,
#'   `"ggplot2"` or `"plotly"`. Default value is `"ggplot2"`. This is set 
#'   globally upon loading `BCEA` and can be modified for instance by using 
#'   `options("bcea.graph"="base")`, or `options("bcea.graph="plotly")`. Partial
#'   matching still applies (so `gg`, or `g`, or `pl`, or `p` also work). Not 
#'   all plotting functions have a `"plotly"` implementation, yet -- see the 
#'   help for the specific functions.  
