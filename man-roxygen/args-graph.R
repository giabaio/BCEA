#' @param graph A string used to select the graphical engine to use for
#'   plotting. Should (partial-) match the three options `"base"`,
#'   `"ggplot2"` or `"plotly"`. Default value is `"base"`. This is set globally
#'   upon loading `BCEA` and can be modified for instance by using 
#'   `options("bcea.graph"="gg")`, or `options("bcea.graph="plotly")`. Partial
#'   matching still applies (so `gg` or `pl` also work). Not all plotting 
#'   functions have a `"plotly"` implementation, yet -- see help for the specifics. 
