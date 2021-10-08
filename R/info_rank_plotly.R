
#' Info rank plot plotly version
#' @rdname info_rank_graph
#' 
#' @param params Graph Parameters including data
#' @importFrom plotly plot_ly layout
#' @export
#' 
info_rank_plotly <- function(params) {
  
  if (exists("ca", where = params)) {
    warning("Argument ca was specified in info.rank.plotly but is not an accepted argument.
                Parameter will be ignored.")
  }

  if (exists("cn", where = params)) {
    warning("Argument cn was specified in info.rank.plotly but is not an accepted argument.
                Parameter will be ignored.")
  }
  
  res <- params$res
  
  default_params <- 
    list(mai = c(1.36, 1.5, 1, 1),
         space = 0.5,
         ca = NULL,
         cn = NULL)
  
  plot_params <- 
    modifyList(params, default_params)
  
  p <- 
    plotly::plot_ly(
      data = res,
      y = ~reorder(.data$parameter, .data$info),
      x = ~.data$info,
      orientation = "h",
      type = "bar",
      marker = list(color = "royalblue"))
  
  p <- 
    plotly::layout(
      p,
      xaxis = list(hoverformat = ".2f",
                   title = plot_params$xlab,
                   range = plot_params$xlim),
      yaxis = list(hoverformat = ".2f",
                   title = ""),
      margin = plot_params$mai,
      bargap = plot_params$space,
      title = plot_params$tit)
  
  decrease_order <- order(res$info, decreasing = TRUE)
  
  p$rank <-
    data.frame(parameter = res$parameter[decrease_order],
               info = res$info[decrease_order])
  return(p)
}

