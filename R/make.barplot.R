
#' Make Bar Plot for Different Graphic Devices
#' 
#' Base R or plotly versions.
#'
#' @param scores Scores
#' @param chk2 chk2
#' @param tit Main title
#' @param xlab x-axis label
#' @param xlim x-axis limits
#' @param ca The magnification to be used for axis annotation relative to the current setting of cex.
#' @param cn Size of axis labels
#' @param mai Margins
#' @param space space
#' @param howManyPars Maximum number of parameters 
#'
#' @return Bar plot
#' @seealso \code{\link{info.rank}}
#' @name make.barplot
NULL


#' @rdname make.barplot
#' @importFrom dplyr slice arrange desc
#' @export
#' 
make.barplot_base <- function(scores,
                              chk2,
                              tit,
                              xlab,
                              xlim,
                              ca,
                              cn,
                              mai,
                              space,
                              howManyPars) {
  
  col <- rep(c("blue", "red"), length(chk2))
  par(mai = mai)
  res <- data.frame(
    parameter = names(chk2),
    info = scores,
    row.names = NULL)
  
  if (requireNamespace("dplyr", quietly = FALSE)) {
    res <- dplyr::arrange(res, dplyr::desc(.data$info))
    
    if (!is.null(howManyPars) &&
        is.numeric(howManyPars) &&
        howManyPars > 0) {
      howManyPars <- min(howManyPars, nrow(res))
      res <- dplyr::slice(res, 1:howManyPars)
    }
  }
  
  barplot(
    res[order(res$info), 2],
    horiz = TRUE,
    names.arg = res[order(res$info), 1],
    cex.names = cn,
    las = 1,
    col = col,
    cex.axis = ca,
    xlab = xlab,
    space = space,
    main = tit,
    xlim = xlim)
  
  par(mai = c(1.360000, 1.093333, 1.093333, 0.560000))
  
  invisible(
    list(rank =
           data.frame(parameter = res[order(-res$info), 1],
                      info = res[order(-res$info), 2])))
}


#' @rdname make.barplot
#' @importFrom dplyr arrange desc slice
#' @import plotly
#' 
#' @export
#' 
make.barplot_plotly <- function(scores,
                                chk2,
                                tit,
                                xlab,
                                xlim,
                                ca,
                                cn,
                                mai,
                                space,
                                howManyPars) {
  
  res <-
    data.frame(
      parameter = names(chk2),
      info = scores)
  
  if (requireNamespace("dplyr", quietly = FALSE)) {
    res <- dplyr::arrange(res, dplyr::desc(.data$info))
    
    if (!is.null(howManyPars) &&
        is.numeric(howManyPars) &&
        howManyPars > 0) {
      howManyPars <- min(howManyPars, nrow(res))
      res <- dplyr::slice(res, 1:howManyPars)
    }
  }
  
  p <- 
    plotly::plot_ly(res,
                    y = ~reorder(.data$parameter,.data$info),
                    x = ~.data$info,
                    orientation = "h",
                    type = "bar",
                    marker = list(color = "royalblue"))
  
  p <- 
    plotly::layout(
      p,
      xaxis = list(hoverformat = ".2f", title = xlab, range = xlim),
      yaxis = list(hoverformat = ".2f", title = ""),
      margin = mai,
      bargap = space,
      title = tit)
  
  p$rank <-
    data.frame(parameter = res[order(-res$info), 1],
               info = res[order(-res$info), 2])
  return(p)
}

