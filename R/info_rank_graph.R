
#' Info Rank Plot By Graph Device
#'
#' Choice of base R, \pkg{ggplot2} and \pkg{plotly}.
#' @name info_rank_graph
#' 
NULL


#' Info rank plot base R version
#' @rdname info_rank_graph
#' 
#' @template args-he
#' @param params Graph parameters
#' 
info_rank_base <- function(he, params) {
  
  res <- params$res
  
  default_params <- 
    list(ca = 0.7,   # cex.axis
         cn = 0.7,   # cex.names
         mai = c(1.36, 1.5, 1, 1),
         space = 0.5)
  
  plot_params <-
    modifyList(default_params,
               params)
  
  par_default <- par(no.readonly = TRUE)
  on.exit(par(par_default))
  
  col <- rep(c("blue", "red"), length(plot_params$chk2))
  
  par(mai = plot_params$mai)
  
  do.call(barplot,
          list(height = res$info,
               horiz = TRUE,
               names.arg = res$parameter,
               cex.names = plot_params$cn,
               las = 1,
               col = col,
               cex.axis = plot_params$ca,
               xlab = plot_params$xlab,
               space = plot_params$space,
               main = plot_params$tit,
               xlim = plot_params$xlim))
  
  decrease_order <- order(res$info, decreasing = TRUE)
  
  invisible(
    list(
      rank =
        data.frame(
          parameter = res$parameter[decrease_order],
          info = res$info[decrease_order])))
}



#' Info rank plot ggplot2 version
#' @rdname info_rank_graph
#' 
#' @template args-he
#' @param params Graph parameters
#' @importFrom stats reorder
#' 
info_rank_ggplot <- function(he, params) {
  data.frame(
    params$res,
    col = as.factor(rep(c("red", "blue"),            # alternate fill by row number
                        length.out = nrow(params$res)))) |> 
  ggplot(aes(x = reorder(.data$parameter, .data$info), y = .data$info)) +
    geom_bar(stat = "identity", aes(fill = col)) +  
    coord_flip() +
    theme_default() +
    ylab(params$xlab) +
    xlab("") +
    ggtitle(params$tit) +
    theme(
      plot.title = element_text(margin = margin(b = 20)), # adjust margin below title
      legend.position = "none")
}




#' Info rank plot plotly version
#' @rdname info_rank_graph
#' 
#' @param params Graph Parameters including data
#' @export
#' @importFrom cli cli_alert_warning
#' 
info_rank_plotly <- function(params) {
  
  if (exists("ca", where = params)) {
    cli::cli_alert_warning(
      "Argument {.var ca} was specified in {.fn info.rank.plotly} but is not an accepted argument.
       Parameter will be ignored.")
  }
  
  if (exists("cn", where = params)) {
    cli::cli_alert_warning(
      "Argument {.var cn} was specified in {.fn info.rank.plotly} but is not an accepted argument.
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

