
#' Info Rank Plot By Graph Device
#'
#' Choice of base R, ggplot2 and plotly.
#' @name info_rank_graph
#' 
NULL


#' Info rank plot base R version
#' @rdname info_rank_graph
#' 
#' @template args-he
#' @param params Graph parameters
#' @export
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

