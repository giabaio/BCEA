
#' Info rank plot base R version
#' 
info_rank_base <- function(he, params) {
  
  default_params <- 
    list(ca = 0.7,   # cex.axis
         cn = 0.7,   # cex.names
         mai = c(1.36, 1.5, 1, 1),
         space = 0.5)
  
  plot_params <-
    modifyList(default_params,
               params)
  
  do.call(make.barplot_base, plot_params)
}

