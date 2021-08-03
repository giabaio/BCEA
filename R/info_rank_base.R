
#' Info rank plot base R version
#' 
info_rank_base <- function(he, params, plot_opt) {
  
  default_params <- 
    list(ca = 0.7,   # cex.axis
         cn = 0.7,   # cex.names
         xlab = ifelse(plot_opt$rel,
                       "Proportion of total EVPI",
                       "Absolute value of the EVPPI"),
         mai = c(1.36, 1.5, 1, 1),
         tit = paste0("Info-rank plot for willingness to pay = ", plot_opt$wtp),
         space = 0.5)
  
  plot_params <-
    modifyList(default_params,
               params)
  
  plot_params <- c(plot_params,
                   list(xlim = c(0, range(plot_params$scores)[2])))
  
  do.call(make.barplot_base, plot_params)
}

