
#' info_rank_plotly
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
  
  default_params <- 
    list(mai = c(1.36, 1.5, 1, 1),
         space = 0.5,
         ca = NULL,
         cn = NULL)
  
  plot_params <- 
    modifyList(params, default_params)
  
  do.call(make.barplot_plotly, plot_params)
}

