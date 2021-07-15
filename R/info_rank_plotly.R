
#' info_rank_plotly
#' 
info_rank_plotly <- function(extra_args,
                             scores,
                             chk2,
                             wtp,
                             howManyPars) {
  ca <- NULL
  if (exists("ca", where = extra_args)) {
    warning("Argument ca was specified in info.rank.plotly but is not an accepted argument.
                Parameter will be ignored.")}
  cn <- NULL
  if (exists("cn", where = extra_args)) {
    warning("Argument cn was specified in info.rank.plotly but is not an accepted argument.
                Parameter will be ignored.")}
  xlab <- "Proportion of total EVPI"
  if (exists("rel", where = extra_args)) {
    if (!extra_args$rel) {
      scores <- unlist(lapply(x, function(x) x$evppi))
      xlab <- "Absolute value of the EVPPI"
    }
  }
  
  xlim <- 
    if (exists("xlim", where = extra_args)) {
      extra_args$xlim
    } else {NULL}
  
  mai <- 
    if (exists("mai", where = extra_args)) {
      extra_args$mai
    } else {NULL}
  
  tit <-
    if (exists("tit", where = extra_args)) {
      extra_args$tit
    } else {
      paste0("Info-rank plot for willingness to pay = ", wtp)}
  
  space <- 
    if (exists("space", where = extra_args)) {
      extra_args$space
    } else {NULL}
  
  make.barplot_plotly(
    scores = scores,
    chk2 = chk2,
    tit = tit,
    xlab = xlab,
    xlim = xlim,
    ca,
    cn,
    mai = mai,
    space,
    howManyPars) 
}

