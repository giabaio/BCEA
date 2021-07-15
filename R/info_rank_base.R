
#' info_rank_base
#' 
info_rank_base <- function(extra_args,
                           scores,
                           chk2,
                           wtp,
                           howManyPars){
  ca <- 
    if (exists("ca", where = extra_args)) {
      extra_args$ca
    } else {0.7}
  
  cn <- 
    if (exists("cn", where = extra_args)) {
      extra_args$cn
    } else {0.7}
  
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
    } else {c(0, range(scores)[2])}
  
  mai <- 
    if (exists("mai", where = extra_args)) {
      extra_args$mai
    } else {c(1.36, 1.5, 1, 1)}
  
  tit <- 
    if (exists("tit", where = extra_args)) {
      extra_args$tit
    } else {
      paste0("Info-rank plot for willingness to pay = ", wtp)
    }
  
  space <- 
    if (exists("space", where = extra_args)) {
      extra_args$space
    } else {0.5}
  
  make.barplot_base(
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

