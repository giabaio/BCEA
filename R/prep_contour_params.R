
#
prep_contour_params <- function(he,
                                xlab,
                                ylab,
                                title,
                                comparison,
                                pos,
                                scale,
                                nlevels,
                                levels,
                                extra_ags) {
  
  params <- list()
  
  params$xlab <- 
    if (!exists("xlab", where = extra_args)) {
      "Effectiveness differential"
    } else {
      extra_args$xlab
    }
  
  params$ylab <- 
    if (!exists("ylab", where = extra_args)) {
      "Cost differential"
    } else {
      extra_args$ylab
    }
  
  params$title <- 
    if (!exists("title", where = extra_args)) {
      paste(
        "Cost effectiveness plane contour plot\n",
        he$interventions[he$ref],
        " vs ",
        he$interventions[comparison],
        sep = "")
    } else {
      extra_args$title
    }
  
  axes_lim <- xy_params(he, wtp = 100000, params)
  
  params$pos_legend <- pos
  params$xlim <- axes_lim$xlim
  params$ylim <- axes_lim$ylim
  params$scale <- scale
  params$nlevels <- nlevels
  params$levels <- levels
  params$point$color <- "grey"
  params$point$size <- 1
  params$comparison <- comparison
  
  params
}
