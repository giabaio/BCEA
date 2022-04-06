
#' add_contour_quadrants
#'
#' @param params 
#'
#' @return
#' @export
#'
add_contour_quadrants <- function(params) {
  pm <- params$quadrants
  
  text(x = pm$offset * pm$M.e,
       y = pm$offset * pm$M.c,
       adj = pm$adj[[1]],
       parse(text = pm$t1),
       cex = pm$cex)
  
  text(pm$offset * pm$m.e,
       pm$offset * pm$M.c,
       adj = pm$adj[[2]],
       parse(text = pm$t2),
       cex = pm$cex)
  
  text(pm$offset * pm$m.e,
       pm$offset * pm$m.c,
       adj = pm$adj[[3]],
       parse(text = pm$t3),
       cex = pm$cex)
  
  text(pm$offset * pm$M.e,
       pm$offset * pm$m.c,
       adj = pm$adj[[4]],
       parse(text = pm$t4),
       cex = pm$cex)
}
