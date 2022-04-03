
#' add_contour_quadrants
#'
#' @param params 
#'
#' @return
#' @export
#'
add_contour_quadrants <- function(params) {
  pm <- param$quadrants
  
  text(pm$offset * pm$M.e,
       pm$offset * pm$M.c,
       parse(text = pm$t1),
       cex = pm$cex,
       pos = 2)
  
  text(pm$offset * pm$m.e,
       pm$offset * pm$M.c,
       parse(text = pm$t2),
       cex = pm$cex,
       pos = 4)
  
  text(pm$offset * pm$m.e,
       parse(text = pm$t3),
       cex = pm$cex,
       pos = 4)
  
  text(pm$offset * pm$M.e,
       pm$offset * pm$m.c,
       parse(text = pm$t4),
       cex = pm$cex,
       pos = 2)
}
