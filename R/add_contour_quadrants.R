
#' add_contour_quadrants
#'
#' @param he 
#' @param params 
#'
#' @return
#' @export
#'
add_contour_quadrants <- function(he, params) {
  
  offset <- 1.0
  
  p.ne <-
    sum(he$delta_e[, he$comp] > 0 &
          he$delta_c[, he$comp] > 0) / he$n_sim
  p.nw <-
    sum(he$delta_e[, he$comp] <= 0 &
          he$delta_c[, he$comp] > 0) / he$n_sim
  p.sw <-
    sum(he$delta_e[, he$comp] <= 0 &
          he$delta_c[, he$comp] <= 0) / he$n_sim
  p.se <-
    sum(he$delta_e[, he$comp] > 0 &
          he$delta_c[, he$comp] <= 0) / he$n_sim
  
  m.e <- params$setup$xlim[1]
  M.e <- params$setup$xlim[2]
  m.c <- params$setup$ylim[1]
  M.c <- params$setup$ylim[2]
  
  cex <- 0.8
  
  t1 <-
    paste("Pr(Delta[e]>0, Delta[c]>0)==",
          format(p.ne, digits = 4, nsmall = 3),
          sep = "")
  
  text(offset * M.e,
       offset * M.c,
       parse(text = t1),
       cex = cex,
       pos = 2)
  
  t2 <-
    paste("Pr(Delta[e]<=0, Delta[c]>0)==",
          format(p.nw, digits = 4, nsmall = 3),
          sep = "")
  
  text(offset * m.e,
       offset * M.c,
       parse(text = t2),
       cex = cex,
       pos = 4)
  
  t3 <-
    paste("Pr(Delta[e]<=0, Delta[c]<=0)==",
          format(p.sw, digits = 4, nsmall = 3),
          sep = "")
  
  text(offset * m.e,
       
       offset * m.c,
       parse(text = t3),
       cex = cex,
       pos = 4)
  
  t4 <-
    paste("Pr(Delta[e]>0, Delta[c]<=0)==",
          format(p.se, digits = 4, nsmall = 3),
          sep = "")
  
  text(offset * M.e,
       offset * m.c,
       parse(text = t4),
       cex = cex,
       pos = 2)
}
