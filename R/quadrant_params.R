
#' Quadrant Parameters
#' requires just a single comparison group
#' @keywords internal aplot
#' 
quadrant_params <- function(he, params) {
  
  p.ne <-
    sum(he$delta_e > 0 &
          he$delta_c > 0) / he$n_sim
  p.nw <-
    sum(he$delta_e <= 0 &
          he$delta_c > 0) / he$n_sim
  p.sw <-
    sum(he$delta_e <= 0 &
          he$delta_c <= 0) / he$n_sim
  p.se <-
    sum(he$delta_e > 0 &
          he$delta_c <= 0) / he$n_sim
  
  list(
    cex = 0.8,
    offset = 1.0,
    adj = list(c(1,1), c(0,1),c(0,0), c(1,0)),
    p.ne = p.ne,
    p.nw = p.nw,
    p.sw = p.sw,
    p.se = p.se,
    m.e = params$xlim[1],
    M.e = params$xlim[2],
    m.c = params$ylim[1],
    M.c = params$ylim[2],
    t1 = paste("Pr(Delta[e]>0, Delta[c]>0)==",
               format(p.ne, digits = 4, nsmall = 3),
               sep = ""),
    t2 = paste("Pr(Delta[e]<=0, Delta[c]>0)==",
               format(p.nw, digits = 4, nsmall = 3),
               sep = ""),
    t3 = paste("Pr(Delta[e]<=0, Delta[c]<=0)==",
               format(p.sw, digits = 4, nsmall = 3),
               sep = ""),
    t4 = paste("Pr(Delta[e]>0, Delta[c]<=0)==",
               format(p.se, digits = 4, nsmall = 3),
               sep = ""))
}
