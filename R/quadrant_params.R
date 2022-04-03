
#
quadrant_params <- function(he, params) {
  
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
  
  list(
    cex = 0.8,
    offset = 0, #1.0,
    m.e = params$setup$xlim[1],
    M.e = params$setup$xlim[2],
    m.c = params$setup$ylim[1],
    M.c = params$setup$ylim[2],
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
