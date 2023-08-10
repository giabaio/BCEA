
# from voi package Get started page
# https://chjackson.github.io/voi/articles/voi.html

library(voi)

nsam <- 10000

inputs <- data.frame(p1 = rnorm(nsam, mean = 1, sd = 1), 
                     p2 = rnorm(nsam, mean = 0, sd = 2))

outputs_nb <- data.frame(t1 = 0, 
                         t2 = inputs$p1 - inputs$p2)

outputs_cea <- list( 
  e = data.frame(t1 = 0, t2 = inputs$p1), 
  c = data.frame(t1 = 0, t2 = inputs$p2), 
  k = c(1, 2, 3))

xx <- evppi(outputs_nb, inputs, pars="p1")

evppi(outputs_nb, inputs, pars=c("p1","p2"))

evppi(outputs_nb, inputs, pars=list("p1","p2"))

evppi(outputs_cea, inputs, pars=list("p1",c("p1","p2")))

evppi(outputs_nb, inputs, pars="p1", method="gp", nsim=1000)

evppi(outputs_nb, inputs, pars="p1", method="earth")

evppi(outputs_nb, inputs, pars=c("p1","p2"), method="inla", pfc_struc="iso")

evppi(chemo_nb, chemo_pars, pars=colnames(chemo_pars), method="bart")

evppi(outputs_nb, inputs, pars="p1", n.blocks=20, method="so")

evppi(outputs_nb, inputs, pars="p1", method="sal")

