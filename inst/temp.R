
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

# evppi examples

evppi(outputs_nb, inputs, pars="p1")

evppi(outputs_nb, inputs, pars=c("p1","p2"))

evppi(outputs_nb, inputs, pars=list("p1","p2"))

evppi(outputs_cea, inputs, pars=list("p1",c("p1","p2")))

evppi(outputs_nb, inputs, pars="p1", method="gp", nsim=1000)

evppi(outputs_nb, inputs, pars="p1", method="earth")

evppi(outputs_nb, inputs, pars=c("p1","p2"), method="inla", pfc_struc="iso")

evppi(chemo_nb, chemo_pars, pars=colnames(chemo_pars), method="bart")

evppi(outputs_nb, inputs, pars="p1", n.blocks=20, method="so")

evppi(outputs_nb, inputs, pars="p1", method="sal")


#######
# BCEA

data(Vaccine, package = "BCEA")
treats <- c("Status quo", "Vaccination")

# Run the health economic evaluation using BCEA
bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)

inp <- createInputs(vaccine_mat)
evppi_bcea <- 
  BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)

evppi_bcea2 <- as.data.frame(evppi_bcea[c("parameters", "k", "evppi")])

evppi_voi <- 
  voi::evppi(outputs = bcea_vacc[c("e","c","k")],
             inputs = vaccine_mat,
             pars = c("beta.1." , "beta.2."))#,
             # check = TRUE)                 ##TODO: error


evppi_voi.bcea <- function(he, param_idx, input, ...) {
  pars <- param_idx
  inputs <- input
  outputs <- he[c("e","c","k")]
  
  voi::evppi(outputs, inputs, pars, ...)
}

