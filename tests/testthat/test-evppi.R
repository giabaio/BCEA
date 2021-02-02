
library(BCEA)

test_that("basic", {
})


# # 1. Runs the full Bayesian model and then produces the overall economic evaluation
# source("Utils.R")	# loads some useful functions
# source("runModel.R")	# runs the full model
# data("Vaccine")
# 
# # using the 2-stage MCMC approach
# source("EVPPI_2stage.R")
# 
# par.int <- beta[, 1]		# defines the parameter of interest
# 
# phi.name <-
#   "beta.sim"		# the name of the parameter of interest in the data list
# 
# n.out <-
#   1000			# number of outer simulations (to compute the mean wrt phi)
# 
# ec.vars <-
#   "ComputeVars.R"	# file where the computations for e,c are stored
# 
# bcea.call <- "bcea(e=e.pts, c=c.pts, ref=2, interventions=treats)"
# 
# ev <-
#   evppi2stage(phi = par.int,
#                phi.name,
#                n.out = n.out,
#                m,
#                data = dataJags,
#                model.file = filein,
#                params = params,
#                inits = NULL,
#                n.iter = 20000,
#                n.burnin = 9500,
#                n.thin = NULL,
#                n.mc = 1000,
#                working.dir = NULL,
#                ec.vars,
#                bcea.call)
# 
# # 3. Now computes the EVPPI using numerical approximation
# 
# source("evppi.R")
# 
# f <- numeric()		# will use to store the values of EVPI for each k
# nb <- m$U		# extracts the net benefits from the full economic model
# evppi.graphics <- FALSE
# 
# for (k in 1:length(m$k)) {
#   nbs <-
#     nb[, k, ]	# this is a n.sims x n.interventions matrix with the NB for each value of k
#   
#   f[k] <-
#     evppi(par.int, nbs, c(2, 1, 1))$evppi	# EVPPI for each value of k
# }
# 
# 
# plot(m$k,
#      ev$evppi,
#      t = "l",
#      lty = 2,
#      col = "blue",
#      # 1. the 2-stages MCMC EVPPI
#      ylim = range(ev$evppi, f, m$evi))
# 
# points(m$k, f, t = "l", col = "red")			# 2. the numerical-approximated EVPPI
# points(m$k, m$evi, t = "l")				# 3. the full EVPI (for reference)
