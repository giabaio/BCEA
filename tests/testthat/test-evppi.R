
library(BCEA)

test_that("basic", {
})


###############
# vaccine data

data(Vaccine, package = "BCEA")
treats <- c("Status quo", "Vaccination")

# Run the health economic evaluation using BCEA
m <- bcea(e.pts, c.pts, ref = 2, interventions = treats)

# Compute the EVPPI for a bunch of parameters
inp <- createInputs(vaccine_mat)

# GAM regression
EVPPI <- evppi(m, c("beta.1." , "beta.2."), inp$mat)

plot(EVPPI)

# deprecated (single parameter) methods
# Strong & Oakley
EVPPI.so <- evppi(m, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50)
# Sadatsafavi et al
EVPPI.sad <- evppi(m, c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)

##TODO: errors
# plot(EVPPI.so)
# plot(EVPPI.sad)

# Compute the EVPPI using INLA/SPDE
if (require("INLA"))
  x_inla <- evppi(he = m, 39:40, input = inp$mat)

# using GAM regression
x_gam <- evppi(he = m, 39:40, input = inp$mat, method = "GAM")

# using Strong et al GP regression
x_gp <- evppi(he = m, 39:40, input = inp$mat, method = "GP")

# plot results
if (require("INLA"))
  plot(x_inla)
points(x_inla$k, x_inla$evppi, type = "l", lwd = 2, lty = 2)
points(x_gam$k, x_gam$evppi, type = "l", col = "red")
points(x_gp$k, x_gp$evppi, type = "l", col = "blue")

if (require("INLA")) {
  plot(x_inla$k, x_inla$evppi, type = "l", lwd = 2, lty = 2)
  points(x_gam$k, x_gam$evppi, type = "l", col = "red")
  points(x_gp$k, x_gp$evppi, type = "l", col = "blue")
}

###############
# smoking data

data(Smoking, package = "BCEA")
treats <-
  c("No intervention", "Self-help",
    "Individual counselling", "Group counselling")

m <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)

##TODO: there is no smoking_output. what should we use?...
inp <- createInputs(smoking_output)

EVPPI <- evppi(m, param_idx = c(2,3), inp$mat, h.value = 5e-7)

plot(EVPPI)


