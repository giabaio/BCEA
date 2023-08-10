
# library(BCEA)


test_that("vaccine data", {
  
  data(Vaccine, package = "BCEA")
  treats <- c("Status quo", "Vaccination")
  
  # Run the health economic evaluation using BCEA
  bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
  
  # inputs
  inp <- createInputs(vaccine_mat)
  
  expect_length(inp , 2)
  expect_named(inp, c("mat", "parameters"))
  expect_type(inp, "list")
  
  # Compute the EVPPI for a bunch of parameters
  
  # GAM regression
  EVPPI <- evppi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat)
  EVPPI_voi <- evppi_voi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat)
  
  expect_s3_class(EVPPI, "evppi")
  expect_length(EVPPI, 10)
  expect_type(EVPPI, "list")
  
  # plot(EVPPI)
  
  # deprecated (single parameter) methods
  # Strong & Oakley
  EVPPI.so <- evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50)
  EVPPI.so_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50)
  
  expect_s3_class(EVPPI.so, "evppi")
  expect_length(EVPPI.so, 6)
  expect_type(EVPPI.so, "list")
  
  # Sadatsafavi et al
  EVPPI.sad <- evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)
  EVPPI.sad_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)
  
  expect_s3_class(EVPPI.sad, "evppi")
  expect_length(EVPPI.sad, 6)
  expect_type(EVPPI.sad, "list")
  
  ##TODO: errors
  # plot(EVPPI.so)
  # plot(EVPPI.sad)
  
  # Compute the EVPPI using INLA/SPDE
  if (require("INLA")) {
    x_inla <- evppi(he = bcea_vacc, 39:40, input = inp$mat)
    x_inla_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat)

    expect_s3_class(x_inla, "evppi")
    expect_length(x_inla, 10)
    expect_type(x_inla, "list")
  }
  
  # using GAM regression
  x_gam <- evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "GAM")
  x_gam_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "GAM")
  
  expect_s3_class(x_gam, "evppi")
  expect_length(x_gam, 10)
  expect_type(x_gam, "list")
  
  # using Strong et al GP regression
  x_gp <- evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "GP")
  x_gp_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "GP")
  
  expect_s3_class(x_gp, "evppi")
  expect_length(x_gp, 10)
  expect_type(x_gp, "list")
  
  # # plot results
  # if (require("INLA"))
  #   plot(x_inla)
  # points(x_inla$k, x_inla$evppi, type = "l", lwd = 2, lty = 2)
  # points(x_gam$k, x_gam$evppi, type = "l", col = "red")
  # points(x_gp$k, x_gp$evppi, type = "l", col = "blue")
  # 
  # if (require("INLA")) {
  #   plot(x_inla$k, x_inla$evppi, type = "l", lwd = 2, lty = 2)
  #   points(x_gam$k, x_gam$evppi, type = "l", col = "red")
  #   points(x_gp$k, x_gp$evppi, type = "l", col = "blue")
  # }
})


test_that("smoking data", {
  
  data(Smoking, package = "BCEA")
  treats <-
    c("No intervention", "Self-help",
      "Individual counselling", "Group counselling")
  
  bcea_smoke <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)
  
  # inputs
  inp <- createInputs(smoking_output)
  
  expect_length(inp , 2)
  expect_named(inp, c("mat", "parameters"))
  expect_type(inp, "list")
  
  EVPPI <- evppi(bcea_smoke, param_idx = c(2,3), inp$mat, h.value = 5e-7)
  EVPPI_voi <- evppi_voi(bcea_smoke, param_idx = c(2,3), inp$mat, h.value = 5e-7)
  
  expect_s3_class(EVPPI, "evppi")
  expect_length(EVPPI, 10)
  expect_type(EVPPI, "list")
  
  # plot(EVPPI)
})


