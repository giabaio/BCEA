
# library(BCEA)
# library(testthat)


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
  
  # Compute the EVPPI for a group of parameters
  
  # GAM regression (default)
  EVPPI <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)
  EVPPI_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)
  EVPPI_voi_orig <- voi::evppi(bcea_vacc[c("e","c","k")], inputs = inp$mat, pars = c("beta.1.", "beta.2."), check = TRUE)
  
  expect_s3_class(EVPPI, "evppi")
  expect_length(EVPPI, 10)
  expect_type(EVPPI, "list")
  
  plot(EVPPI)
  plot(EVPPI_voi)
  
  # deprecated (single parameter) methods
  # Strong & Oakley
  EVPPI.so <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50)
  
  ##TODO:
  ## error: `method="so" only works for single-parameter EVPPI
  # EVPPI.so_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50)
  
  EVPPI.so_voi <- evppi_voi(bcea_vacc, "beta.1.", inp$mat, method = "so", n.blocks = 50)
  EVPPI.so_voi <- evppi_voi(bcea_vacc, "beta.2.", inp$mat, method = "so", n.blocks = 50)
  
  expect_s3_class(EVPPI.so, "evppi")
  expect_length(EVPPI.so, 6)
  expect_type(EVPPI.so, "list")
  
  # Sadatsafavi et al
  EVPPI.sad <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)
  
  # TODO: error
  # EVPPI.sal <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sal", n.seps = 1)
  
  ##TODO:
  ## error: `method="sal" only works for single-parameter EVPPI
  EVPPI.sal_voi <- evppi_voi(bcea_vacc, param_idx = c("beta.1.", "beta.2."), inp$mat, method = "sal", n.seps = 1)
  EVPPI.sad_voi <- evppi_voi(bcea_vacc, param_idx = c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)
  
  # voiEVPPI.sad <- voi::evppi(outputs = bcea_vacc[c("e","c","k")], inputs = inp$mat, pars = c("beta.1.", "beta.2."), method = "sal", n.seps = 1)
  
  EVPPI.sad_voi <- evppi_voi(bcea_vacc, "beta.2.", inp$mat, method = "sad", n.seps = 1)
  EVPPI.sal_voi <- evppi_voi(bcea_vacc, "beta.1.", inp$mat, method = "sal", n.seps = 1)
  
  expect_s3_class(EVPPI.sad, "evppi")
  expect_length(EVPPI.sad, 6)
  expect_type(EVPPI.sad, "list")
  
  # select parameters by position
  evppi_idx <- BCEA::evppi(he = bcea_vacc, param_idx = 39:40, input = inp$mat)
  evppi_idx_voi <- evppi_voi(he = bcea_vacc, param_idx = 39:40, input = inp$mat)
  
  expect_s3_class(evppi_idx, "evppi")
  expect_length(evppi_idx, 10)
  expect_type(evppi_idx, "list")
  
  ##TODO: errors
  # plot(EVPPI.so)
  # plot(EVPPI.sad)
  
  # Compute the EVPPI using INLA/SPDE
  if (require("INLA")) {
    x_inla <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla")
    
    ##TODO: error
    x_inla_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla")

    expect_s3_class(x_inla, "evppi")
    expect_length(x_inla, 10)
    expect_type(x_inla, "list")
    
    ##TODO: should we include this plot functionality in new evppi()?
    # x_inla <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", plot = TRUE)
  }
  
  # using GAM regression
  x_gam <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "GAM")
  
  # lower case method name
  x_gam_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "gam")
  
  expect_s3_class(x_gam, "evppi")
  expect_length(x_gam, 10)
  expect_type(x_gam, "list")
  
  # using Strong et al GP regression
  x_gp <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "GP")
  
  # lower case method name
  x_gp_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "gp")
  
  expect_s3_class(x_gp, "evppi")
  expect_length(x_gp, 10)
  expect_type(x_gp, "list")
  
  # subsetting input PSA simulations
  ##TODO:...
  EVPPI <- BCEA::evppi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat, N = 100)
  EVPPI_voi <- evppi_voi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat, N = 100)
  
  
  ## mesh plotting and residuals
  
  # GAM regression (default)
  # plot not produced
  EVPPI <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, plot = TRUE)
  EVPPI_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, plot = TRUE)
  EVPPI_voi_orig <- voi::evppi(bcea_vacc[c("e","c","k")], inputs = inp$mat, pars = c("beta.1.", "beta.2."), check = TRUE, plot_inla_mesh = TRUE)

  # INLA
  EVPPI_inla <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", plot = TRUE)
  EVPPI_inla_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", plot = TRUE)

  EVPPI_inla <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla")
  EVPPI_inla_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla")
                          
  EVPPI_inla_residuals <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", residuals = FALSE)
  EVPPI_inla_voi_residuals <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", residuals = FALSE)
  
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
  
  ## doesn't return error when really should
  # EVPPI <- BCEA::evppi(bcea_smoke, param_idx = c(2,3), inp$mat, h.value = 5e-7)
  
  # error
  EVPPI_voi <- evppi_voi(bcea_smoke, param_idx = c(2,3), inp$mat, h.value = 5e-7)
  voiEVPPI <- voi::evppi(bcea_smoke[c("e","c","k")], pars = c("d.3.", "d.4."), inputs = inp$mat, h.value = 5e-7)
  
  expect_s3_class(EVPPI, "evppi")
  expect_length(EVPPI, 10)
  expect_type(EVPPI, "list")
  
  # plot(EVPPI)
})


