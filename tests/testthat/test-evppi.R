
# library(BCEA)
# library(testthat)


test_that("vaccine data", {
  
  data(Vaccine, package = "BCEA")
  treats <- c("Status quo", "Vaccination")
  
  # Run the health economic evaluation using BCEA
  bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
  
  # inputs
  inp <- createInputs(vaccine_mat)
  
  expect_length(inp, 2)
  expect_named(inp, c("mat", "parameters"))
  expect_type(inp, "list")
  
  # Compute the EVPPI for a group of parameters
  
  ###########################
  # GAM regression (default)
  
  EVPPI <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)
  EVPPI_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)
  EVPPI_voi_orig <- voi::evppi(bcea_vacc[c("e","c","k")], inputs = inp$mat, pars = c("beta.1.", "beta.2."), check = TRUE)
  
  expect_s3_class(EVPPI, "evppi")
  expect_length(EVPPI, 10)
  expect_type(EVPPI, "list")
  
  expect_equivalent(EVPPI$evppi, EVPPI_voi$evppi, tolerance = 0.001)
  expect_equivalent(EVPPI_voi_orig$evppi, EVPPI_voi$evppi, tolerance = 0.001)
  
  expect_equivalent(EVPPI$k, EVPPI_voi$k, tolerance = 0.001)
  expect_equivalent(EVPPI_voi_orig$k, EVPPI_voi$k, tolerance = 0.001)
  
  expect_equivalent(EVPPI$evi, EVPPI_voi$evi, tolerance = 0.001)
  
  ##TODO: snapshot
  # plot(EVPPI)
  # plot(EVPPI_voi)

  ##################
  # Strong & Oakley
  
  expect_error(evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50),
               regexp = "only works for single-parameter EVPPI")
  
  EVPPI.so <- BCEA::evppi(bcea_vacc, "beta.1.", inp$mat, method = "so", n.blocks = 50)
  EVPPI.so_voi <- evppi_voi(bcea_vacc, "beta.1.", inp$mat, method = "so", n.blocks = 50)
  
  expect_s3_class(EVPPI.so, "evppi")
  expect_length(EVPPI.so, 6)
  expect_type(EVPPI.so, "list")
  
  expect_equivalent(EVPPI.so$evppi, EVPPI.so_voi$evppi, tolerance = 0.001)
  expect_equivalent(EVPPI.so$k, EVPPI.so_voi$k, tolerance = 0.001)
  expect_equivalent(EVPPI.so$evi, EVPPI.so_voi$evi, tolerance = 0.001)
  
  ####################
  # Sadatsafavi et al
  
  # voi::evppi only works for single-parameter EVPPI
  EVPPI.sad <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)
  
  # TODO: error
  # EVPPI.sal <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sal", n.seps = 1)
  
  expect_error(evppi_voi(bcea_vacc, param_idx = c("beta.1.", "beta.2."), inp$mat, method = "sal", n.seps = 1),
               regexp = "only works for single-parameter EVPPI")
  
  # voiEVPPI.sad <- voi::evppi(outputs = bcea_vacc[c("e","c","k")], inputs = inp$mat,
  #                            pars = c("beta.1.", "beta.2."), method = "sal", n.seps = 1)
  
  EVPPI.sad <- BCEA::evppi(bcea_vacc, "beta.2.", inp$mat, method = "sad", n.seps = 1)
  
  EVPPI.sad_voi <- evppi_voi(bcea_vacc, "beta.2.", inp$mat, method = "sad", n.seps = 1)
  EVPPI.sal_voi <- evppi_voi(bcea_vacc, "beta.1.", inp$mat, method = "sal", n.seps = 1)
  
  expect_s3_class(EVPPI.sad, "evppi")
  expect_length(EVPPI.sad, 6)
  expect_type(EVPPI.sad, "list")
  
  expect_equivalent(EVPPI.sad$evppi, EVPPI.sad_voi$evppi, tolerance = 0.001)
  expect_equivalent(EVPPI.sad$k, EVPPI.sad_voi$k, tolerance = 0.001)
  expect_equivalent(EVPPI.sad$evi, EVPPI.sad_voi$evi, tolerance = 0.001)
  
  # select parameters by position
  evppi_idx <- BCEA::evppi(he = bcea_vacc, param_idx = 39:40, input = inp$mat)
  evppi_idx_voi <- evppi_voi(he = bcea_vacc, param_idx = 39:40, input = inp$mat)
  
  expect_s3_class(evppi_idx, "evppi")
  expect_length(evppi_idx, 10)
  expect_type(evppi_idx, "list")

  ##TODO: snapshot
  # plot(EVPPI.so)
  # plot(EVPPI.sad)
  # plot(EVPPI.so_voi)
  # plot(EVPPI.sad_voi)
  
  # Compute the EVPPI using INLA/SPDE
  if (require("INLA")) {
    x_inla <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla")
    x_inla_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla")

    expect_s3_class(x_inla, "evppi")
    expect_length(x_inla, 10)
    expect_type(x_inla, "list")
  }
  
  #############################
  # different argument formats
  
  # GAM regression
  x_gam <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "GAM")
  
  # lower case method name
  x_gam_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "gam")
  
  expect_s3_class(x_gam, "evppi")
  expect_length(x_gam, 10)
  expect_type(x_gam, "list")
  
  # Strong et al GP regression
  x_gp <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "GP")
  
  # lower case method name
  x_gp_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "gp")
  
  expect_s3_class(x_gp, "evppi")
  expect_length(x_gp, 10)
  expect_type(x_gp, "list")
  
  # subsetting input PSA simulations
  EVPPI_psa <- BCEA::evppi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat, N = 100)
  EVPPI_psa_voi <- evppi_voi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat, N = 100)
  
  ##TODO: different
  expect_equivalent(EVPPI_psa$evppi, EVPPI_psa_voi$evppi, tolerance = 0.001)
  
  expect_equivalent(EVPPI_psa$k, EVPPI_psa_voi$k, tolerance = 0.001)
  expect_equivalent(EVPPI_psa$evi, EVPPI_psa_voi$evi, tolerance = 0.001)
  
  ################
  # mesh plotting
  
  # # GAM regression (default)
  # # plot not produced
  # EVPPI <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, plot = TRUE)
  # EVPPI_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, plot = TRUE)
  # EVPPI_voi_orig <- voi::evppi(bcea_vacc[c("e","c","k")], inputs = inp$mat, pars = c("beta.1.", "beta.2."), check = TRUE, plot_inla_mesh = TRUE)
  # 
  # # INLA
  # EVPPI_inla <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", plot = TRUE)
  # EVPPI_inla_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", plot = TRUE)

  ################
  # fitted values
  
  # two parameters
  
  EVPPI_inla_residuals <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", residuals = TRUE)
  EVPPI_inla_voi_residuals <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", residuals = TRUE)
  
  expect_equivalent(EVPPI_inla_residuals$fitted.costs, EVPPI_inla_voi_residuals$fitted.costs, tolerance = 0.001)
  expect_equivalent(EVPPI_inla_residuals$fitted.effects, EVPPI_inla_voi_residuals$fitted.effects, tolerance = 0.001)
  
  EVPPI_gp_residuals <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "gp", residuals = TRUE)
  EVPPI_gp_voi_residuals <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "gp", residuals = TRUE)
  
  expect_equivalent(EVPPI_gp_residuals$fitted.costs, EVPPI_gp_voi_residuals$fitted.costs, tolerance = 0.001)
  expect_equivalent(EVPPI_gp_residuals$fitted.effects, EVPPI_gp_voi_residuals$fitted.effects, tolerance = 0.001)
  
  # no fitted values returned
  EVPPI_sad_residuals <- BCEA::evppi(he = bcea_vacc, "beta.2.", input = inp$mat, method = "sad", residuals = TRUE)
  EVPPI_sad_voi_residuals <- evppi_voi(he = bcea_vacc, "beta.2.", input = inp$mat, method = "sad", residuals = TRUE)
  
  expect_equivalent(EVPPI_sad_residuals$fitted.costs, EVPPI_sad_voi_residuals$fitted.costs, tolerance = 0.001)
  expect_equivalent(EVPPI_sad_residuals$fitted.effects, EVPPI_sad_voi_residuals$fitted.effects, tolerance = 0.001)
  
  EVPPI_gam_residuals <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "gam", residuals = TRUE)
  EVPPI_gam_voi_residuals <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "gam", residuals = TRUE)
  
  expect_equivalent(EVPPI_gam_residuals$fitted.costs, EVPPI_gam_voi_residuals$fitted.costs, tolerance = 0.001)
  expect_equivalent(EVPPI_gam_residuals$fitted.effects, EVPPI_gam_voi_residuals$fitted.effects, tolerance = 0.001)
  
  # three parameters
  
  EVPPI_inla_residuals <- BCEA::evppi(he = bcea_vacc, 39:41, input = inp$mat, method = "inla", residuals = TRUE)
  EVPPI_inla_voi_residuals <- evppi_voi(he = bcea_vacc, 39:41, input = inp$mat, method = "inla", residuals = TRUE)
  
  expect_equivalent(EVPPI_inla_residuals$fitted.costs, EVPPI_inla_voi_residuals$fitted.costs, tolerance = 0.001)
  expect_equivalent(EVPPI_inla_residuals$fitted.effects, EVPPI_inla_voi_residuals$fitted.effects, tolerance = 0.001)
  
  EVPPI_gp_residuals <- BCEA::evppi(he = bcea_vacc, 39:41, input = inp$mat, method = "gp", residuals = TRUE)
  EVPPI_gp_voi_residuals <- evppi_voi(he = bcea_vacc, 39:41, input = inp$mat, method = "gp", residuals = TRUE)
  
  expect_equivalent(EVPPI_gp_residuals$fitted.costs, EVPPI_gp_voi_residuals$fitted.costs, tolerance = 0.001)
  expect_equivalent(EVPPI_gp_residuals$fitted.effects, EVPPI_gp_voi_residuals$fitted.effects, tolerance = 0.001)
  
  EVPPI_gam_residuals <- BCEA::evppi(he = bcea_vacc, 39:41, input = inp$mat, method = "gam", residuals = TRUE)
  EVPPI_gam_voi_residuals <- evppi_voi(he = bcea_vacc, 39:41, input = inp$mat, method = "gam", residuals = TRUE)
  
  expect_equivalent(EVPPI_gam_residuals$fitted.costs, EVPPI_gam_voi_residuals$fitted.costs, tolerance = 0.001)
  expect_equivalent(EVPPI_gam_residuals$fitted.effects, EVPPI_gam_voi_residuals$fitted.effects, tolerance = 0.001)
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


