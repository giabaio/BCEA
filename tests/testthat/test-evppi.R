
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
  EVPPI <- BCEA::evppi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat)
  EVPPI_voi <- evppi_voi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat)
  
  expect_s3_class(EVPPI, "evppi")
  expect_length(EVPPI, 10)
  expect_type(EVPPI, "list")
  
  # plot(EVPPI)
  
  # deprecated (single parameter) methods
  # Strong & Oakley
  EVPPI.so <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50)
  
  ##TODO: this in only available for single parameter
  ## error
  EVPPI.so_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "so")
  
  EVPPI.so_voi <- evppi_voi(bcea_vacc, "beta.1.", inp$mat, method = "so", n.blocks = 50)
  EVPPI.so_voi <- evppi_voi(bcea_vacc, "beta.2.", inp$mat, method = "so", n.blocks = 50)
  
  expect_s3_class(EVPPI.so, "evppi")
  expect_length(EVPPI.so, 6)
  expect_type(EVPPI.so, "list")
  
  # Sadatsafavi et al
  EVPPI.sad <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)
  
  ##TODO: this in only available for single parameter
  ## error
  EVPPI.sad_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sal", n.seps = 1)
  
  EVPPI.sad_voi <- evppi_voi(bcea_vacc, "beta.1.", inp$mat, method = "sal", n.seps = 1)
  EVPPI.sad_voi <- evppi_voi(bcea_vacc, "beta.2.", inp$mat, method = "sal", n.seps = 1)
  
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
    x_inla <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat)
    x_inla_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat)

    expect_s3_class(x_inla, "evppi")
    expect_length(x_inla, 10)
    expect_type(x_inla, "list")
  }
  
  # using GAM regression
  x_gam <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "GAM")
  x_gam_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "GAM")
  
  expect_s3_class(x_gam, "evppi")
  expect_length(x_gam, 10)
  expect_type(x_gam, "list")
  
  # using Strong et al GP regression
  x_gp <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "GP")
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
  
  EVPPI <- BCEA::evppi(bcea_smoke, param_idx = c(2,3), inp$mat, h.value = 5e-7)
  EVPPI_voi <- evppi_voi(bcea_smoke, param_idx = c(2,3), inp$mat, h.value = 5e-7)
  
  expect_s3_class(EVPPI, "evppi")
  expect_length(EVPPI, 10)
  expect_type(EVPPI, "list")
  
  # plot(EVPPI)
})


