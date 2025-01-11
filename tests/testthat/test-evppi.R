# 
# # mainly regression tests against old version of BCEA::evppi()
# # from version <= 2.4.5
# # versus new evppi() calling voi::evppi internally
# 
# 
# # library(BCEA)
# if (interactive()) library(testthat)
# 
# if (!requireNamespace("voi", quietly = TRUE)) {
#   stop(
#     "Package \"voi (>= 1.0.1)\" must be installed to use this function.",
#     call. = FALSE
#   )
# }
# 
# test_that("GAM regression (default) with vaccine data", {
#   
#   data(Vaccine, package = "BCEA")
#   treats <- c("Status quo", "Vaccination")
#   
#   # Run the health economic evaluation using BCEA
#   bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#   
#   inp <- createInputs(vaccine_mat, print_is_linear_comb = FALSE)
#   
#   # expect_length(inp, 2)
#   # expect_named(inp, c("mat", "parameters"))
#   # expect_type(inp, "list")
#   
#   # EVPPI <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)
#   # save(EVPPI, file = "tests/testthat/testdata/EVPPI_GAM_default.RData")
#   load(file = test_path("testdata", "EVPPI_GAM_default.RData"))
#   
#   EVPPI_voi <- evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)
#   
#   EVPPI_voi_orig <- voi::evppi(bcea_vacc[c("e","c","k")],
#                                inputs = inp$mat,
#                                pars = c("beta.1.", "beta.2."),
#                                check = TRUE)
#   
#   expect_s3_class(EVPPI, "evppi")
#   expect_length(EVPPI, 10)
#   expect_type(EVPPI, "list")
#   
#   expect_s3_class(EVPPI_voi, "evppi")
#   expect_type(EVPPI_voi, "list")
#   
#   expect_equivalent(EVPPI$evppi, EVPPI_voi$evppi, tolerance = 0.001)
#   expect_equivalent(EVPPI_voi_orig$evppi, EVPPI_voi$evppi, tolerance = 0.001)
#   
#   expect_equivalent(EVPPI$k, EVPPI_voi$k, tolerance = 0.001)
#   expect_equivalent(EVPPI_voi_orig$k, EVPPI_voi$k, tolerance = 0.001)
#   
#   expect_equivalent(EVPPI$evi, EVPPI_voi$evi, tolerance = 0.001)
#   expect_equivalent(EVPPI$index, EVPPI_voi$index)
#   expect_equivalent(EVPPI$fitted.costs, EVPPI_voi$fitted.costs, tolerance = 0.001)
#   expect_equivalent(EVPPI$fitted.effects, EVPPI_voi$fitted.effects, tolerance = 0.001)
#   expect_equivalent(EVPPI$select, EVPPI_voi$select)
#   
#   ##TODO: snapshot
#   # plot(EVPPI)
#   # plot(EVPPI_voi)
#   
#   rm(EVPPI)
# })
# 
# test_that("Strong & Oakley with vaccine data", {
#   
#   data(Vaccine, package = "BCEA")
#   treats <- c("Status quo", "Vaccination")
#   
#   bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#   
#   inp <- createInputs(vaccine_mat, print_is_linear_comb = FALSE)
#   
#   expect_error(evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50),
#                regexp = "only works for single-parameter EVPPI")
#   
#   # EVPPI.so <- BCEA::evppi(bcea_vacc, "beta.1.", inp$mat, method = "so", n.blocks = 50)
#   # save(EVPPI.so, file = "tests/testthat/testdata/EVPPI_so_default.RData")
#   load(file = test_path("testdata", "EVPPI_so_default.RData"))
#   
#   EVPPI.so_voi <- evppi(bcea_vacc, "beta.1.", inp$mat, method = "so", n.blocks = 50)
#   
#   expect_s3_class(EVPPI.so, "evppi")
#   expect_length(EVPPI.so, 6)
#   expect_type(EVPPI.so, "list")
#   
#   expect_s3_class(EVPPI.so_voi, "evppi")
#   expect_type(EVPPI.so_voi, "list")
#   
#   expect_equivalent(EVPPI.so$evppi, EVPPI.so_voi$evppi, tolerance = 0.001)
#   expect_equivalent(EVPPI.so$k, EVPPI.so_voi$k, tolerance = 0.001)
#   expect_equivalent(EVPPI.so$evi, EVPPI.so_voi$evi, tolerance = 0.001)
#   expect_equivalent(EVPPI.so$index, EVPPI.so_voi$index)
#   
#   rm(EVPPI.so)
# })
# 
# test_that("Sadatsafavi et al with vaccine data", {
#   
#   data(Vaccine, package = "BCEA")
#   treats <- c("Status quo", "Vaccination")
#   
#   # Run the health economic evaluation using BCEA
#   bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#   
#   inp <- createInputs(vaccine_mat, print_is_linear_comb = FALSE)
#   
#   # voi::evppi only works for single-parameter EVPPI
#   
#   # EVPPI.sad <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)
#   # save(EVPPI.sad, file = "tests/testthat/testdata/EVPPI_sad_default.RData")
#   # load(file = test_path("testdata", "EVPPI_sad_default.RData"))
#   
#   # TODO: error
#   # EVPPI.sal <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, method = "sal", n.seps = 1)
#   
#   expect_error(evppi(bcea_vacc, param_idx = c("beta.1.", "beta.2."), inp$mat, method = "sal", n.seps = 1),
#                regexp = "only works for single-parameter EVPPI")
#   
#   # voiEVPPI.sad <- voi::evppi(outputs = bcea_vacc[c("e","c","k")], inputs = inp$mat,
#   #                            pars = c("beta.1.", "beta.2."), method = "sal", n.seps = 1)
#   
#   # EVPPI.sad <- BCEA::evppi(bcea_vacc, "beta.2.", inp$mat, method = "sad", n.seps = 1)
#   # save(EVPPI.sad, file = "tests/testthat/testdata/EVPPI_sad_default.RData")
#   load(file = test_path("testdata", "EVPPI_sad_default.RData"))
#   
#   EVPPI.sad_voi <- evppi(bcea_vacc, "beta.2.", inp$mat, method = "sad", n.seps = 1)
#   EVPPI.sal_voi <- evppi(bcea_vacc, "beta.1.", inp$mat, method = "sal", n.seps = 1)
#   
#   expect_s3_class(EVPPI.sad, "evppi")
#   expect_length(EVPPI.sad, 6)
#   expect_type(EVPPI.sad, "list")
#   
#   expect_s3_class(EVPPI.sad_voi, "evppi")
#   expect_type(EVPPI.sad_voi, "list")
#   
#   expect_equivalent(EVPPI.sad$evppi, EVPPI.sad_voi$evppi, tolerance = 0.001)
#   expect_equivalent(EVPPI.sad$k, EVPPI.sad_voi$k, tolerance = 0.001)
#   expect_equivalent(EVPPI.sad$evi, EVPPI.sad_voi$evi, tolerance = 0.001)
#   expect_equivalent(EVPPI.sad$index, EVPPI.sad_voi$index)
#   
#   ##TODO: snapshot
#   # plot(EVPPI.so)
#   # plot(EVPPI.sad)
#   # plot(EVPPI.so_voi)
#   # plot(EVPPI.sad_voi)
#   
#   rm(EVPPI.sad)
# })
# 
# test_that("Select parameters by position with vaccine data", {
#   
#   data(Vaccine, package = "BCEA")
#   treats <- c("Status quo", "Vaccination")
#   
#   # Run the health economic evaluation using BCEA
#   bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#   
#   inp <- createInputs(vaccine_mat, print_is_linear_comb = FALSE)
#   
#   # evppi_idx <- BCEA::evppi(he = bcea_vacc, param_idx = 39:40, input = inp$mat)
#   # save(evppi_idx, file = "tests/testthat/testdata/EVPPI_idx.RData")
#   load(file = test_path("testdata", "EVPPI_idx.RData"))
#   
#   evppi_idx_voi <- evppi(he = bcea_vacc, param_idx = 39:40, input = inp$mat)
#   
#   expect_s3_class(evppi_idx, "evppi")
#   expect_length(evppi_idx, 10)
#   expect_type(evppi_idx, "list")
#   
#   expect_s3_class(evppi_idx_voi, "evppi")
#   expect_type(evppi_idx_voi, "list")
# })
# 
# test_that("INLA/SPDE with vaccine data", {
#   
#   data(Vaccine, package = "BCEA")
#   treats <- c("Status quo", "Vaccination")
#   
#   # Run the health economic evaluation using BCEA
#   bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#   
#   inp <- createInputs(vaccine_mat, print_is_linear_comb = FALSE)
#   
#   skip_if_not_installed("INLA")
#   skip("INLA is crashing")
#   
#   if (require("INLA")) {
#     # EVPPI_inla <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla")
#     # save(EVPPI_inla, file = "tests/testthat/testdata/EVPPI_inla_default.RData")
#     load(file = test_path("testdata", "EVPPI_inla_default.RData"))
#     
#     EVPPI_inla_voi <- evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla")
#     
#     expect_s3_class(EVPPI_inla, "evppi")
#     expect_length(EVPPI_inla, 10)
#     expect_type(EVPPI_inla, "list")
#     
#     expect_s3_class(EVPPI_inla_voi, "evppi")
#     expect_type(EVPPI_inla_voi, "list")
#   }
# })
# 
# test_that("Different argument formats with vaccine data", {
#   
#   data(Vaccine, package = "BCEA")
#   treats <- c("Status quo", "Vaccination")
#   
#   # Run the health economic evaluation using BCEA
#   bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#   
#   inp <- createInputs(vaccine_mat, print_is_linear_comb = FALSE)
#   
#   # GAM regression
#   # EVPPI_gam <- BCEA::evppi(he = bcea_vacc, param_idx = 39:40, input = inp$mat, method = "GAM")
#   # save(EVPPI_gam, file = "tests/testthat/testdata/EVPPI_gam.RData")
#   load(file = test_path("testdata", "EVPPI_gam.RData"))
#   
#   # lower case method name
#   EVPPI_gam_voi <- evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "gam")
#   
#   expect_s3_class(EVPPI_gam_voi, "evppi")
#   expect_type(EVPPI_gam_voi, "list")
#   expect_equivalent(EVPPI_gam$k, EVPPI_gam_voi$k, tolerance = 0.001)
#   expect_equivalent(EVPPI_gam$select, EVPPI_gam_voi$select)
#   
#   # Strong et al GP regression
#   
#   # EVPPI_gp <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "GP")
#   # save(EVPPI_gp, file = "tests/testthat/testdata/EVPPI_gp.RData")
#   load(file = test_path("testdata", "EVPPI_gp.RData"))
#   
#   # lower case method name
#   EVPPI_gp_voi <- evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "gp")
#   
#   expect_s3_class(EVPPI_gp_voi, "evppi")
#   expect_type(EVPPI_gp_voi, "list")
#   
#   # subsetting input PSA simulations
#   
#   # set.seed(1234)
#   # EVPPI_psa <- BCEA::evppi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat, N = 100)
#   # save(EVPPI_psa, file = "tests/testthat/testdata/EVPPI_psa.RData")
#   load(file = test_path("testdata", "EVPPI_psa.RData"))
#   
#   set.seed(1234)
#   EVPPI_psa_voi <- evppi(bcea_vacc, c("beta.1." , "beta.2."), inp$mat, N = 100)
#   
#   expect_equivalent(EVPPI_psa$select, EVPPI_psa_voi$select)
#   expect_equivalent(EVPPI_psa$evppi, EVPPI_psa_voi$evppi, tolerance = 0.001)
#   
#   expect_equivalent(EVPPI_psa$k, EVPPI_psa_voi$k, tolerance = 0.001)
#   expect_equivalent(EVPPI_psa$evi, EVPPI_psa_voi$evi, tolerance = 0.001)
#   expect_equivalent(EVPPI_psa$index, EVPPI_psa_voi$index)
# })
# 
# test_that("Mesh plotting with vaccine data", {
#   
#   skip("plot = TRUE mesh plot to be snapshot")
#   
#   data(Vaccine, package = "BCEA")
#   treats <- c("Status quo", "Vaccination")
#   
#   # Run the health economic evaluation using BCEA
#   bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#   
#   inp <- createInputs(vaccine_mat, print_is_linear_comb = FALSE)
#   
#   # # GAM regression (default)
#   # # plot not produced
#   # EVPPI <- BCEA::evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, plot = TRUE)
#   # EVPPI_voi <- evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat, plot = TRUE)
#   # EVPPI_voi_orig <- voi::evppi(bcea_vacc[c("e","c","k")], inputs = inp$mat, pars = c("beta.1.", "beta.2."), check = TRUE, plot_inla_mesh = TRUE)
#   # 
#   # # INLA
#   # EVPPI_inla <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", plot = TRUE)
#   # EVPPI_inla_voi <- evppi_voi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", plot = TRUE)
# })
# 
# test_that("Fitted values with vaccine data two parameters", {
#   
#   data(Vaccine, package = "BCEA")
#   treats <- c("Status quo", "Vaccination")
#   
#   # Run the health economic evaluation using BCEA
#   bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#   
#   inp <- createInputs(vaccine_mat, print_is_linear_comb = FALSE)
#   
#   # GP
#   # set.seed(1234)
#   # EVPPI_gp_residuals <-
#   #   BCEA::evppi(
#   #     he = bcea_vacc,
#   #     param_idx = 39:40,
#   #     input = inp$mat,
#   #     method = "gp",
#   #     residuals = TRUE)
#   # save(EVPPI_gp_residuals, file = "tests/testthat/testdata/EVPPI_gp_residuals.RData")
#   load(file = test_path("testdata", "EVPPI_gp_residuals.RData"))
#   
#   set.seed(1234)
#   EVPPI_gp_voi_residuals <-
#     evppi(
#       he = bcea_vacc,
#       param_idx = 39:40,
#       input = inp$mat,
#       method = "gp",
#       residuals = TRUE)
#   
#   expect_equivalent(
#     EVPPI_gp_residuals$select,
#     EVPPI_gp_voi_residuals$select)
#   
#   expect_equivalent(
#     EVPPI_gp_residuals$fitted.costs,
#     EVPPI_gp_voi_residuals$fitted.costs,
#     tolerance = 0.1)
#   
#   expect_equivalent(
#     EVPPI_gp_residuals$fitted.effects,
#     EVPPI_gp_voi_residuals$fitted.effects,
#     tolerance = 0.1)
#   
#   # SAD
#   # set.seed(1234)
#   # EVPPI_sad_residuals <-
#   #   BCEA::evppi(
#   #     he = bcea_vacc,
#   #     param_idx = "beta.2.",
#   #     input = inp$mat,
#   #     method = "sad",
#   #     residuals = TRUE)
#   # save(EVPPI_sad_residuals, file = "tests/testthat/testdata/EVPPI_sad_residuals.RData")
#   load(file = test_path("testdata", "EVPPI_sad_residuals.RData"))
#   
#   set.seed(1234)
#   EVPPI_sad_voi_residuals <-
#     evppi(
#       he = bcea_vacc,
#       param_idx = "beta.2.",
#       input = inp$mat,
#       method = "sad",
#       residuals = TRUE)
#   
#   # no fitted values returned
#   expect_null(EVPPI_sad_residuals$fitted.costs)
#   expect_null(EVPPI_sad_residuals$fitted.effects)
#   
#   expect_null(EVPPI_sad_voi_residuals$fitted.costs)
#   expect_null(EVPPI_sad_voi_residuals$fitted.effects)
#   
#   # GAM
#   # set.seed(1234)
#   # EVPPI_gam_residuals <-
#   #   BCEA::evppi(
#   #     he = bcea_vacc,
#   #     param_idx = 39:40,
#   #     input = inp$mat,
#   #     method = "gam",
#   #     residuals = TRUE)
#   # save(EVPPI_gam_residuals, file = "tests/testthat/testdata/EVPPI_gam_residuals.RData")
#   load(file = test_path("testdata", "EVPPI_gam_residuals.RData"))
#   
#   set.seed(1234)
#   EVPPI_gam_voi_residuals <-
#     evppi(
#       he = bcea_vacc,
#       param_idx = 39:40,
#       input = inp$mat,
#       method = "gam",
#       residuals = TRUE)
#   
#   expect_equivalent(
#     EVPPI_gam_residuals$fitted.costs,
#     EVPPI_gam_voi_residuals$fitted.costs,
#     tolerance = 0.001)
#   
#   expect_equivalent(
#     EVPPI_gam_residuals$fitted.effects,
#     EVPPI_gam_voi_residuals$fitted.effects,
#     tolerance = 0.001)
#   
#   skip_if_not_installed("INLA")
#   skip("INLA is crashing")
#   
#   if (require("INLA")) {
#     # set.seed(1234)
#     # EVPPI_inla_residuals <- BCEA::evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", residuals = TRUE)
#     # save(EVPPI_inla_residuals, file = "tests/testthat/testdata/EVPPI_inla_residuals.RData")
#     load(file = test_path("testdata", "EVPPI_inla_residuals.RData"))
#     
#     set.seed(1234)
#     EVPPI_inla_voi_residuals <- evppi(he = bcea_vacc, 39:40, input = inp$mat, method = "inla", residuals = TRUE)
#     
#     expect_equivalent(
#       EVPPI_inla_residuals$select,
#       EVPPI_inla_voi_residuals$select)
#     
#     expect_equivalent(
#       EVPPI_inla_residuals$fitted.costs,
#       EVPPI_inla_voi_residuals$fitted.costs,
#       tolerance = 0.1)
#     
#     expect_equivalent(
#       EVPPI_inla_residuals$fitted.effects,
#       EVPPI_inla_voi_residuals$fitted.effects,
#       tolerance = 0.1)
#   }
# })
# 
# test_that("Fitted values with vaccine data three parameters", {
#   
#   data(Vaccine, package = "BCEA")
#   treats <- c("Status quo", "Vaccination")
#   
#   # Run the health economic evaluation using BCEA
#   bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#   
#   inp <- createInputs(vaccine_mat, print_is_linear_comb = FALSE)
#   
#   # GP
#   # set.seed(1234)
#   # EVPPI_gp_3_residuals <-
#   #   BCEA::evppi(
#   #     he = bcea_vacc,
#   #     param_idx = 39:41,
#   #     input = inp$mat,
#   #     method = "gp",
#   #     residuals = TRUE)
#   # save(EVPPI_gp_3_residuals, file = "tests/testthat/testdata/EVPPI_gp_3_residuals.RData")
#   load(file = test_path("testdata", "EVPPI_gp_3_residuals.RData"))
#   
#   set.seed(1234)
#   EVPPI_gp_voi_residuals <-
#     evppi(
#       he = bcea_vacc,
#       param_idx = 39:41,
#       input = inp$mat,
#       method = "gp",
#       residuals = TRUE)
#   
#   expect_equivalent(
#     EVPPI_gp_3_residuals$fitted.costs,
#     EVPPI_gp_voi_residuals$fitted.costs,
#     tolerance = 0.1)
#   
#   expect_equivalent(
#     EVPPI_gp_3_residuals$fitted.effects,
#     EVPPI_gp_voi_residuals$fitted.effects,
#     tolerance = 0.1)
#   
#   # GAM
#   # set.seed(1234)
#   # EVPPI_gam_3_residuals <-
#   #   BCEA::evppi(
#   #     he = bcea_vacc,
#   #     param_idx = 39:41,
#   #     input = inp$mat,
#   #     method = "gam",
#   #     residuals = TRUE)
#   # save(EVPPI_gam_3_residuals, file = "tests/testthat/testdata/EVPPI_gam_3_residuals.RData")
#   load(file = test_path("testdata", "EVPPI_gam_3_residuals.RData"))
#   
#   set.seed(1234)
#   EVPPI_gam_voi_residuals <-
#     evppi(
#       he = bcea_vacc,
#       param_idx = 39:41,
#       input = inp$mat,
#       method = "gam",
#       residuals = TRUE)
#   
#   expect_equivalent(
#     EVPPI_gam_3_residuals$fitted.costs,
#     EVPPI_gam_voi_residuals$fitted.costs,
#     tolerance = 0.1)
#   
#   expect_equivalent(
#     EVPPI_gam_3_residuals$fitted.effects,
#     EVPPI_gam_voi_residuals$fitted.effects,
#     tolerance = 0.1)
#   
#   skip_if_not_installed("INLA")
#   skip("INLA is crashing")
#   
#   if (require("INLA")) {
#     # set.seed(1234)
#     # EVPPI_inla_3_residuals <-
#     #   BCEA::evppi(
#     #     he = bcea_vacc,
#     #     param_idx = 39:41,
#     #     input = inp$mat,
#     #     method = "inla",
#     #     residuals = TRUE)
#     # save(EVPPI_inla_3_residuals, file = "tests/testthat/testdata/EVPPI_inla_3_residuals.RData")
#     load(file = test_path("testdata", "EVPPI_inla_3_residuals.RData"))
#     
#     set.seed(1234)
#     EVPPI_inla_voi_residuals <-
#       evppi(
#         he = bcea_vacc,
#         param_idx = 39:41,
#         input = inp$mat,
#         method = "inla",
#         residuals = TRUE)
#     
#     expect_equivalent(
#       EVPPI_inla_3_residuals$fitted.costs,
#       EVPPI_inla_voi_residuals$fitted.costs,
#       tolerance = 0.1)
#     
#     expect_equivalent(
#       EVPPI_inla_3_residuals$fitted.effects,
#       EVPPI_inla_voi_residuals$fitted.effects,
#       tolerance = 0.1)
#   }
# })
# 
# test_that("More that two interventions with smoking data", {
#   
#   skip("more than two interventions to be revisited after {voi} update")
#   
#   data(Smoking, package = "BCEA")
#   treats <-
#     c("No intervention", "Self-help",
#       "Individual counselling", "Group counselling")
#   
#   inp <- createInputs(smoking_output, print_is_linear_comb = FALSE)
#   
#   bcea_smoke <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)  # all interventions
#   
#   # bcea_smoke <- bcea(eff, cost, ref = 4, .comparison = 1, interventions = treats, Kmax = 500)
#   # bcea_smoke <- bcea(eff, cost, ref = 4, .comparison = c(2,3), interventions = treats, Kmax = 500)
#   
#   # expect_length(inp , 2)
#   # expect_named(inp, c("mat", "parameters"))
#   # expect_type(inp, "list")
#   
#   set.seed(1234)
#   # EVPPI_smoke <- BCEA::evppi(bcea_smoke, param_idx = c(2,3), inp$mat, h.value = 5e-7, method = "gam")
#   # save(EVPPI_smoke, file = "tests/testthat/testdata/EVPPI_smoke.RData")
#   # load(file = test_path("testdata", "EVPPI_smoke.RData"))
#   
#   set.seed(1234)
#   EVPPI_voi <- evppi(bcea_smoke, param_idx = c(2,3), inp$mat, h.value = 5e-7, method = "gam")
#   
#   # voiEVPPI <- voi::evppi(bcea_smoke[c("e","c","k")], pars = c("d.3.", "d.4."), inputs = inp$mat, h.value = 5e-7)
#   
#   expect_s3_class(EVPPI_voi, "evppi")
#   expect_type(EVPPI_voi, "list")
#   
#   ##TODO: error
#   expect_equivalent(EVPPI_smoke$evppi, EVPPI_voi$evppi, tolerance = 0.01)
#   
#   expect_equivalent(EVPPI_smoke$k, EVPPI_voi$k, tolerance = 0.001)
#   expect_equivalent(EVPPI_smoke$k, EVPPI_voi$k, tolerance = 0.001)
#   
#   expect_equivalent(EVPPI_smoke$evi, EVPPI_voi$evi, tolerance = 0.001)
#   expect_equivalent(EVPPI_smoke$select, EVPPI_voi$select)
#   expect_equivalent(EVPPI_smoke$index, EVPPI_voi$index)
#   
#   ##TODO: error
#   ##TODO: seems like the wrong order of columns?
#   ## what is the correct order? label columns?
#   expect_equivalent(EVPPI_smoke$fitted.costs, EVPPI_voi$fitted.costs, tolerance = 0.001)
#   expect_equivalent(EVPPI_smoke$fitted.effects, EVPPI_voi$fitted.effects, tolerance = 0.001)
#   
#   # plot(EVPPI_voi)
# })
# 
# 
