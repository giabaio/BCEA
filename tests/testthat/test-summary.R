
# library(BCEA)

##TODO:
# data(Vaccine, package = "BCEA")
# he <- bcea(eff = e, cost = c, interventions = treats, ref = 2)
# 
# test_that("vaccine data", {
#   
#   testthat::local_edition(3)
#   expect_snapshot_output(summary(he))
# })

# test_that("smoking data", {
#   load("bcea_smoking.RData")
#   
#   testthat::local_edition(3)
#   expect_snapshot_output(summary(he))
# })


test_that("eib in summary print is same as bcea", {
  
  load(test_path("ce_vaccine.RData"))
  
  vacc_bcea <- bcea(eff, cost, ref = 1, interventions = c("1", "2"))
  
  capture_wtp <- capture.output(summary(vacc_bcea, wtp = 20000))
  
  expect_equal(tolerance = 0.0001,
               as.double(strsplit(capture_wtp[17], split = " ")[[1]][4]),
               vacc_bcea$eib[201])
  
  capture_wtp <- capture.output(summary(vacc_bcea, wtp = 30000))
  
  expect_equal(tolerance = 0.0001,
               as.double(strsplit(capture_wtp[17], split = " ")[[1]][4]),
               vacc_bcea$eib[301])
})


test_that("subset of interventions included in comp and ref", {
  
  load(test_path("ce_smoking.RData"))

  # just dont error
  smoke_bcea <- bcea(eff, cost, ref = 1)
  expect_output(summary(smoke_bcea),
                regexp = "Cost-effectiveness analysis summary")
  
  smoke_bcea <- bcea(eff, cost, ref = 1, .comparison = c(2,4))
  expect_output(summary(smoke_bcea),
                regexp = "Cost-effectiveness analysis summary")

  smoke_bcea <- bcea(eff, cost, ref = 1, .comparison = 3)
  expect_output(summary(smoke_bcea),
                regexp = "Cost-effectiveness analysis summary")
  
  smoke_bcea <- bcea(eff, cost, ref = 1, .comparison = c(2,4), interventions = c("a", "b", "c", "d"))
  expect_output(summary(smoke_bcea),
                regexp = "Cost-effectiveness analysis summary")
})

