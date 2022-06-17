
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


