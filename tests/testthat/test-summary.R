
# library(BCEA)

# data(Vaccine, package = "BCEA")
# he <- bcea(e = e, c = c, interventions = treats, ref = 2)
# save(he, file = "tests/testthat/bcea_vaccine.RData")

test_that("vaccine data", {
  load("bcea_vaccine.RData")
  
  testthat::local_edition(3)
  expect_snapshot_output(summary(he))
})

# test_that("smoking data", {
#   load("bcea_smoking.RData")
#   
#   testthat::local_edition(3)
#   expect_snapshot_output(summary(he))
# })

