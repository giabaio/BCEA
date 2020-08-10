
# library(BCEA)

# data(Vaccine, package = "BCEA")
# he <- bcea(e = e, c = c, interventions = treats, ref = 2)
# save(he, file = "tests/testthat/bcea_vaccine.RData")
load("bcea_vaccine.RData")

test_that("summary.bcea", {

  testthat::local_edition(3)
  expect_snapshot_output(summary(he))
})