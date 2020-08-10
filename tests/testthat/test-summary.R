
library(BCEA)

test_that("summary.bcea", {
  
  data(Vaccine)
  he <- bcea(e,c, interventions = treats, ref = 2)

  testthat::local_edition(3)
  expect_snapshot_output(summary(he))
})