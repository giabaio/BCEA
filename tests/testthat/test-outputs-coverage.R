test_that("sim_table, ce_table, and summary functions run without error", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(sim_table(he), NA)
  expect_error(ce_table(he), NA)
  expect_error(summary(he), NA)
  expect_output(print(he))
})

test_that("CEriskav setter runs without error", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(CEriskav(he) <- 0.001, NA)
})
