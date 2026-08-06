test_that("multi-intervention data and multi.ce / ceaf.plot run cleanly", {
  data(Vaccine)
  he <- bcea(eff, cost, interventions = treats, ref = 2)
  
  expect_s3_class(he, "bcea")
  expect_error(summary(he), NA)
  expect_error(ceplane.plot(he, graph = "base"), NA)
  expect_error(ceplane.plot(he, graph = "ggplot"), NA)
  expect_error(ceac.plot(he, graph = "base"), NA)
  expect_error(ceac.plot(he, graph = "ggplot"), NA)
  
  he_multi <- multi.ce(he)
  expect_s3_class(he_multi, "pairwise")
  expect_error(summary(he_multi), NA)
  expect_error(ceaf.plot(he_multi, graph = "base"), NA)
  expect_error(ceaf.plot(he_multi, graph = "ggplot"), NA)
})

test_that("bcea setters work properly", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(setReferenceGroup(he) <- 1, NA)
  expect_error(setKmax(he) <- 100000, NA)
})
