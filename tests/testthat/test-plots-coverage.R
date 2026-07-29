test_that("ceplane.plot runs without error across graph engines", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(ceplane.plot(he, graph = "base"), NA)
  expect_error(ceplane.plot(he, graph = "ggplot"), NA)
  expect_error(ceplane.plot(he, graph = "plotly"), NA)
  expect_error(ceplane.plot(he, wtp = 20000, graph = "ggplot"), NA)
})

test_that("ceac.plot runs without error across graph engines", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(ceac.plot(he, graph = "base"), NA)
  expect_error(ceac.plot(he, graph = "ggplot"), NA)
  expect_error(ceac.plot(he, graph = "plotly"), NA)
})

test_that("eib.plot runs without error across graph engines", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(eib.plot(he, graph = "base"), NA)
  expect_error(eib.plot(he, graph = "ggplot"), NA)
  expect_error(eib.plot(he, graph = "plotly"), NA)
})

test_that("evi.plot runs without error across graph engines", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(evi.plot(he, graph = "base"), NA)
  expect_error(evi.plot(he, graph = "ggplot"), NA)
})

test_that("contour runs without error across graph engines", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(contour(he, graph = "base"), NA)
  expect_error(contour(he, graph = "ggplot"), NA)
  expect_error(contour(he, graph = "plotly"), NA)
  expect_error(contour2(he, graph = "base"), NA)
  expect_error(contour2(he, graph = "ggplot"), NA)
  expect_error(contour2(he, graph = "plotly"), NA)
})

test_that("ib.plot runs without error across graph engines", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(ib.plot(he, graph = "base"), NA)
  expect_error(ib.plot(he, graph = "ggplot"), NA)
})

test_that("ceef.plot runs without error across graph engines", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(ceef.plot(he, graph = "base"), NA)
  expect_error(ceef.plot(he, graph = "ggplot"), NA)
})

test_that("mce.plot runs without error across graph engines", {
  c_tmp <- matrix(c(0, 0, 100, 10, 50, 60), nrow = 2)
  e_tmp <- matrix(c(0, 0, 1, -2, 3, 4), nrow = 2)
  he3 <- bcea(e = e_tmp, c = c_tmp, plot = FALSE)
  
  expect_error(suppressWarnings(mce.plot(he3, graph = "base")), NA)
})

test_that("generic plot.bcea runs without error for various types", {
  load(test_path("testdata", "ce.RData"))
  he <- bcea(e = eff, c = cost, plot = FALSE)
  
  expect_error(plot(he, type = "ceplane"), NA)
  expect_error(plot(he, type = "eib"), NA)
  expect_error(plot(he, type = "ceac"), NA)
  expect_error(plot(he, type = "evi"), NA)
})
