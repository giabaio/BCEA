
# library(ggplot2)
# library(dplyr)
# library(reshape2)
# library(purrr)
# library(vdiffr)


test_that("eib.plot_ggplot draws correctly", {
  load(test_path("testdata", "ce.RData"))
  
  he <- BCEA::bcea(eff, cost)
  
  eib_plot <- eib.plot(he, graph = "ggplot2", title = "my title")
  # vdiffr::expect_doppelganger(title = "eib plot ggplot", fig = eib_plot)
})
