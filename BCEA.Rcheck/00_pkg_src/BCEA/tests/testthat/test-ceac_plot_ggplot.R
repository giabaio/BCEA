context("ceac_plot")

# vdiffr::manage_cases(filter = "ceac")

library(ggplot2)
library(dplyr)
library(reshape2)
library(purrr)
library(vdiffr)

load("ce.RData")
he <- BCEA::bcea(eff, cost)

test_that("ceac.plot_ggplot draws correctly", {
  
  ceac_plot <- ceac.plot(he, graph = "ggplot2", title = "my title")
  # vdiffr::expect_doppelganger(title = "ceac plot ggplot", fig = ceac_plot)
})
