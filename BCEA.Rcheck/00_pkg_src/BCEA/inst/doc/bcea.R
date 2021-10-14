## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BCEA)

## -----------------------------------------------------------------------------
data(Smoking, package = "BCEA")

## -----------------------------------------------------------------------------
treats <- c("No intervention", "Self-help", "Individual counselling", "Group counselling")

## -----------------------------------------------------------------------------
bcea_smoke <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)

## ----fig.width=10, fig.height=10----------------------------------------------
library(ggplot2)

plot(bcea_smoke)

## -----------------------------------------------------------------------------
ceplane.plot(bcea_smoke, comparison = 2, wtp = 250)

eib.plot(bcea_smoke)

contour(bcea_smoke)

ceac.plot(bcea_smoke)

ib.plot(bcea_smoke)

