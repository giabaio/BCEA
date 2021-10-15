## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
fig.width = 6
)

## ----setup, results='hide', message=FALSE, warning=FALSE, echo=FALSE----------
library(BCEA)
library(dplyr)
library(reshape2)
library(ggplot2)
library(purrr)

## -----------------------------------------------------------------------------
data("Smoking")
he <- bcea(e, c, ref = 4, Kmax = 500)

## ----fig.height=10------------------------------------------------------------
par(mfrow = c(2,1))
ceac.plot(he)
abline(h = 0.5, lty = 2)
abline(v = c(160, 225), lty = 3)
eib.plot(he, plot.cri = FALSE)

## -----------------------------------------------------------------------------
he.multi <- multi.ce(he)

## ----fig.height=10------------------------------------------------------------
par(mfrow = c(2, 1))
ceac.plot(he.multi)
abline(h = 0.5, lty = 2)
abline(v = c(160, 225), lty = 3)
eib.plot(he, plot.cri = FALSE)

