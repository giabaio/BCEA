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

## ----eval = FALSE, echo=FALSE-------------------------------------------------
#  # create output docs
#  rmarkdown::render(input = "vignettes/R_journal_examples.Rmd", output_format = "latex_document", output_dir = "vignettes")
#  # rmarkdown::render(input = "vignettes/ceac.Rmd", output_format = "html_document", output_dir = "vignettes")

## -----------------------------------------------------------------------------
data(Smoking, package = "BCEA")

treats <- c("No intervention", "Self-help", "Individual counselling", "Group counselling")
bcea_smoke <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)

## ----fig.width=10, fig.height=10----------------------------------------------
library(ggplot2)

plot(bcea_smoke)

plot(bcea_smoke, graph = "ggplot2", wtp = 250, pos = TRUE, size = rel(2), ICER.size = 2)

## -----------------------------------------------------------------------------
ceplane.plot(bcea_smoke, comparison = 2, wtp = 250)

setComparisons(bcea_smoke) <- c(1,3)

ceplane.plot(bcea_smoke, wtp = 250, graph = "ggplot2")

## -----------------------------------------------------------------------------
eib.plot(bcea_smoke)

contour(bcea_smoke)

ceac.plot(bcea_smoke)

ib.plot(bcea_smoke)

ceef.plot(bcea_smoke)

## -----------------------------------------------------------------------------
bcea_smoke <- multi.ce(bcea_smoke)

ceac.plot(bcea_smoke, pos = "topright")
ceaf.plot(bcea_smoke)

## -----------------------------------------------------------------------------
mixedAn(bcea_smoke) <- c(0.4, 0.3, 0.2, 0.1)
summary(bcea_smoke, wtp = 250)
evi.plot(bcea_smoke, graph = "ggplot", pos = "b")

## -----------------------------------------------------------------------------
r <- c(0, 0.005, 0.020, 0.035)
CEriskav(bcea_smoke) <- r

plot(bcea_smoke)

## -----------------------------------------------------------------------------
data(Vaccine)

treats <- c("Status quo", "Vaccination")
bcea_vacc <- bcea(e, c, ref = 2, interventions = treats)

## ----fig.width=10, fig.height=10----------------------------------------------
plot(bcea_vacc)

## -----------------------------------------------------------------------------
summary(bcea_vacc, wtp = 10000)
head(sim_table(bcea_vacc, wtp = 25000)$Table)

## -----------------------------------------------------------------------------
evi.plot(bcea_vacc)

inp <- createInputs(vaccine, print_is_linear_comb = FALSE)

## -----------------------------------------------------------------------------
info.rank(bcea_vacc, inp)

## -----------------------------------------------------------------------------
EVPPI <- evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)
plot(EVPPI)

