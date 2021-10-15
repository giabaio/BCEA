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
data(Smoking)

treats <- c("No intervention", "Self-help", "Individual counselling", "Group counselling")
bcea_smoke <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)

## -----------------------------------------------------------------------------
r <- c(0, 0.005, 0.020, 0.035)
CEriskav(bcea_smoke) <- r

plot(bcea_smoke)
plot(bcea_smoke, graph = "ggplot")

## -----------------------------------------------------------------------------
setComparisons(bcea_smoke) <- c(1,3)

## -----------------------------------------------------------------------------
CEriskav(bcea_smoke) <- r

plot(bcea_smoke)
plot(bcea_smoke, graph = "ggplot")

## -----------------------------------------------------------------------------
r <- 0
CEriskav(bcea_smoke) <- r

plot(bcea_smoke)

plot(bcea_smoke, graph = "ggplot")

bcea_smoke0 <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)
eib.plot(bcea_smoke0, comparison = 1)
evi.plot(bcea_smoke0)

## -----------------------------------------------------------------------------
# negative
r <- -0.005
CEriskav(bcea_smoke) <- r
plot(bcea_smoke)

# large
r <- 2
CEriskav(bcea_smoke) <- r
plot(bcea_smoke)

## -----------------------------------------------------------------------------
setComparisons(bcea_smoke) <- c(3,1)

## -----------------------------------------------------------------------------
r <- c(0, 0.005, 0.020, 0.035)
CEriskav(bcea_smoke) <- r

plot(bcea_smoke)
plot(bcea_smoke, graph = "ggplot")

## -----------------------------------------------------------------------------
# base R
plot(bcea_smoke, pos = c(1,0))
plot(bcea_smoke, pos = c(1,1))

plot(bcea_smoke, pos = TRUE)
plot(bcea_smoke, pos = FALSE)

plot(bcea_smoke, pos = "topleft")
plot(bcea_smoke, pos = "topright")
plot(bcea_smoke, pos = "bottomleft")
plot(bcea_smoke, pos = "bottomright")

# ggplot2
plot(bcea_smoke, graph = "ggplot", pos = c(1,0))
plot(bcea_smoke, graph = "ggplot", pos = c(1,1))

plot(bcea_smoke, graph = "ggplot", pos = TRUE)
plot(bcea_smoke, graph = "ggplot", pos = FALSE)

plot(bcea_smoke, graph = "ggplot", pos = "top")
plot(bcea_smoke, graph = "ggplot", pos = "bottom")
plot(bcea_smoke, graph = "ggplot", pos = "left")
plot(bcea_smoke, graph = "ggplot", pos = "right")

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  # create output docs
#  rmarkdown::render(input = "vignettes/ceac.Rmd", output_format = "pdf_document", output_dir = "vignettes")
#  rmarkdown::render(input = "vignettes/ceac.Rmd", output_format = "html_document", output_dir = "vignettes")

