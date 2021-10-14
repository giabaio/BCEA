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
# all interventions
ceef.plot(bcea_smoke)

# subset
setComparisons(bcea_smoke) <- c(1,3)
ceef.plot(bcea_smoke)

# check numbering and legend
setComparisons(bcea_smoke) <- c(3,1)
ceef.plot(bcea_smoke)

setComparisons(bcea_smoke) <- c(3,2)
ceef.plot(bcea_smoke)

setComparisons(bcea_smoke) <- 1
ceef.plot(bcea_smoke)

# add interventions back in
setComparisons(bcea_smoke) <- c(1,3)
ceef.plot(bcea_smoke)

## -----------------------------------------------------------------------------
bcea_smoke <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)

# all interventions
ceef.plot(bcea_smoke, graph = "ggplot")

# subset
setComparisons(bcea_smoke) <- c(1,3)
ceef.plot(bcea_smoke, graph = "ggplot")

# check numbering and legend
setComparisons(bcea_smoke) <- c(3,1)
ceef.plot(bcea_smoke, graph = "ggplot")

setComparisons(bcea_smoke) <- c(3,2)
ceef.plot(bcea_smoke, graph = "ggplot")

setComparisons(bcea_smoke) <- 1
ceef.plot(bcea_smoke, graph = "ggplot")

# add interventions back in
setComparisons(bcea_smoke) <- c(1,3)
ceef.plot(bcea_smoke, graph = "ggplot")

## -----------------------------------------------------------------------------
# base R
ceef.plot(bcea_smoke, pos = c(1,0))
ceef.plot(bcea_smoke, pos = c(1,1))

ceef.plot(bcea_smoke, pos = TRUE)
ceef.plot(bcea_smoke, pos = FALSE)

ceef.plot(bcea_smoke, pos = "topleft")
ceef.plot(bcea_smoke, pos = "topright")
ceef.plot(bcea_smoke, pos = "bottomleft")
ceef.plot(bcea_smoke, pos = "bottomright")

# ggplot2
ceef.plot(bcea_smoke, graph = "ggplot", pos = c(1,0))
ceef.plot(bcea_smoke, graph = "ggplot", pos = c(1,1))

ceef.plot(bcea_smoke, graph = "ggplot", pos = TRUE)
ceef.plot(bcea_smoke, graph = "ggplot", pos = FALSE)

ceef.plot(bcea_smoke, graph = "ggplot", pos = "top")
ceef.plot(bcea_smoke, graph = "ggplot", pos = "bottom")
ceef.plot(bcea_smoke, graph = "ggplot", pos = "left")
ceef.plot(bcea_smoke, graph = "ggplot", pos = "right")

## -----------------------------------------------------------------------------
ceef.plot(bcea_smoke,
          flip = TRUE,
          dominance = FALSE,
          start.from.origins = FALSE,
          print.summary = FALSE,
          graph = "base")

ceef.plot(bcea_smoke,
          dominance = TRUE,
          start.from.origins = FALSE,
          pos = TRUE,
          print.summary = FALSE,
          graph = "ggplot2")

## -----------------------------------------------------------------------------
ceef.plot(bcea_smoke,
          flip = TRUE,
          dominance = TRUE,
          start.from.origins = TRUE,
          print.summary = FALSE,
          graph = "base")

ceef.plot(bcea_smoke,
          dominance = TRUE,
          start.from.origins = TRUE,
          pos = TRUE,
          print.summary = FALSE,
          graph = "ggplot2")

## -----------------------------------------------------------------------------
data("Smoking")

c[, 4] <- -c[, 4]
bcea_smoke <- bcea(e, c, ref = 3, interventions = treats, Kmax = 500)

# all interventions
ceef.plot(bcea_smoke, graph = "ggplot")
ceef.plot(bcea_smoke, graph = "base")

ceef.plot(bcea_smoke, start.from.origins = TRUE, graph = "ggplot")
ceef.plot(bcea_smoke, start.from.origins = TRUE, graph = "base")

setComparisons(bcea_smoke) <- c(1,2)
ceef.plot(bcea_smoke, graph = "ggplot")
ceef.plot(bcea_smoke, graph = "base")

e[, 3] <- -e[, 3]
bcea_smoke <- bcea(e, c, ref = 3, interventions = treats, Kmax = 500)
ceef.plot(bcea_smoke, graph = "ggplot")
ceef.plot(bcea_smoke, graph = "base")


data("Smoking")
e[, 3] <- -e[, 3]
bcea_smoke <- bcea(e, c, ref = 3, interventions = treats, Kmax = 500)
ceef.plot(bcea_smoke, graph = "ggplot")
ceef.plot(bcea_smoke, graph = "base")

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  # create output docs
#  rmarkdown::render(input = "vignettes/ceef.Rmd", output_format = "pdf_document", output_dir = "vignettes")
#  rmarkdown::render(input = "vignettes/ceef.Rmd", output_format = "html_document", output_dir = "vignettes")

