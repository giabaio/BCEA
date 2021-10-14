## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 6
)

## ----setup, results='hide', message=FALSE, warning=FALSE, echo=FALSE----------
library(BCEA)
library(dplyr)
library(reshape2)
library(ggplot2)
library(purrr)

## -----------------------------------------------------------------------------
data(Vaccine)

he <-
  bcea(e, c,                  
       ref = 2,                
       interventions = treats, 
       Kmax = 50000,
       plot = FALSE)

## -----------------------------------------------------------------------------
eib.plot(he)

## -----------------------------------------------------------------------------
eib.plot(he, graph = "base")
eib.plot(he, graph = "ggplot2")
# ceac.plot(he, graph = "plotly")

## -----------------------------------------------------------------------------
eib.plot(he,
         graph = "ggplot2",
         main = "my title",
         line = list(colors = "green"),
         theme = theme_dark())

## -----------------------------------------------------------------------------
eib.plot(he, plot.cri = TRUE)

## -----------------------------------------------------------------------------
data(Smoking)

treats <- c("No intervention", "Self-help",
            "Individual counselling", "Group counselling")
he <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)

eib.plot(he)

## -----------------------------------------------------------------------------
eib.plot(he,
         graph = "base",
         main = "my title",
         line = list(colors = "green"))

## -----------------------------------------------------------------------------
eib.plot(he,
         graph = "ggplot2",
         main = "my title",
         line = list(colors = "green"))

## -----------------------------------------------------------------------------
eib.plot(he, plot.cri = TRUE)

## -----------------------------------------------------------------------------
eib.plot(he, pos = FALSE) # bottom right
eib.plot(he, pos = c(0, 0))
eib.plot(he, pos = c(0, 1))
eib.plot(he, pos = c(1, 0))
eib.plot(he, pos = c(1, 1))

## -----------------------------------------------------------------------------
##TODO:
eib.plot(he, graph = "ggplot2", pos = c(0, 0))
eib.plot(he, graph = "ggplot2", pos = c(0, 1))
eib.plot(he, graph = "ggplot2", pos = c(1, 0))
eib.plot(he, graph = "ggplot2", pos = c(1, 1))

## -----------------------------------------------------------------------------
mypalette <- RColorBrewer::brewer.pal(3, "Accent")

eib.plot(he,
         graph = "base",
         line = list(colors = mypalette))

eib.plot(he,
         graph = "ggplot2",
         line = list(colors = mypalette))

## ----echo=FALSE---------------------------------------------------------------
# create output docs
# rmarkdown::render(input = "vignettes/eib.Rmd", output_format = "pdf_document", output_dir = "vignettes")
# rmarkdown::render(input = "vignettes/eib.Rmd", output_format = "html_document", output_dir = "vignettes")

