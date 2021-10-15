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

## ----fig.height=15, fig.width=8-----------------------------------------------
data("Vaccine")
m <- bcea(e,c)
inp <- createInputs(vaccine, print_is_linear_comb = FALSE)
info.rank(m, inp)
info.rank(m, inp, graph = "base")
info.rank(m, inp, graph = "plotly")

## ----fig.height=5, fig.width=8------------------------------------------------
info.rank(m, inp, graph = "base", howManyPars = 10)
info.rank(m, inp, graph = "plotly", howManyPars = 10)

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  # create output docs
#  # rmarkdown::render(input = "vignettes/info-rank_plot.Rmd", output_format = "pdf_document", output_dir = "vignettes")
#  # rmarkdown::render(input = "vignettes/info-rank_plot.Rmd", output_format = "html_document", output_dir = "vignettes")

