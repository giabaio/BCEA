## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
fig.width = 6,
fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(BCEA)

## ----eval=FALSE---------------------------------------------------------------
#  bcea(e,
#       c,
#       ref = 1,
#       interventions = NULL,
#       .comparison = NULL,
#       Kmax = 50000,
#       wtp = NULL,
#       plot = FALSE)

## -----------------------------------------------------------------------------
data(Vaccine)

## -----------------------------------------------------------------------------
he_ref1 <- bcea(e, c,
                ref = 1,           
                interventions = treats,
                Kmax = 50000)  
str(he_ref1)

ceplane.plot(he_ref1)

## -----------------------------------------------------------------------------
he_ref2 <- bcea(e, c,
                ref = 2,           
                interventions = treats,
                Kmax = 50000)  

str(he_ref2[c("n_comparators", "ICER", "ref", "comp")])

## -----------------------------------------------------------------------------
setReferenceGroup(he_ref1) <- 2

str(he_ref1[c("n_comparators", "ICER", "ref", "comp")])

## -----------------------------------------------------------------------------
he_Kmax1 <- bcea(e, c,
                 ref = 1,           
                 interventions = treats,
                 Kmax = 50000)  

str(he_Kmax1[c("n_comparators", "ICER", "ref", "comp", "Kmax")])

## -----------------------------------------------------------------------------
he_Kmax2 <- bcea(e, c,
                 ref = 2,           
                 interventions = treats,
                 Kmax = 2000)  

str(he_Kmax2[c("n_comparators", "ICER", "ref", "comp", "Kmax")])

## -----------------------------------------------------------------------------
setKmax(he_Kmax1) <- 2000

str(he_Kmax1[c("n_comparators", "ICER", "ref", "comp", "Kmax")])

## -----------------------------------------------------------------------------
data(Smoking)

## -----------------------------------------------------------------------------
he_comp234 <- bcea(e, c,
                   ref = 1,           
                   interventions = treats,
                   Kmax = 50000)  

str(he_comp234[c("n_comparators", "ICER", "ref", "comp")])

ceplane.plot(he_comp234, wtp = 2000)

## -----------------------------------------------------------------------------
he_comp2 <- bcea(e, c,
                 ref = 1,
                 .comparison = 2,
                 interventions = treats,
                 Kmax = 2000)  

str(he_comp2[c("n_comparators", "ICER", "ref", "comp")])

ceplane.plot(he_comp2, wtp = 2000)

## -----------------------------------------------------------------------------
setComparisons(he_comp234) <- 2

str(he_comp234[c("n_comparators", "ICER", "ref", "comp")])

ceplane.plot(he_comp234, wtp = 2000)

## -----------------------------------------------------------------------------
he_comp24 <- bcea(e, c,
                  ref = 1,
                  .comparison = c(2,4),
                  interventions = treats,
                  Kmax = 2000)  
str(he_comp24[c("n_comparators", "ICER", "ref", "comp")])

ceplane.plot(he_comp24, wtp = 2000)

## -----------------------------------------------------------------------------
setComparisons(he_comp234) <- c(2,4)

str(he_comp234[c("n_comparators", "ICER", "ref", "comp")])

ceplane.plot(he_comp24, wtp = 2000)

## -----------------------------------------------------------------------------
ceplane.plot(he_comp234, comparison = 2, wtp = 2000)
ceplane.plot(he_comp234, comparison = c(2,4), wtp = 2000)

## ----echo=FALSE---------------------------------------------------------------
# create output docs
# rmarkdown::render(input = "vignettes/Set_bcea_parameters.Rmd", output_format = "pdf_document", output_dir = "vignettes")
# rmarkdown::render(input = "vignettes/Set_bcea_parameters.Rmd", output_format = "html_document", output_dir = "vignettes")

