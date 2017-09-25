# BCEA ![Travis Build Status](https://travis-ci.org/giabaio/BCEA.png?branch=master)(https://travis-ci.org/giabaio/BCEA)[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/giabaio/BCEA?branch=master&svg=true)](https://ci.appveyor.com/project/giabaio/BCEA)[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/BCEA)](https://cran.r-project.org/package=BCEA)[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/BCEA)](https://cran.r-project.org/package=BCEA)[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/BCEA?color=orange)](	)

## Bayesian Cost-Effectiveness Analysis

Given the results of a Bayesian model (possibly based on MCMC) in the form of simulations from the posterior distributions of suitable variables of costs and clinical benefits for two or more interventions, produces a health economic evaluation. Compares one of the interventions (the "reference") to the others ("comparators"). Produces many summary and plots to analyse the results

## Installation
There are two ways of installing `BCEA`. A "stable" version (currently 2.5.5) is packaged and available from [CRAN](https://cran.r-project.org/index.html). So you can simply type on your R terminal
```R
install.packages("BCEA")
```
The second way involves using the "development" version of `BCEA` - this will usually be updated more frequently and may be continuously tested. On Windows machines, you need to install a few dependencies, including [Rtools](https://cran.r-project.org/bin/windows/Rtools/) first, e.g. by running
```R
pkgs <- c("MASS","Rtools","devtools")
repos <- c("https://cran.rstudio.com", "https://www.math.ntnu.no/inla/R/stable") 
install.packages(pkgs,repos=repos,dependencies = "Depends")
```
before installing the package using `devtools`:
```R
devtools::install_github("giabaio/BCEA")
```
Under Linux or MacOS, it is sufficient to install the package via `devtools`:
```R
install.packages("devtools")
devtools:install_github("giabaio/BCEA")
```
