# BCEA

<!-- badges: start -->

[![Build status](https://img.shields.io/travis/giabaio/BCEA/master.svg?maxAge=0)](https://travis-ci.org/giabaio/BCEA) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/giabaio/BCEA/master.svg)](https://ci.appveyor.com/project/giabaio/BCEA) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/BCEA)](https://cran.r-project.org/package=BCEA) [![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/BCEA)](https://cran.r-project.org/package=BCEA) [![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/BCEA?color=orange)](	)
<!-- badges: end -->

## Contents

- [Overview](#introduction)
- [Features](#features)
- [Installation](#installation)
- [Further details](#further-details)

## Overview

Perform Bayesian Cost-Effectiveness Analysis in R.
Given the results of a Bayesian model (possibly based on MCMC) in the form of simulations from the posterior distributions of suitable variables of costs and clinical benefits for two or more interventions, produces a health economic evaluation. Compares one of the interventions (the "reference") to the others ("comparators").

## Features

Main features of `BCEA` include:

* Summary statistics and tables
* Cost-effectiveness analysis plots, such as CE planes and CEAC
* EVPPI calculations and plots

## Installation
There are two ways of installing `BCEA`. A "stable" version (currently 2.2.6) is packaged and available from [CRAN](https://cran.r-project.org/index.html). So you can simply type on your R terminal

```r
install.packages("BCEA")
```
The second way involves using the "development" version of `BCEA` - this will usually be updated more frequently and may be continuously tested. On Windows machines, you need to install a few dependencies, including [Rtools](https://cran.r-project.org/bin/windows/Rtools/) first, e.g. by running

```r
pkgs <- c("MASS", "Rtools", "devtools")
repos <- c("https://cran.rstudio.com", "https://www.math.ntnu.no/inla/R/stable") 
install.packages(pkgs, repos = repos, dependencies = "Depends")
```
before installing the package using `devtools`:

```r
devtools::install_github("giabaio/BCEA")
```
Under Linux or MacOS, it is sufficient to install the package via `devtools`:

```r
install.packages("devtools")
devtools:install_github("giabaio/BCEA")
```

## Further details
More details on `BCEA` are available in our book [_Bayesian Cost-Effectiveness Analysis with the R Package BCEA_](http://www.statistica.it/gianluca/book/bcea/) (published in the UseR! Springer series). Also, details about the package, including some references and links to a pdf presentation and some posts on my own blog) are given [here](http://www.statistica.it/gianluca/software/bcea/).

## Licence
GPL-3 © [G Baio](https://github.com/giabaio).
