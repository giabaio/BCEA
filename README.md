# BCEA <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->
[![Build Status](https://app.travis-ci.com/n8thangreen/BCEA.svg?branch=dev)](https://app.travis-ci.com/n8thangreen/BCEA)
[![R-CMD-check](https://github.com/n8thangreen/BCEA/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/n8thangreen/BCEA/actions/workflows/check-standard.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/BCEA)](https://cran.r-project.org/package=BCEA)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/BCEA)](https://cran.r-project.org/package=BCEA)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/grand-total/BCEA?color=orange)](https://cran.r-project.org/package=BCEA)
[![CodeFactor](https://www.codefactor.io/repository/github/n8thangreen/bcea/badge)](https://www.codefactor.io/repository/github/n8thangreen/bcea)
<!-- badges: end -->

> Perform Bayesian Cost-Effectiveness Analysis in R.

:rocket: **Version 2.4.1 out now!** [Check out the release notes here](https://github.com/n8thangreen/BCEA/releases/tag/v2.4.1).

## Contents

- [Overview](#introduction)
- [Features](#features)
- [Installation](#installation)
- [Articles](#articles)
- [Further details](#further-details)

## Overview

Given the results of a Bayesian model (possibly based on MCMC) in the form of simulations from the posterior distributions of suitable variables of costs and clinical benefits for two or more interventions, produces a health economic evaluation. Compares one of the interventions (the "reference") to the others ("comparators").

## Features
Main features of `BCEA` include:

* Cost-effectiveness analysis plots, such as CE planes and CEAC
* Summary statistics and tables
* EVPPI calculations and plots

## Installation
Install the released version from CRAN with
```r
install.packages("BCEA")
```

The development version can be installed using this GitHub repository. On Windows machines, you need to install a few dependencies, including [Rtools](https://cran.r-project.org/bin/windows/Rtools/) first, e.g. by running

```r
pkgs <- c("MASS", "Rtools", "remotes")
repos <- c("https://cran.rstudio.com", "https://inla.r-inla-download.org/R/stable") 
install.packages(pkgs, repos=repos, dependencies = "Depends")
```
before installing the package using `remotes`:

```r
remotes::install_github("giabaio/BCEA", ref="dev")
```
Under Linux or MacOS, it is sufficient to install the package via `remotes`:

```r
install.packages("remotes")
remotes::install_github("giabaio/BCEA", ref="dev")
```

## Articles
Examples of using specific functions and their different arguments are given in [these articles](https://n8thangreen.github.io/BCEA/articles/index.html):

-  [Get Started](https://n8thangreen.github.io/BCEA/articles/bcea.html)
-  [Set `bcea()` Parameters: Constructor and Setters](https://n8thangreen.github.io/BCEA/articles/Set_bcea_parameters.html)
-  [Cost-Effectiveness Acceptability Curve Plots](https://n8thangreen.github.io/BCEA/articles/ceac.html)
-  [Cost-Effectiveness Efficiency Frontier](https://n8thangreen.github.io/BCEA/articles/ceef.html)
-  [Risk Aversion Analysis](https://n8thangreen.github.io/BCEA/articles/CEriskav.html)
-  [Expected Incremental Benefit Plot](https://n8thangreen.github.io/BCEA/articles/eib.html)
-  [Paired vs Multiple Comparisons](https://n8thangreen.github.io/BCEA/articles/paired_vs_multiple_comps.html)


## Further details
The `pkgdown` site is [here](https://n8thangreen.github.io/BCEA/).
More details on `BCEA` are available in our book [_Bayesian Cost-Effectiveness Analysis with the R Package BCEA_](https://gianluca.statistica.it/book/bcea/) (published in the UseR! Springer series). Also, details about the package, including some references and links to a pdf presentation and some posts on my own blog) are given [here](https://gianluca.statistica.it/software/bcea/).

## Licence
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Contributing
Please submit contributions through `Pull Requests`, following the [contributing
guidelines](https://github.com/n8thangreen/BCEA/blob/dev/CONTRIBUTING.md).
To report issues and/or seek support, please file a new ticket in the
[issue](https://github.com/n8thangreen/BCEA/issues) tracker.

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/n8thangreen/BCEA/blob/dev/CONDUCT.md). By participating in this project you agree to abide by its terms.
