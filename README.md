# BCEAweb
## A web-application to front-end the R package BCEA

`BCEAweb` is a front-end that can be used to access many of the functionalities of the `R` package `BCEA`.
`BCEAweb` already exists as a stand-alone [webpage](https://egon.stats.ucl.ac.uk/projects/BCEAweb/). This package allows the user to have a **local** version of the webapp, so that they can post-process the output of their economic model without needing to use a remote server.

From an existing `R` session`, the user can simply call the webapp by typing
```
library(BCEAweb)
BCEAweb()
```
(see the help for other options)

## Installation
The `BCEAweb` add-on package to `BCEA` is available from this GitHub repository and can be installed using
```
remotes::install_github("giabaio/BCEA",ref="BCEAweb")
```
(this requires that the package `remotes` is also installed).
