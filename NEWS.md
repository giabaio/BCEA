# BCEA 2.3-2
Oct 2021

## Major refactoring

Code base improved robustness and extensibility.
* `bcea()` is now a helper function which calls the constructor `new_bcea()`, separating concerns.
* `new_bcea()` composed of smaller HEE statistics functions with names starting with `compute_*` e.g. `compute_CEAC()`, `compute_EIB()`,.... This allows us to call and test them individually.
  It also allows more flexibility in changing or adding functionality to `new_bcea()`.
* Plotting functions have been rewritten. These functions now simply dispatch to the base R, ggplot2 or plotly versions (think strategy pattern).
  Internally, these functions, e.g.`ceplane_plot_ggplot()`, are also split into parameter and data setting and plotting components.
  This modulisation allows us to add new layers to plots or modify existing parameter sets and defaults. We could also return the data without the plotting step as in e.g. `ggplot2::autoplot()`.
  It also means we can reuse some functionality across plots such as axes and legend setting e.g. `BCEA:::where_legend()`.
* `ceac_plot()` changes
  * Deprecated `mce.plot()`. Now dispatched on `ceac.plot()` for both `multi.ce()` and `bcea()` outputs.
  * For a multiple comparison the plot for pairwise comparison over all interventions is returned by default. The alternative version of each comparison against the reference group is still available.* Plots and tables using S3 methods for `bcea` type object.
* New vignettes about plotting and comparison intervention setting.
* Tables updated. Duplication in `summary()` and `sim_table()` removed.
* `createInputs()` used for EVPI calculation now dispatches S3 methods by JAGS, BUGS, Stan and other R data types.
* `make.report()` rewritten to have separate section files.

## New features

* Extend ways to set comparison interventions. Subsets of comparison can still be set in a call to a plotting function as before.
Now subsets can be set in both the original `bcea()` construction or separately using a setter functions `setComparisons()`.
* Similarly, maximum willingness to pay and the reference group can be set with `setKmax()` and `setReferenceGroup()`, respectively.
* `multi.ce()` and `CEriskAv()` also now work similarly. They operate by modifying the `bcea` object, rather than creating new one (think decorator pattern).
* `bcea()` methods for jags, WinBUGS, Stan (#76)

## Miscellaneous

* Additional help documentation and examples.
* Testing suite started. This is not comprehensive as of yet.
* Added a `NEWS.md` file to track changes to the package. Details about previous releases, such as dates, versions, fixes and enhancements obtained from CRAN and code comments so a little patchy.
* `pkgdown` GitHub site made.
* Cheatsheet written and published on RStudio site (#22).
* Dependency package **ldr** removed from **BCEA** because it was removed from CRAN (#74)





# BCEA 2.3-1.1
26 Aug 2019

# BCEA 2.3-1
5 Aug 2019

# BCEA 2.2-6
11 July 2018

* Fix in `evppi` to allow `N` to be selected in all methods
* Fix `diag.evppi`

# BCEA 2.2-5
18 Nov 2016

* Some changes to EVPPI

# BCEA 2.2.4
Nov 2016

* Fixes for new ggplot2 version (`legend.spacing()` and `plot.title` `hjust` argument)

# BCEA 2.2-3
22 May 2016

* Major update for the EVPPI to include PFC
* Fixed issues with `info.rank`

# BCEA 2.2-2
25 Jan 2016

* Minor change to `ceef.plot` to align with ggplot2 v2.0.0

# BCEA 2.2.1
Oct 2015

* Adds the info-rank plot

# BCEA 2.2
Oct 2015

* Cleaned up and aligned with R's settings
* `EVPPI` function polished up

# BCEA 2.1-1
6 May 2015 2015

* New function for EVPPI using SPDE-INLA
* Modifications to the EVPPI functions
* Documentation updated
* Allows `xlim` & `ylim` in the `ceplane.plot`, `contour` and `contour2` functions
* It is now possible to run `bcea` for a scalar wtp
* Old `evppi` function and method has been renamed `evppi0`, which means there's also a new `plot.evppi0` method

# BCEA 2.1-0
13 Jan 2015

* Migrated from `if (require())` to `if (requireNamespace(,quietly=TRUE))`
* Documentation updated
* Added threshold argument to `ceef.plot` function

# BCEA 2.1.0-pre2
Oct 2014

* modifications to `ceef.plot`, `createInputs`, `struct.psa`

# BCEA 2.1-0-pre1
13 Jan 2015

* Documentation updated
* Smoking dataset and `ceef.plot` function included, additional modifications

# BCEA 2.0-2c
2 Dec 2013

# BCEA v2.0-2b
2 Dec 2013

* `ceac.plot` and `eib.plot`: option comparison included for base graphics

# BCEA 2.0-2
2 Dec 2013

# BCEA 2.0-1
31 July 2013

# BCEA 2.0
30 July 2013

## Feature updates

* Implements two quick and general methods to compute the EVPPI
* Function `CreateInputs()`, which takes as input an object in the class rjags or bugs
* Compute the EVPPI for one or more parameters calling the function `evppi()`
* Results can be visualised using the specific method plot for the class `evppi` and show the overall EVPI with the EVPPI for the selected parameter(s)

# BCEA 1.3-1

# BCEA 1.3-0
3 July 2013

# BCEA 1.2
17 September 2012

# BCEA 1.1.1
22 Feb 2013

# BCEA 1.1
15 Sept 2012


# BCEA 1.0
13 May 2012
