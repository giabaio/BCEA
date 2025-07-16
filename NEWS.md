# BCEA 2.4.81

_July 2025_

* Fix a small issue in `multi.ce`. The CRAN version has removed the use of `BCEA:::compute_p_best_interv`, which computes the probability that each intervention is the 
most cost-effective, for each value of the willingness to pay. The output of that call must be passed as input to `BCEA:::ceac_plot_XXXX.pairwise` in order for
`ceac.plot` to produce the individual probability of cost-effectiveness

* Fix the function `summary.pairwise` to comply with the changes in `multi.ce`

# BCEA 2.4.8 

_June 2025_

* In `bcea()` the probability of an intervention being 'best' was the most cost-effective one calculated using `compute_p_optimal_best()`. However, this is not actually how this is defined in the literature
for the cost-effectiveness acceptability frontier (CEAF). Is should be calculated as the probability that the best intervention is the 'optimal' one, that is the one on average using the mean cost and mean effectiveness. We're replaced
this internal with `compute_p_optimal_best()`. In most cases this will make very little difference but when the cost or effectiveness are (highly) skewed they may deviate (a0b9ed6). 

* Many changes to the `plotly` infrastracture, which are helpful for `BCEAweb` (a565620). 

* Reformatting of the example datasets (daa2494).

* Aligns examples and code with newer version of `ggplot2` (c1e0aac).


# BCEA 2.4.7

_January 2025_

* In `ceplane.plot()` for `{ggplot2}` version used the ggplot syntax thats already used for other plotting arguments so that we can now pass, e.g. `wtp = list(value = 20000, colour = "blue", x = 10, y = 10, size = 4)`. This closes issue #151 so can do something like `wtp = list(size = 0)` to hide the willingness to pay text. (3d8a770)
* Small features added which were missing for the new edition of the BCEA book:
  - Extended `ggplot` version legend to take a vertical or horizontal part to the `pos` argument (85ba87f).
  - `ceplane.plot()` and `ceac plot` relative font size argument `rel` fixed (8d1586f).
  - `eib_plot` and `evi_plot` text size `ggplot` argument added (1e32788) and (84aaa2f).
  - `ceplane.plot()` now has`label.pos` logical argument fixed for `ggplot` (92e9eb0) and base `R` (c7351e4).


# BCEA 2.4.6

_February 2024_

Patch fixing small bugs from last CRAN release.

* Moved `{voi}` package to Suggests in DESCRIPTION and added `requireNamespace()` in `evppi()` to avoid error when not installed (e.g. on CRAN) (f3e3e3e)
* Converted help documentation in `man-roxygen` folder to md (cf858b1)
* bugfix: line width in CEAC plot. `{ggplot2}` changed in version 3 to `linewidth` from `size` argument and had only changed some of the code. Updated to `scale_linewidth_manual()`. (60bea9c)
* Using `testdata` folder `{testthat}` unit tests. (cbce0fa)

# BCEA 2.4.5

_November 2023_

Some cosmetic changes to clean up.

* Removed the (by now, unnecessary) appveyor webhook
* Added correct `Remotes` in the `DESCRIPTION` file to point to the correct GitHub repos for `voi` and `plotrix`
* Changed the class of the object `smoking_output` to be used in the `evppi` example avoiding the need for `rjags`

_October 2023_

Moved internal EVPPI calculation out of `BCEA` and now uses `voi` package instead. 
Refactoring but retaining same interface and functionality.

* Ensure using latest CRAN release of `{voi}` which has a patch so that BCEA can use it without losing functionality
  + Latest version of `{voi}` needed when we use `check = TRUE` in `voi::evppi()` in order to access fitting data (6e436b5, 94f5fc5)
* `evppi()` tested against all use cases in BCEA book (1c1457d2)
* Select parameters by position (as well as name) in new `evppi()` (f2e4d005)
* Use single parameter case only like `voi` package for methods `sal` and `so` (#140)
* New `evppi()` matching output of old `evppi()` (1e2c5e7)
* Latest development version of `voi` needed when we use `check = TRUE` in `voi::evppi()` in order to access fitting data (6e436b5, 94f5fc5)
* No longer require `INLA` package to be available inside of `BCEA` so can remove direct dependency. This helps with passing CRAN checks and GitHub Actions ()

# BCEA 2.4.4
_June 2023_

* Patch to fix a CRAN checks error. Suggested package `{MCMCvis}` wasn't used conditionally in unit test. Moved to Required packages in `DESCRIPTION`.

# BCEA 2.4.3
_May 2023_

## Bug fixes

* Consistent colours across plots for each intervention for grid of plots in `plot.bcea()` (cf1ee43)
* `make.report()` change variable name (f940f2e)
* Fixed issue with summary table where names of interventions in the wrong order (6a006e3)
* `summary.bcea()` now only prints results for chosen comparisons and not always all of them. `kstar` and `best` in `bcea()` object were not updated with subset of interventions (#125)

## Refactoring

* `withr::with_par()` used in plotting function `plot.bcea()` to only temporarily change graphics parameters. (725c536)
* Using `@md` and markdown syntax in function documentation
* Update `psa.struct()` to add the absolute value in the formula to compute the weights (1cea278)
* Use `dplyr` piping new syntax from `.data$*` to simply using speech marks `"*"` (2b280ad)

## Miscellaneous

* Template added for GitHub Issues (0ea59fa)


# BCEA 2.4.2
_August 2022_

## Bug fixes

* `summary.bcea()` wasn't passing `wtp` argument to `sim_table()` internally (5440eb3)
* `summary()` was the same for basic `bcea` and `multi.ce` objects. Now has own `summary.pairwise()` method. (88ade51)
* `struct.psa()` output now works with `summary()` and plots all still work without having to use $ to get at `bcea` object as before. (b014c83)
* Changed `wtp` argument in `bcea()` to `k` because `wtp` in the plotting functions refers to the wtp line and so is a scalar whereas `k` is a grid of points. Added an error message to use new argument. (b014c83)
* `bcea()` still allows a scalar `k` but added a warning that this will give empty plots.
* Updated GitHub Actions for checking the package to use `r-lib/Actions` version 2. There was an error with not finding INLA but this was solved by Gabor at RStudio (see thread here https://community.rstudio.com/t/not-finding-inla-package-not-on-cran-in-actions/141398)
* GrassmannOptim package r-release-macos-x86_64 isn't available resulting in a CRAN check error and doesn't appear to be maintained. Tried emailing the author but bounced. Removed dependency and copied `GrassmannOptim()` function inside of package with acknowledgement.

## Refactoring

* Now uses `Rdpack` for bibliography in documentation (229c96d)
* The cost and health values in the `Smoking` and `Vaccine` data sets have been renamed from `c` and `e` to `cost` and `eff`. This is to avoid any conflict with the `c()` function.
* Changed the axes labels in the cost-effectiveness planes from "differential" to "incremental". (688d98b)

## New features

* Can now specify what order the interventions labels are in the legend for ce plane (and contour plots) for base R and ggplot2 i.e. reference first or second with optional `ref_first` argument (cc38f07)
* Can specify currency for axes in `ceplane.plot()` and `ceac.plot()` `ggplot2` versions (6808aa6)
* Argument added to `ceplane.plot()` of `icer_annot` to annotate each of the ICER points with the text label of the intervention name. Only for `ggplot2` at the moment. (a7b4beb)
* Added `pos` argument to `contour2()` so that its consistent with `contour()` and `ceplane.plot()`. (50f8f8b)
* Allow passing `ref` argument by name as well as index in `bcea()`. (9eab459)


# BCEA 2.4.1.2
_April 2022_

## Bug fixes

* `ceplane_ggplot()` missing legend
* Legend bug in `evppi()`
* Arguments in consistent same order as `ceplane.plot()`
* `ceplane_plot_base()` wasn't showing grey area. Fixed by removing `alpha` transparency
* `ceac.plot()` wasn't showing confidence interval by default for one comparison
* Typo fixed in dropping dimension in `compute_vi()`
* `setReferenceGroup()` for CEAC plot legend error; doesn't use supplied names but generic intervention 1, intervention 2, ... (#82)
* Missing `multi.ce()` line for reference group (#80)

## Miscellaneous

* Use `cli` package for warning messages
* Clean @keywords in Roxygen
  * Removed all the internal helper functions from the Manual by using @keyword internal
* Refactor contour plots
* Plot functions take more standard `ggplot2` format style arguments e.g. as list
  * Extend some function (`ceac.plot()`) to take more style arguments than before for e.g. colour of lines, types of points and line thickness.
  * Resuse `ceplane.plot()` code in `contour()`
* `goodpractice` package suggested changes
  * line length, `seq_len()`, remove `;`
* Contributor guidelines (#93)
* Deprecated functions document
* `contour2()` changed so `xlim`, `ylim` arguments are optional; the same as `ceplane.plot()` since they are passes to it
* `contour()` and `ceplane.plot()` vignettes written



# BCEA 2.4.1.1
_Oct 2021_

## Major refactoring

* Code base improved robustness and extensibility.
* `bcea()` is now a helper function which calls the constructor `new_bcea()`, separating concerns.
* `new_bcea()` composed of smaller HEE statistics functions with names starting with `compute_*` e.g. `compute_CEAC()`, `compute_EIB()`,.... This allows us to call and test them individually.
  It also allows more flexibility in changing or adding functionality to `new_bcea()`.
* Plotting functions have been rewritten. These functions now simply dispatch to the base R, ggplot2 or plotly versions (think strategy pattern).
  Internally, these functions, e.g.`ceplane_plot_ggplot()`, are also split into parameter and data setting and plotting components.
  This modulisation allows us to add new layers to plots or modify existing parameter sets and defaults. We could also return the data without the plotting step as in e.g. `ggplot2::autoplot()`.
  It also means we can reuse some functionality across plots such as axes and legend setting e.g. `BCEA:::where_legend()`.
* `ceac_plot()` changes
  * Deprecated `mce.plot()`. Now dispatched on `ceac.plot()` for both `multi.ce()` and `bcea()` outputs.
  * For a multiple comparison the plot for pairwise comparison over all interventions is returned by default. The alternative version of each comparison against the reference group is still available.
  * Plots and tables using S3 methods for `bcea` type object.
* Tables updated. Duplication in `summary()` and `sim_table()` removed.
* `createInputs()` used for EVPI calculation now dispatches S3 methods by JAGS, BUGS, Stan and other R data types.
* `make.report()` rewritten to have separate section files.

## New features

* Extend ways to set comparison interventions. Subsets of comparison can still be set in a call to a plotting function as before.
Now subsets can be set in both the original `bcea()` construction or separately using a setter functions `setComparisons()`.
* Similarly, maximum willingness to pay and the reference group can be set with `setKmax()` and `setReferenceGroup()`, respectively.
* `multi.ce()` and `CEriskAv()` also now work similarly. They operate by modifying the `bcea` object, rather than creating new one (think decorator pattern).
* `bcea()` methods for JAGS, WinBUGS, Stan (#76)

## Miscellaneous

* Additional help documentation and examples.
* New vignettes about plotting and comparison intervention setting.
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
* Allows `xlim` & `ylim` in the `ceplane.plot()`, `contour()` and `contour2()` functions
* It is now possible to run `bcea` for a scalar wtp
* Old `evppi()` function and method has been renamed `evppi0`, which means there's also a new `plot.evppi0` method

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
