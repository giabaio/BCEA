# BCEA 2.3-2

* Added a `NEWS.md` file to track changes to the package.
* Major refactoring of code base
* Testing suite written

## Feature updates

* Deprecated `mce.plot()`. Now dispatched on `ceac.plot()` for both `multi.ce()` and `bcea()` outputs.
* `new_bcea()` constructor


## Fixes

# BCEA 2.3-1.1
26 Aug 2019
	
# BCEA 2.3-1
5 Aug 2019
 
# BCEA 2.2-6
11 July 2018

* Fix in `evppi` to allow N to be selected in all methods
* Fix `diag.evppi`

# BCEA 2.2-5
18 Nov 2016

* Some changes to EVPPI

# BCEA 2.2.4
Nov 2016

* Fixes for new ggplot2 version (`legend.spacing()` and `plot.title` hjust argument)

# BCEA 2.2-3
22 May 2016

* Major update for the EVPPI to include PFC
* Fixed issues with info.rank

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
* Allows xlim & ylim in the `ceplane.plot`, `contour` and `contour2` functions
* It is now possible to run bcea for a scalar wtp
* Old evppi function and method has been renamed `evppi0`, which means there's also a new `plot.evppi0` method
	  
# BCEA 2.1-0
13 Jan 2015

* Migrated from `if(require())` to `if(requireNamespace(,quietly=TRUE))`
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
* Results can be visualised using the specific method plot for the class evppi and show the overall EVPI with the EVPPI for the selected parameter(s)

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
