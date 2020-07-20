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
	
# BCEA 2.3-1<sup>^</sup>

# BCEA 2.2-6<sup>^</sup>
* Fix in `evppi` to allow N to be selected in all methods + fix `diag.evppi`

# BCEA 2.2-5<sup>^</sup>

* Some changes to EVPPI

# BCEA v2.2.4
Nov 2016

* fixes for new ggplot2 version (`legend.spacing()` and `plot.title` hjust argument)

# BCEA 2.2-3<sup>^</sup>
May 2016

* major update for the EVPPI to include PFC + fixed issues with info.rank

# BCEA 2.2-2<sup>^</sup>
January 2016

* minor change to `ceef.plot` to align with ggplot2 v2.0.0

# BCEA v2.2.1
October 2015

* adds the info-rank plot

# BCEA v2.2
October 2015

* cleaned up and aligned with R's settings
* `EVPPI` function polished up

# BCEA 2.1-1<sup>^</sup>
April/July 2015

* new function for EVPPI using SPDE-INLA
* modifications to the EVPPI functions 
* documentation updated
* allows xlim & ylim in the `ceplane.plot`, `contour` and `contour2` functions
* it is now possible to run bcea for a scalar wtp
* old evppi function and method has been renamed `evppi0`, which means there's also a new `plot.evppi0` method
	  
# BCEA 2.1-0<sup>^</sup>
October 2014

* migrated from `if(require())` to `if(requireNamespace(,quietly=TRUE))`
* documentation updated
* added threshold argument to `ceef.plot` function; documentation updated

# BCEA v2.1.0-pre2
October 2014

* modifications to `ceef.plot`, `createInputs`, `struct.psa`

# BCEA v2.1-0-pre1
September 2014

* documentation updated
* Smoking dataset and `ceef.plot` function included, additional modifications

# BCEA v2.0-2c
July, 2014

# BCEA v2.0-2b
February 2014

* `ceac.plot` and `eib.plot`: option comparison included for base graphics

# BCEA 2.0-2<sup>^</sup>
November 2013

# BCEA 2.0-1<sup>^</sup>
July, 2013

# BCEA 2.0<sup>^</sup>

## Feature updates

* Implements two quick and general methods to compute the EVPPI
* Function `CreateInputs()`, which takes as input an object in the class rjags or bugs
* Compute the EVPPI for one or more parameters calling the function `evppi()`
* Results can be visualised using the specific method plot for the class evppi and show the overall EVPI with the EVPPI for the selected parameter(s)

# BCEA 1.3-1

# BCEA 1.3-0<sup>^</sup>
June 2013

# BCEA 1.2
17 September 2012

# BCEA 1.1.1<sup>^</sup>

# BCEA 1.1<sup>^</sup>
14 September 2012

# BCEA 1.0<sup>^</sup>
4 January 2012
