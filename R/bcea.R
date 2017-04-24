###INTRO#############################################################################################
## Define Classes & Methods
## v1.0. 4 January, 2012
## v1.1. 14 September, 2012
## v1.2. 17 September 2012
## v1.3-0 June, 2013
## v2.0-1 July, 2013
## v2.0-2 November, 2013
## v2.0-2b February, 2014 - ceac.plot and eib.plot: option comparison included for base graphics
## v2.0-2c July, 2014
## v2.1-0-pre1 AB September, 2014: documentation updated, Smoking dataset and ceef.plot function included, additional modifications
## v2.1.0-pre2 GB October, 2014: modifications to ceef.plot, CreateInputs, struct.psa
## v2.1.0 AB October, 2014: migrated from if(require()) to if(requireNamespace(,quietly=TRUE)); documentation updated
## v2.1.0 AB December, 2014: added threshold argument to ceef.plot function; documentation updated
## v2.1.1 GB+AH April/July 2015: new function for EVPPI using SPDE-INLA; modifications to the EVPPI functions; 
##        documentation updated; allows xlim & ylim in the ceplane.plot, contour and contour2 functions;
##	  it is now possible to run bcea for a scalar wtp; the old evppi function and method has been renamed 
## 	  evppi0, which means there's also a new plot.evppi0 method
## v2.2   GB October 2015: cleaned up and aligned with R's settings. EVPPI function polished up
## v2.2.1 GB+AH October 2015: adds the info-rank plot
## v2.2.2 AB January 2016: minor change to ceef.plot to align with ggplot2 v2.0.0
## v2.2.3 AH+GB May 2016: major update for the EVPPI to include PFC + fixed issues with info.rank
## v2.2.4 AB Nov 2016: fixes for new ggplot2 version (legend.spacing() and plot.title hjust argument)
## v2.2.5 Some changes to EVPPI
## v2.2.6 Fix in evppi to allow N to be selected in all methods + fix diag.evppi
## (C) Gianluca Baio + contributions by Andrea Berardi, Chris Jackson, Mark Strong & Anna Heath

###bcea##############################################################################################
bcea <- function(e,c,ref=1,interventions=NULL,Kmax=50000,wtp=NULL,plot=FALSE) UseMethod("bcea")





























