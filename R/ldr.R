
# Package: ldr
# Type: Package
# Title: Methods for likelihood-based dimension reduction in regression
# Version: 1.3.3
# Date: 2014-06-06
# Author: Kofi Placid Adragni, Andrew Raim
# Maintainer: Kofi Placid Adragni <kofi@umbc.edu>
# Description: Functions, methods, and data sets for fitting likelihood-based dimension reduction in regression,
# using principal fitted components (pfc), likelihood acquired directions (lad), covariance reducing models (core).
# URL: https://www.jstatsoft.org/v61/i03/
#   License: GPL (>= 2)
# Packaged: 2021-10-08 16:32:42 UTC; Nathan
# Repository: https://github.com/cran/ldr
# Date/Publication: 2014-10-29 16:36:14
#
# Likelihood-based Dimension Reduction
# 
# Main function of the package. It creates objects of one of classes core, lad,
# or pfc to estimate a sufficient dimension reduction subspace using covariance reducing models (CORE),
# likelihood acquired directions (LAD), or principal fitted components (PFC).
# 
ldr <-
  function(X, y=NULL, fy=NULL, Sigmas=NULL, ns=NULL, numdir=NULL, nslices=NULL, 
           model=c("core", "lad", "pfc"), numdir.test=FALSE, ...)
  {
    if (model=="pfc")
    {	
      if (is.null(fy)){stop("fy is not provided"); return()}
      
      return(invisible(pfc(X=X, y=y, fy=fy, numdir=numdir, numdir.test=numdir.test, ...)))
    }
    
    if (model=="lad") 
    {
      if (is.null(y)){stop("The response is needed"); return()}
      
      return(invisible(lad(X=X, y=y, numdir=numdir, nslices=nslices, numdir.test=numdir.test,...)))
    }
    
    if (model=="core") 
    {
      if (!is.null(Sigmas) && !is.null(ns))
        return(invisible(core(Sigmas=Sigmas, ns=ns, numdir=numdir, numdir.test=numdir.test,...)))
      
      return(invisible(core(X=X, y=y, numdir=numdir, numdir.test=numdir.test,...)))
    }
  }

