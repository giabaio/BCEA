
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
# Divides a vector of length n into slices of approximately equal size.
# It is used to construct the piecewise bases, and internally used in lad functions.
# 
ldr.slices <-
  function(y, nslices=3)
  {	
    endpoints <- function(n, nslices)
    {
      # This function is intended to determine the end-points of the slices.
      increment <- floor(n/nslices)
      if (nslices ==1) return(n)
      
      if (nslices > 1) 
      {
        ends<- seq(1:nslices)*increment
        rest <- n%%nslices
        if (rest==0) return(ends)
        
        if (rest>0)
        {
          for (i in 1:rest) ends[i]<-ends[i]+i
          for (i in (rest+1):nslices) ends[i]<- ends[i]+rest
          return(ends)
        }
      }
    }
    n <- length(y)
    indicators <- vector(length=n)
    sorty <- sort(y)
    bins.y <- vector("list", nslices)
    ends <- endpoints(n, nslices)
    
    if (nslices==1)
    {
      bins.y[[1]] <- sorty[1:ends[1]]
      indicators[1:ends[1]]<-1
      return(list(bins=bins.y, nslices=nslices, slice.size=n, slice.indicator=indicators[rank(y)]))
    }
    else if (nslices==2)
    {
      bins.y[[1]] <- sorty[1:ends[1]]
      bins.y[[2]] <- sorty[(ends[1]+1):ends[2]]
      indicators[1:ends[1]]<-1
      indicators[(ends[1]+1):ends[2]]<-2
      
      return(list(bins=bins.y, nslices=nslices, slice.size=diff(c(0,ends)), slice.indicator=indicators[rank(y)]))
    }
    else
    {		
      bins.y[[1]] <- sorty[1:(ends[1]-1)]
      indicators[1:(ends[1]-1)]<-1
      
      for (i in 2:(nslices-1))
      { 
        bins.y[[i]] <- sorty[ends[i-1]:(ends[i]-1)]
        indicators[ends[i-1]:(ends[i]-1)] <- i
      }
      bins.y[[nslices]] <- sorty[ends[nslices-1]:(ends[nslices])]
      indicators[ends[nslices-1]:(ends[nslices])] <- nslices
      
      return(list(bins=bins.y, nslices=nslices, slice.size=diff(c(0,ends)), slice.indicator=indicators[rank(y)]))
    }
  }
