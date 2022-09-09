
# Package: ldr
# Type: Package
# Title: Methods for likelihood-based dimension reduction in regression
# Version: 1.3.3
# Date: 2014-06-06
# Author: Kofi Placid Adragni, Andrew Raim
# Maintainer: Kofi Placid Adragni <kofi@umbc.edu>
# Description: Functions, methods, and data sets for fitting likelihood-based
# dimension reduction in regression,
# using principal fitted components (pfc), likelihood acquired directions (lad),
# covariance reducing models (core).
# URL: https://www.jstatsoft.org/v61/i03/
#   License: GPL (>= 2)
# Packaged: 2021-10-08 16:32:42 UTC; Nathan
# Repository: https://github.com/cran/ldr
# Date/Publication: 2014-10-29 16:36:14
#
# Function to generate a basis function.
# 
# This function is to construct a data-matrix of basis function using the n
# response observations.
# The response can be continuous or categorical. The function returns a matrix of n
# rows and r columns.
# The number of columns r depends on the choice of basis function. Polynomial,
# piecewise polynomial continuous and discontinuous,
# and Fourier bases are implemented. For a polynomial basis, r is the degree of
# the polynomial.
# 
bf <-
  function(y, case=c("poly", "categ", "fourier", "pcont", "pdisc"), degree=1, nslices=1, scale=FALSE)
  {
    Indicator<-function(x, H) return(ifelse((x %in% H), 1, 0))
    
    case <- match.arg(case)
    nobs <- length(y) 
    
    if (case=="categ")
    {
      bins.y<-unique(sort(y))
      r<- length(unique(sort(y)))-1
      fy<-array(rep(0), c(r, nobs))
      for (i in 1:r){ fy[i,]<-sapply(y, function(x) (x==bins.y[i]))} 
    }
    else if (case=="fourier")
    {
      fy<-array(rep(0), c(2*degree, nobs))
      for(i in 1:degree)
      {
        fy[2*i-1, 1:nobs]<- cos(2*pi*y*i)
        fy[2*i, 1:nobs]<-  sin(2*pi*y*i)
      }
    }
    else if (case=="poly") 
    {
      if (degree==0) stop("This case is not defined")
      fy <- array(rep(0), c(degree, nobs))
      for (k in 1:degree) fy[k, ] <- y^k
    }
    else if (case=="pdisc")
    {
      if ((nslices==0) | (nslices==1)){message("The minimum number of slices is 2")
        nslices <- 2
      }
      r <- (degree + 1) * nslices - 1
      fy <- array(rep(0), c(r, nobs))
      slicing <- ldr.slices(y,nslices)
      bins.y <- slicing$bins
      
      if (degree==0)	# Piecewise constant discontinuous
      {
        for(i in 1:r) fy[i,] <- Indicator(y, bins.y[[i]])
      }	
      else if (degree==1) # Piecewise linear discontinuous
      { 
        for(i in seq_len(nslices-1))
        {
          fy[2*i-1, ] <- Indicator(y, bins.y[[i]])
          fy[2*i, ] <- Indicator(y, bins.y[[i]])*(y-bins.y[[i]][1])
        }
        fy[2*nslices-1, ] <- Indicator(y, bins.y[[nslices]])*(y-bins.y[[nslices]][1])
      } 
      else if (degree==2) # Piecewise quadratic discontinuous
      { 
        for(i in 1:(nslices-1))
        {
          fy[3*(i-1)+1, ] <- Indicator(y, bins.y[[i]])
          fy[3*(i-1)+2, ] <- Indicator(y, bins.y[[i]])*(y-bins.y[[i]][1])
          fy[3*(i-1)+3, ] <- Indicator(y, bins.y[[i]])*(y-bins.y[[i]][1])**2
        }
        fy[3*nslices-2, ] <- Indicator(y, bins.y[[nslices]])*(y-bins.y[[nslices]][1])
        fy[3*nslices-1, ] <- Indicator(y, bins.y[[nslices]])*(y-bins.y[[nslices]][1])**2
      }
      else if (degree==3)# Piecewise cubic discontinuous
      { 
        for(i in 1:(nslices-1))
        {
          fy[4*(i-1)+1, ] <- Indicator(y, bins.y[[i]])
          fy[4*(i-1)+2, ] <- Indicator(y, bins.y[[i]])*(y-bins.y[[i]][1])
          fy[4*(i-1)+3, ] <- Indicator(y, bins.y[[i]])*(y-bins.y[[i]][1])**2
          fy[4*(i-1)+4, ] <- Indicator(y, bins.y[[i]])*(y-bins.y[[i]][1])**3
        }
        fy[4*nslices-3, ] <- Indicator(y, bins.y[[nslices]])*(y-bins.y[[nslices]][1])
        fy[4*nslices-2, ] <- Indicator(y, bins.y[[nslices]])*(y-bins.y[[nslices]][1])**2
        fy[4*nslices-1, ] <- Indicator(y, bins.y[[nslices]])*(y-bins.y[[nslices]][1])**3
      } 
    } 
    else if (case=="pcont")
    {
      if (nslices == 0 || nslices == 1){
        message("The minimum number of slices is 2")
        nslices <- 2}
      if (degree==0) stop("Piecewise Constant Continuous is not defined.")
      
      r <- nslices*degree+1
      fy <- array(rep(0), c(r, nobs))
      slicing <- ldr.slices(y, nslices)
      bins.y <- slicing$bins
      
      if (degree==1)# Piecewise linear continuous
      {  
        fy[1,] <- Indicator(y, bins.y[[1]])
        if (r>1) for(i in 1:nslices) fy[i+1,] <- Indicator(y, bins.y[[i]])*(y - bins.y[[i]][1])
      } 
      else if (degree==2)# Piecewise quadratic continuous
      { 
        fy[1,] <- Indicator(y, bins.y[[1]])
        for(i in 1:nslices)
        {
          fy[2*i,] <- Indicator(y, bins.y[[i]])*(y - bins.y[[i]][1])
          fy[2*i+1,] <- Indicator(y, bins.y[[i]])*(y - bins.y[[i]][1])**2
        }
      } 
      else if (degree==3)# Piecewise cubic continuous
      { 
        fy[1,] <- Indicator(y, bins.y[[1]])
        
        for(i in 1:nslices)
        {
          fy[3*i-1,] <- Indicator(y, bins.y[[i]])*(y - bins.y[[i]][1])
          fy[3*i,] <- Indicator(y, bins.y[[i]])*(y - bins.y[[i]][1])**2
          fy[3*i+1,] <- Indicator(y, bins.y[[i]])*(y - bins.y[[i]][1])**3
        }
      } 	
    }
    return( scale(t(Re(fy)), center=TRUE, scale=scale))
  }

