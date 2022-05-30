
#' BCEA: A package for Bayesian Cost-Effectiveness Analysis
#' 
#' A package to post-process the results of a Bayesian health economic model
#' and produce standardised output for the analysis of the results.
#' 
#' \tabular{ll}{ Package: \tab BCEA\cr
#'               Type: \tab Package\cr
#'               Version: \tab 2.4.1\cr
#'               Date: \tab 2021-02-05\cr
#'               License: \tab GPL2 \cr
#'               LazyLoad: \tab Yes\cr }
#'               
#' BCEA produces a health economic evaluation given a random sample of
#' suitable variables of costs and clinical benefits for two or more
#' interventions, e.g. using results of a Bayesian model (possibly based on
#' MCMC) in the form of simulations from the posterior distributions.
#' Compares one of the interventions (the "reference") to the others
#' ("comparators"). Produces many summaries and plots to analyse the results.
#' 
#' @aliases BCEA-package BCEA
#' 
#' @author Gianluca Baio, Andrea Berardi, Anna Heath, Nathan Green
#' 
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' \insertRef{Baio2017}{BCEA}
#'  
#' @keywords package
#' 
#' @docType package
#' @name BCEA-package
#' 
#' @import dplyr ggplot2 purrr reshape2
#' @importFrom Rdpack reprompt
NULL
