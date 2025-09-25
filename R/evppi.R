
#' Expected Value of Perfect Partial Information (EVPPI) for Selected
#' Parameters
#' 
#' Calculates the Expected Value of Perfect Partial Information (EVPPI) for
#' subsets of parameters. Uses GAM non-parametric regression for single
#' parameter EVPPI and the SPDE-INLA method for larger parameter subsets.
#' 
#' The single parameter EVPPI has been calculated using the non-parametric GAM
#' regression developed by Strong *et al.* (2014). The multi-parameter EVPPI is
#' calculated using the SPDE-INLA regression method for Gaussian Process
#' regression developed by Heath *et al.* (2015).
#' 
#' This function has been completely changed and restructured to make it possible
#' to change regression method.
#' The method argument can now be given as a list. The first element in the
#' list is a vector giving the regression method for the effects. The second gives
#' the regression method for the costs. The `method` argument can also be given as
#' before which then uses the same regression method for all curves.
#' All other `extra_args` can be given as before. `int.ord` can be updated using the
#' list formulation above to give the interactions for each different curve.
#' The formula argument for GAM can only be given once, either `te()` or `s() + s()`
#' as this is for computational reasons rather than to aid fit.
#' You can still plot the INLA mesh elements but not output the meshes.
#' 
#' @aliases evppi evppi.default
#' @param param_idx A vector of parameters for which the EVPPI should be
#' calculated. This can be given as a string (or vector of strings) of names or
#' a numeric vector, corresponding to the column numbers of important
#' parameters.
#' @param input A matrix containing the simulations for all the parameters
#' monitored by the call to JAGS or BUGS. The matrix should have column names
#' matching the names of the parameters and the values in the vector parameter
#' should match at least one of those values.
#' @template args-he
#' @param N The number of PSA simulations used to calculate the EVPPI. The
#' default uses all the available samples.
#' @param plot A logical value indicating whether the triangular mesh for
#' SPDE-INLA should be plotted. Default set to `FALSE`.
#' @param residuals A logical value indicating whether the fitted values for
#' the SPDE-INLA method should be outputted. Default set to `TRUE`.
#' @param method Character string to select which method to use. The default methods are recommended.
#'   However, it is possible (mainly for backward compatibility) to use different methods. 
#' @param ...  Additional arguments. Details of the methods to compute the EVPPI and their additional arguments are:
#' - For single-parameter:
#'    - Generalized additive model (GAM) (default).
#'    - The method of Strong & Oakley use `method` as string `so`.
#'      The user *needs* to also specify the number of "blocks" (e.g. `n.blocks=20`).
#'      Note that the multi-parameter version for this method has been deprecated.
#'    - The method of Sadatsafavi *et al.* where `method` takes as value a string of either `sad` or `sal`.
#'      It is then possible to also specify the number of "separators" (e.g. `n.seps=3`).
#'      If none is specified, the default value `n.seps=1` is used. 
#'      Note that the multi-parameter version for this method has been deprecated.
#' - For multi-parameter:
#'    - INLA/SPDE (default).
#'    - Gaussian process regression with `method` of `gp`.
#' 
#' @section GAM regression:
#' For multi-parameter, the user can select 3 possible methods. If
#' `method = "GAM"` (BCEA will accept also `"gam"`, `"G"` or
#' `"g"`), then the computations are based on GAM regression. The user can
#' also specify the formula for the regression. The default option is to use a
#' tensor product (e.g. if there are two main parameters, `p1` and
#' `p2`, this amounts to setting `formula = "te(p1,p2)"`, which
#' indicates that the two parameters interact). Alternatively, it is possible
#' to specify a model in which the parameters are independent using the
#' notation `formula = "s(p1) + s(p2)"`. This may lead to worse accuracy in
#' the estimates.
#' 
#' @section Strong *et al.* GP regression:
#' This is used if `method="GP"` (BCEA will also accept the specification
#' `method="gp"`). In this case, the user can also specify the number of
#' PSA runs that should be used to estimate the hyperparameters of the model
#' (e.g. `n.sim=100`). This value is set by default to 500.
#' 
#' @section INLA-related options:
#' These are all rather technical and are described in detail in Baio *et al.* (2017).
#' The optional parameter vector `int.ord` can take integer values (c(1,1) is
#' default) and will force the predictor to include interactions: if
#' `int.ord = c(k, h)`, then all k-way interactions will be used for the
#' effects and all h-way interactions will be used for the costs. Also, the
#' user can specify the feature of the mesh for the "spatial" part of the
#' model. The optional parameter `cutoff` (default 0.3) controls the
#' density of the points inside the mesh. Acceptable values are typically in
#' the interval (0.1, 0.5), with lower values implying more points (and thus
#' better approximation and greater computational time). The construction of the
#' boundaries for the mesh can be controlled by the optional inputs
#' `convex.inner` (default = -0.4) and `convex.outer` (default =
#' -0.7). These should be negative values and can be decreased (say to -0.7 and
#' -1, respectively) to increase the distance between the points and the outer
#' boundary, which also increases precision and computational time. The
#' optional argument`robust` can be set to TRUE, in which case INLA will
#' use a t prior distribution for the coefficients of the linear predictor.
#' Finally, the user can control the accuracy of the INLA grid-search for the
#' estimation of the hyperparameters. This is done by setting a value
#' `h.value` (default = 0.00005). Lower values imply a more refined search
#' (and hence better accuracy), at the expense of computational speed. The
#' method argument can also be given as a list allowing different regression
#' methods for the effects and costs, and the different incremental decisions.
#' The first list element should contain a vector of methods for the
#' incremental effects and the second for the costs, for example
#' `method = list(c("GAM"), c("INLA"))`. The `int.ord` argument can also
#' be given as a list to give different interaction levels for each regression
#' curve.
#'
#' By default, when no method is specified by the user, `evppi` will
#' use GAM if the number of parameters is <5 and INLA otherwise.
#' 
#' @return An object of class `evppi`, containing the following components:
#' 
#' - **evppi**: The computed values of `evppi` for all values of the parameter of willingness to pay.
#' - **index**: A numerical vector with the indices of the parameters for which the EVPPI was calculated.
#' - **k**: A vector of values for the willingness to pay.
#' - **evi**: A vector of values for the overall EVPPI.
#' - **fitted.costs**: The fitted values for the costs.
#' - **fitted.effects**: The fitted values for the effects.
#' - **parameters**: A single string containing the names of the parameters for which the EVPPI was calculated (used for plotting).
#' - **time**: The computational time (in seconds).
#' - **fit.c**: The object produced by the model fit for the costs.
#' - **fit.e**: The object produced by the model fit for the effects.
#' - **formula**: The formula used to fit the model.
#' - **method**: A string indicating the method used to estimate the EVPPI.
#' 
#' @author Anna Heath, Gianluca Baio
#' @seealso [bcea()],
#'          [plot.evppi()]
#' @importFrom Rdpack reprompt
#' @import voi
#' 
#' @references
#' 
#' \insertRef{Strong2014}{BCEA}
#' 
#' \insertRef{Sadatsafavi2013}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' \insertRef{Baio2017}{BCEA}
#' 
#' \insertRef{Heath2016}{BCEA}
#' 
#' @export
#' @md
#' 
#' @examples
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#'
#' \dontrun{
#' # Load the post-processed results of the MCMC simulation model
#' # original JAGS output is can be downloaded from here
#' # https://gianluca.statistica.it/books/bcea/code/vaccine.RData
#' 
#' data(Vaccine, package = "BCEA")
#' treats <- c("Status quo", "Vaccination")
#' 
#' # Run the health economic evaluation using BCEA
#' m <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#'
#' # Compute the EVPPI for a bunch of parameters
#' inp <- createInputs(vaccine_mat)
#' 
#' # explicitly use BCEA package namespace to avoid voi package conflict
#' EVPPI <- BCEA::evppi(m, c("beta.1." , "beta.2."), inp$mat)
#' 
#' plot(EVPPI)
#' 
#' # deprecated (single parameter) methods
#' EVPPI.so <- BCEA::evppi(m, c("beta.1.", "beta.2."), inp$mat, method = "so", n.blocks = 50)
#' EVPPI.sad <- BCEA::evppi(m, c("beta.1.", "beta.2."), inp$mat, method = "sad", n.seps = 1)
#' 
#' plot(EVPPI.so)
#' plot(EVPPI.sad)
#'  
#' # Compute the EVPPI using INLA/SPDE
#' if (require("INLA"))
#'   x_inla <- BCEA::evppi(he = m, 39:40, input = inp$mat)
#' 
#' # using GAM regression
#' x_gam <- BCEA::evppi(he = m, 39:40, input = inp$mat, method = "GAM")
#' 
#' # using Strong et al GP regression
#' x_gp <- BCEA::evppi(he = m, 39:40, input = inp$mat, method = "GP")
#' 
#' # plot results
#' if (require("INLA")) plot(x_inla)
#' points(x_inla$k, x_inla$evppi, type = "l", lwd = 2, lty = 2)
#' points(x_gam$k, x_gam$evppi, type = "l", col = "red")
#' points(x_gp$k, x_gp$evppi, type = "l", col = "blue")
#' 
#' if (require("INLA")) {
#'   plot(x_inla$k, x_inla$evppi, type = "l", lwd = 2, lty = 2)
#'   points(x_gam$k, x_gam$evppi, type = "l", col = "red")
#'   points(x_gp$k, x_gp$evppi, type = "l", col = "blue")
#' }
#' 
#' data(Smoking)
#' treats <- c("No intervention", "Self-help",
#' "Individual counselling", "Group counselling")
#' m <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)
#' inp <- createInputs(smoking_output)
#' EVPPI <- BCEA::evppi(m, c(2,3), inp$mat, h.value = 0.0000005)
#' plot(EVPPI)
#' }
#' 
evppi <- function(he,
                  param_idx,
                  input,
                  N = NULL,
                  plot = FALSE,
                  residuals = TRUE,
                  ...)
  UseMethod("evppi", he)

