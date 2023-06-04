# ##TODO:...
# 
# #' Two-stage EVPPI
# #'
# #' Performs the (rather computationally expensive) computations for the EVPPI. Effectively, considers
# #' the vector of parameters theta=(phi,psi), where phi is the parameter of interest and psi is
# #' the rest of the parameters. First needs to run the Bayesian model assuming psi as random variables
# #' and taking phi as a fixed value. Then performs the economic analysis calling BCEA and then
# #' computes some relevant syntheses.
# #' 
# #' @param phi a vector with simulations from the posterior distribution of the the parameter with respect
# #' to which the EVPPI is computed (phi), obtained by the full model
# #' @param phi.name a string containing the name of the parameter phi, as represented in the txt file
# #' of the full model for psi. In this case phi is considered as fixed. This is has to be
# #' the same as one of the elements of the list data!
# #' @param n.out number of simulations used to compute the average with respect to phi (default = 1000)
# #' @param m a BCEA object in which the full economic analysis is stored as a result of the call to bcea
# #' @param data a list with the data to be passed to JAGS to run the model for the vector of parameters psi
# #' (conditionally on each value of the fixed parameter phi)
# #' @param model.file a txt file containing the full model for psi (but considering the parameter phi as fixed.
# #' This is in general a modification of the original model file)
# #' @param params a collection of parameters to be monitored in the conditional model for psi (given phi) 
# #' @param inits specifies the initial values for the MCMC procedure (default = NULL)
# #' @param n.iter number of iterations to be run
# #' @param n.burnin number of iterations to be discarded as burn-in
# #' @param n.thin number of iterations to be considered for thinning (default NULL)
# #' @param n.mc number of simulations retained to compute the MC estimations (default=1000)
# #' @param working.dir a string containing the working directory (default is NULL)
# #' @param ec.vars a string with the name of the R file that contains the commands required to
# #' compute the relevant variables of costs and benefits. These are usually
# #' obtained as functions of the parameters estimated by the full Bayesian model
# #' @param bcea.call a string containing the call to BCEA to perform the C/E evaluation. By default
# #'             assumes no options and the existence of two vectors e,c in which suitable
# #'             values for the measures of effectiveness and costs are stored
# #'             
# #' @return List
# #'
# evppi2stage <- function(phi,
#                         phi.name,
#                         n.out = 1000,
#                         m,
#                         data,
#                         model.file,
#                         params,
#                         inits = NULL,
#                         n.iter,
#                         n.burnin,
#                         n.thin = NULL,
#                         n.mc = 1000,
#                         working.dir = NULL,
#                         ec.vars,
#                         bcea.call = "bcea(eff, cost)") {
#   
#   # Replicates the C/E analysis for a fixed value of the relevant parameter at each iteration
#   Ustar.phi <-
#     matrix(NA, n.out, length(m$k))	# matrix of conditional net benefits
#   
#   for (i in 1:n.out) {
#     cmd <- paste(phi.name, " <- phi[i]", sep = "")
#     eval(parse(text = cmd))
#
#     n.thin <- n.thin %||% floor((n.iter - n.burnin) / (n.mc / 2))
#     
#     model.evppi <-
#       jags(data,
#            inits,
#            params,
#            model.file = model.file,
#            n.chains = 2,
#            n.iter,
#            n.burnin,
#            n.thin,
#            DIC = TRUE,
#            working.directory = working.dir,
#            progress.bar = "text")
#     
#     attach.bugs(model.evppi$BUGSoutput)
#     
#     # performs the economic analysis given the simulations for psi conditionally on phi
#     cmd <- paste("source('", ec.vars, "')", sep = "")
#     eval(parse(text = cmd))
#     
#     # runs BCEA to perform the economic analysis conditionally on phi
#     m.evppi <- eval(parse(text = bcea.call))
#     
#     # now computes the maximum expected utility for the current iteration for each value of k
#     Ustar.phi[i, ] <- apply(apply(m.evppi$U, c(2, 3), mean), 1, max)
#     rm(m.evppi)
#     detach.bugs()
#     print(i)		# prints a counter to show the progression in the analysis
#   }
#   
#   # Computes the average value of the maximum expected utility for each value of k
#   Vstar.phi <- apply(Ustar.phi, 2, mean)
#   Umax <- apply(apply(m$U, c(2, 3), mean), 1, max)
#   
#   # Finally computes the EVPPI for each value of k
#   EVPPI <- Vstar.phi - Umax
#   
#   list(evppi = EVPPI,
#        Vstar.phi = Vstar.phi,
#        Ustar.phi = Ustar.phi)
# }
# 
