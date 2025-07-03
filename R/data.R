#' @title Smoking Cessation Cost-Effectiveness Data
#' @name Smoking
#'
#' @description This data set contains the results of a Bayesian analysis modeling the
#' clinical outputs and costs for an economic evaluation of four different
#' smoking cessation interventions.
#'
#' @format A list containing the variables for the cost-effectiveness analysis:
#' \describe{
#'   \item{cost}{A matrix of 500 simulations from the posterior distribution
#'     of the overall costs for the four strategies.}
#'   \item{data}{A dataset with characteristics of smokers in the UK population.}
#'   \item{eff}{A matrix of 500 simulations from the posterior distribution of
#'     the clinical benefits for the four strategies.}
#'   \item{life.years}{A matrix of 500 simulations from the posterior
#'     distribution of the life years gained with each strategy.}
#'   \item{pi_post}{A matrix of 500 simulations from the posterior
#'     distribution of the probability of smoking cessation with each strategy.}
#'   \item{smoking}{A data frame with inputs for the network meta-analysis,
#'     containing: \code{nobs} (record ID), \code{s} (study ID), \code{i}
#'     (intervention ID), \code{r_i} (number of patients who quit),
#'     \code{n_i} (total patients in arm), and \code{b_i} (reference
#'     intervention for the study).}
#'   \item{smoking_output}{A matrix of results from the network meta-analysis model
#'     run on the \code{smoking} object.}
#'   \item{treats}{A character vector of labels for the four strategies.}
#' }
#' @references Baio G. (2012). *Bayesian Methods in Health Economics*. CRC/Chapman & Hall, London.
#' @source
#' Effectiveness data adapted from Hasselblad V. (1998). "Meta-analysis
#' of Multitreatment Studies". *Medical Decision Making*, 18:37-43.
#'
#' Cost and population data adapted from various sources:
#' \itemize{
#'   \item Taylor, D.H. Jr, et al. (2002). "Benefits of smoking
#'     cessation on longevity". *American Journal of Public Health*, 92(6).
#'   \item Action on Smoking and Health (ASH) (2013). "ASH fact sheet on smoking
#'     statistics". \url{https://ash.org/wp-content/uploads/2014/05/ASH-Annual-Report-2014.pdf}.
#'   \item Flack, S., et al. (2007). "Cost-effectiveness of interventions for smoking
#'     cessation". *York Health Economics Consortium*.
#'   \item McGhan, W.F.D., and Smith, M. (1996). "Pharmacoeconomic analysis of
#'     smoking-cessation interventions". *American Journal of Health-System Pharmacy*, 53:45-52.
#' }
#' @aliases Smoking cost data eff life.years pi_post smoking smoking_output treats
#' @keywords datasets
NULL

#' @title Influenza Vaccination Cost-Effectiveness Data
#' @name Vaccine
#'
#' @description This data set contains the results of a Bayesian analysis modeling the
#' clinical outputs and costs associated with an influenza vaccination program.
#'
#' @format A list containing variables for the influenza vaccination model:
#' \describe{
#'   \item{cost}{A matrix of simulations of the overall costs for the two treatments.}
#'   \item{c.pts}{Coordinates for plotting cost distributions.}
#'   \item{cost.GP}{A matrix of simulations of the costs for GP visits.}
#'   \item{cost.hosp}{A matrix of simulations of the costs for hospitalisations.}
#'   \item{cost.otc}{A matrix of simulations of the costs for over-the-counter medications.}
#'   \item{cost.time.off}{A matrix of simulations of the costs for time off work.}
#'   \item{cost.time.vac}{A matrix of simulations of the costs for time to get vaccinated.}
#'   \item{cost.travel}{A matrix of simulations of the costs for travel to get vaccinated.}
#'   \item{cost.trt1}{A matrix of simulations of the overall costs for first-line treatment.}
#'   \item{cost.trt2}{A matrix of simulations of the overall costs for second-line treatment.}
#'   \item{cost.vac}{A matrix of simulations of the costs for vaccination.}
#'   \item{eff}{A matrix of simulations of the clinical benefits.}
#'   \item{e.pts}{Coordinates for plotting effectiveness distributions.}
#'   \item{N}{The number of subjects in the reference population.}
#'   \item{N.outcomes}{The number of clinical outcomes analysed.}
#'   \item{N.resources}{The number of health-care resources under study.}
#'   \item{QALYs.adv}{A vector of QALYs associated with adverse events.}
#'   \item{QALYs.death}{A vector of QALYs associated with death.}
#'   \item{QALYs.hosp}{A vector of QALYs associated with hospitalisation.}
#'   \item{QALYs.inf}{A vector of QALYs associated with influenza infection.}
#'   \item{QALYs.pne}{A vector of QALYs associated with pneumonia.}
#'   \item{treats}{A character vector of labels for the two treatments.}
#'   \item{vaccine_mat}{A matrix of simulations for the parameters in the original model.}
#' }
#' @references Baio, G., & Dawid, A. P. (2011). "Probabilistic Sensitivity
#'   Analysis in Health Economics". *Statistical Methods in Medical Research*.
#'   doi:10.1177/0962280211419832.
#' @source Adapted from Turner D, et al. (2006). "The cost-effectiveness of
#'   influenza vaccination of healthy adults 50-64 years of age". *Vaccine*, 24:1035-1043.
#' @aliases c.pts cost.GP cost.hosp cost.otc cost.time.off cost.time.vac cost.travel cost.trt1 cost.trt2 cost.vac e.pts N N.outcomes N.resources QALYs.adv QALYs.death QALYs.hosp QALYs.inf QALYs.pne vaccine_mat
#' @keywords datasets
NULL

#' @title Statins Evidence-Synthesis Data (Base Model)
#' @name statins_base
#' 
#' @description This data set contains the results of a Bayesian analysis used to model
#' the effectiveness of various statins. The analysis is based on the simplest
#' model, using vague priors.
#'
#' @format A `BUGS` object containing the simulations for the evidence synthesis model.
#' @references Baio G. (2012). *Bayesian Methods in Health Economics*. CRC/Chapman & Hall, London.
#' @source A systematic review and economic evaluation of statins for the
#'   prevention of coronary events. Ward 2007.
#' @keywords datasets
#' @aliases statins_base
#' @examples
#' data(statins_base)
#' lapply(statins_base$sims.list, summary)
NULL


#' @title Statins Evidence-Synthesis Data (Robust Model)
#' @name statins_HC
#' 
#' @description This data set contains the results of a Bayesian analysis used to model
#' the effectiveness of various statins. The analysis uses robust Half-Cauchy
#' priors for the structured effects standard deviations.
#'
#' @format A `BUGS` object containing the simulations for the evidence synthesis model.
#' @references Baio G. (2012). *Bayesian Methods in Health Economics*. CRC/Chapman & Hall, London.
#' @source A systematic review and economic evaluation of statins for the
#'   prevention of coronary events. Ward 2007.
#' @keywords datasets
#' @aliases statins_HC
#' @examples
#' data(statins_HC)
#' lapply(statins_HC$sims.list, summary)
NULL
