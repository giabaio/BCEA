
#' Data set for the Bayesian model for the cost-effectiveness of smoking
#' cessation interventions
#' 
#' This data set contains the results of the Bayesian analysis used to model
#' the clinical output and the costs associated with the health economic
#' evaluation of four different smoking cessation interventions.
#' 
#' @name Smoking
#' @docType data
#' @aliases Smoking cost data eff life.years pi_post smoking smoking_output treats
#' @format A data list including the variables needed for the smoking cessation
#' cost-effectiveness analysis. The variables are as follows:
#' \describe{
#' \item{list("cost")}{a matrix of 500 simulations from the posterior distribution
#'  of the overall costs associated with the four strategies}
#' \item{list("data")}{a dataset containing the characteristics of the smokers
#'  in the UK population}
#' \item{list("eff")}{a matrix of 500 simulations from the
#'  posterior distribution of the clinical benefits associated with the four
#'  strategies}
#' \item{list("life.years")}{a matrix of 500 simulations from the
#'  posterior distribution of the life years gained with each strategy}
#' \item{list("pi_post")}{a matrix of 500 simulations from the posterior
#'  distribution of the event of smoking cessation with each strategy}
#' \item{list("smoking")}{a data frame containing the inputs needed for the
#'  network meta-analysis model. The `data.frame` object contains:
#'  `nobs`: the record ID number, `s`: the study ID number, `i`:
#'  the intervention ID number, `r_i`: the number of patients who quit
#'  smoking, `n_i`: the total number of patients for the row-specific arm
#'  and `b_i`: the reference intervention for each study}
#' \item{list("smoking_mat")}{a matrix obtained by running the network
#'  meta-analysis model based on the data contained in the `smoking` object}
#' \item{list("treats")}{a vector of labels associated with the four strategies}
#' }
#' @references Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
#' 
#' @source Effectiveness data adapted from Hasselblad V. (1998). Meta-analysis
#' of Multitreatment Studies. Medical Decision Making 1998;18:37-43.
#' Cost and population characteristics data adapted from various sources:
#' \itemize{
#'    \item Taylor, D.H. Jr, et al. (2002). Benefits of smoking
#'    cessation on longevity. American Journal of Public Health 2002;92(6)
#'    \item ASH: Action on Smoking and Health (2013). ASH fact sheet on smoking
#'    statistics, \cr `https://ash.org.uk/files/documents/ASH_106.pdf`
#'    \item Flack, S., et al. (2007). Cost-effectiveness of interventions for smoking
#'    cessation. York Health Economics Consortium, January 2007
#'    \item McGhan, W.F.D., and Smith, M. (1996). Pharmacoeconomic analysis of
#'    smoking-cessation interventions. American Journal of Health-System Pharmacy
#'    1996;53:45-52}
#' @keywords datasets
NULL



#' Data set for the Bayesian model for the cost-effectiveness of influenza
#' vaccination
#' 
#' This data set contains the results of the Bayesian analysis used to model
#' the clinical output and the costs associated with an influenza vaccination.
#' 
#' @name Vaccine
#' @docType data
#' @aliases Vaccine c.pts cost.GP cost.hosp cost.otc cost.time.off cost.time.vac
#' cost.travel cost.trt1 cost.trt2 cost.vac e.pts N N.outcomes N.resources
#' QALYs.adv QALYs.death QALYs.hosp QALYs.inf QALYs.pne vaccine vaccine_mat
#' @format A data list including the variables needed for the influenza
#' vaccination. The variables are as follows:
#' \describe{
#'   \item{list("cost")}{a matrix of simulations from the posterior
#'   distribution of the overall costs associated with the two treatments}
#'   \item{list("c.pts")}{}
#'   \item{list("cost.GP")}{a matrix of simulations from the posterior
#'   distribution of the costs for GP visits associated with the two treatments}
#'   \item{list("cost.hosp")}{a matrix of simulations from the posterior
#'   distribution of the costs for hospitalisations associated with the two
#'   treatments}
#'   \item{list("cost.otc")}{a matrix of simulations from the posterior distribution
#'   of the costs for over-the-counter medications associated with the two treatments}
#'   \item{list("cost.time.off")}{a matrix of simulations from the posterior
#'   distribution of the costs for time off work associated with the two treatments}
#'   \item{list("cost.time.vac")}{a matrix of simulations from the posterior
#'   distribution of the costs for time needed to get the vaccination associated
#'   with the two treatments}
#'   \item{list("cost.travel")}{a matrix of simulations from the posterior
#'   distribution of the costs for travel to get vaccination associated with the
#'   two treatments}
#'   \item{list("cost.trt1")}{a matrix of simulations from the
#'   posterior distribution of the overall costs for first line of treatment
#'   associated with the two interventions}
#'   \item{list("cost.trt2")}{a matrix of simulations from the posterior distribution
#'   of the overall costs for second line of treatment associated with the two
#'   interventions}
#'   \item{list("cost.vac")}{a matrix of simulations from the posterior
#'   distribution of the costs for vaccination}
#'   \item{list("eff")}{a matrix of simulations from the posterior distribution of
#'   the clinical benefits associated with the two treatments}
#'   \item{list("e.pts")}{}
#'   \item{list("N")}{the number of subjects in the reference population}
#'   \item{list("N.outcomes")}{the number of clinical outcomes analysed}
#'   \item{list("N.resources")}{the number of health-care resources under study}
#'   \item{list("QALYs.adv")}{a vector from the posterior distribution of the
#'   QALYs associated with advert events}
#'   \item{list("QALYs.death")}{a vector from the posterior distribution of the
#'   QALYs associated with death}
#'   \item{list("QALYs.hosp")}{a vector from the posterior distribution of the
#'   QALYs associated with hospitalisation}
#'   \item{list("QALYs.inf")}{a vector from the posterior distribution of the
#'   QALYs associated with influenza infection}
#'   \item{list("QALYs.pne")}{a vector from the posterior distribution of the
#'   QALYs associated with pneumonia}
#'   \item{list("treats")}{a vector of labels associated with the two treatments}
#'   \item{list("vaccine_mat")}{a matrix containing the simulations for the
#'   parameters used in the original model}
#'   }
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics. Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' @source Adapted from Turner D, Wailoo A, Cooper N, Sutton A, Abrams K,
#' Nicholson K. The cost-effectiveness of influenza vaccination of healthy
#' adults 50-64 years of age. Vaccine. 2006;24:1035-1043.
#' @keywords datasets
NULL

#' Data set for the Bayesian model for the evidence-synthesis of statins
#' (base model)
#' 
#' This data set contains the results of the Bayesian analysis used to model
#' the effectiveness of various statins. The analysis is based on the simplest
#' model, using vague priors.
#' 
#' @name statins_base
#' @docType data
#' @aliases Statins base model
#' @format A BUGS object containing the simulations for the evidence synthesis
#' model
#' \describe{
#' \item{list("statins_base")}{a BUGS object}
#' @references Baio G. (2012). Bayesian Methods in Health Economics. 
#' CRC/Chapman Hall, London
#' 
#' @source A systematic review and economic evaluation of statins for the 
#' prevention of coronary events. Ward 2007.
#' @keywords datasets
NULL

#' Data set for the Bayesian model for the evidence-synthesis of statins
#' (robust model using Half Cauchy priors for the structured effects standard
#' deviations)
#' 
#' This data set contains the results of the Bayesian analysis used to model
#' the effectiveness of various statins. The analysis is based on the simplest
#' model, using robust priors.
#' 
#' @name statins_HC
#' @docType data
#' @aliases Statins robust model
#' @format A BUGS object containing the simulations for the evidence synthesis
#' model
#' \describe{
#' \item{list("statins_HC")}{a BUGS object}
#' @references Baio G. (2012). Bayesian Methods in Health Economics. 
#' CRC/Chapman Hall, London
#' 
#' @source A systematic review and economic evaluation of statins for the 
#' prevention of coronary events. Ward 2007.
#' @keywords datasets
NULL
