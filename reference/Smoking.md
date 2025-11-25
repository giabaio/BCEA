# Smoking Cessation Cost-Effectiveness Data

This data set contains the results of a Bayesian analysis modeling the
clinical outputs and costs for an economic evaluation of four different
smoking cessation interventions.

## Format

A list containing the variables for the cost-effectiveness analysis:

- cost:

  A matrix of 500 simulations from the posterior distribution of the
  overall costs for the four strategies.

- data:

  A dataset with characteristics of smokers in the UK population.

- eff:

  A matrix of 500 simulations from the posterior distribution of the
  clinical benefits for the four strategies.

- life.years:

  A matrix of 500 simulations from the posterior distribution of the
  life years gained with each strategy.

- pi_post:

  A matrix of 500 simulations from the posterior distribution of the
  probability of smoking cessation with each strategy.

- smoking:

  A data frame with inputs for the network meta-analysis, containing:
  `nobs` (record ID), `s` (study ID), `i` (intervention ID), `r_i`
  (number of patients who quit), `n_i` (total patients in arm), and
  `b_i` (reference intervention for the study).

- smoking_output:

  A matrix of results from the network meta-analysis model run on the
  `smoking` object.

- treats:

  A character vector of labels for the four strategies.

## Source

Effectiveness data adapted from Hasselblad V. (1998). "Meta-analysis of
Multitreatment Studies". *Medical Decision Making*, 18:37-43.

Cost and population data adapted from various sources:

- Taylor, D.H. Jr, et al. (2002). "Benefits of smoking cessation on
  longevity". *American Journal of Public Health*, 92(6).

- Action on Smoking and Health (ASH) (2013). "ASH fact sheet on smoking
  statistics".
  <https://ash.org/wp-content/uploads/2014/05/ASH-Annual-Report-2014.pdf>.

- Flack, S., et al. (2007). "Cost-effectiveness of interventions for
  smoking cessation". *York Health Economics Consortium*.

- McGhan, W.F.D., and Smith, M. (1996). "Pharmacoeconomic analysis of
  smoking-cessation interventions". *American Journal of Health-System
  Pharmacy*, 53:45-52.

## References

Baio G. (2012). *Bayesian Methods in Health Economics*. CRC/Chapman &
Hall, London.
