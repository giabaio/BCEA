# Influenza Vaccination Cost-Effectiveness Data

This data set contains the results of a Bayesian analysis modeling the
clinical outputs and costs associated with an influenza vaccination
program.

## Format

A list containing variables for the influenza vaccination model:

- cost:

  A matrix of simulations of the overall costs for the two treatments.

- c.pts:

  Coordinates for plotting cost distributions.

- cost.GP:

  A matrix of simulations of the costs for GP visits.

- cost.hosp:

  A matrix of simulations of the costs for hospitalisations.

- cost.otc:

  A matrix of simulations of the costs for over-the-counter medications.

- cost.time.off:

  A matrix of simulations of the costs for time off work.

- cost.time.vac:

  A matrix of simulations of the costs for time to get vaccinated.

- cost.travel:

  A matrix of simulations of the costs for travel to get vaccinated.

- cost.trt1:

  A matrix of simulations of the overall costs for first-line treatment.

- cost.trt2:

  A matrix of simulations of the overall costs for second-line
  treatment.

- cost.vac:

  A matrix of simulations of the costs for vaccination.

- eff:

  A matrix of simulations of the clinical benefits.

- e.pts:

  Coordinates for plotting effectiveness distributions.

- N:

  The number of subjects in the reference population.

- N.outcomes:

  The number of clinical outcomes analysed.

- N.resources:

  The number of health-care resources under study.

- QALYs.adv:

  A vector of QALYs associated with adverse events.

- QALYs.death:

  A vector of QALYs associated with death.

- QALYs.hosp:

  A vector of QALYs associated with hospitalisation.

- QALYs.inf:

  A vector of QALYs associated with influenza infection.

- QALYs.pne:

  A vector of QALYs associated with pneumonia.

- treats:

  A character vector of labels for the two treatments.

- vaccine_mat:

  A matrix of simulations for the parameters in the original model.

## Source

Adapted from Turner D, et al. (2006). "The cost-effectiveness of
influenza vaccination of healthy adults 50-64 years of age". *Vaccine*,
24:1035-1043.

## References

Baio, G., & Dawid, A. P. (2011). "Probabilistic Sensitivity Analysis in
Health Economics". *Statistical Methods in Medical Research*.
doi:10.1177/0962280211419832.
