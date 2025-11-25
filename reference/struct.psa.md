# Structural Probability Sensitivity Analysis

Computes the weights to be associated with a set of competing models in
order to perform structural PSA.

## Usage

``` r
struct.psa(
  models,
  effect,
  cost,
  ref = NULL,
  interventions = NULL,
  Kmax = 50000,
  plot = FALSE,
  w = NULL
)
```

## Arguments

- models:

  A (possibly named) list containing the output from either R2jags or
  R2WinBUGS for all the models that need to be combined in the model
  average

- effect:

  A list containing the measure of effectiveness computed from the
  various models (one matrix with `n_sim` x `n_ints` simulations for
  each model)

- cost:

  A list containing the measure of costs computed from the various
  models (one matrix with `n_sim` x `n_ints` simulations for each model)

- ref:

  Which intervention is considered to be the reference strategy. The
  default value `ref=1` means that the intervention appearing first is
  the reference and the other(s) is(are) the comparator(s)

- interventions:

  Defines the labels to be associated with each intervention. By default
  and if `NULL`, assigns labels in the form "Intervention1", ... ,
  "InterventionT"

- Kmax:

  Maximum value of the willingness to pay to be considered. Default
  value is `50000`. The willingness to pay is then approximated on a
  discrete grid in the interval `[0, Kmax]`. The grid is equal to `k` if
  the parameter is given, or composed of `501` elements if `k=NULL` (the
  default)

- plot:

  A logical value indicating whether the function should produce the
  summary plot or not

- w:

  A vector of weights. By default it's `NULL` to indicate that the
  function will calculate the model weights based on DIC and the
  individual model fit. This behaviour can be overridden by passing a
  vector `w`, for instance based on expert opinion

## Value

List object of bcea object, model weights and DIC

## Details

The model is a list containing the output from either R2jags or
R2WinBUGS for all the models that need to be combined in the model
average effect is a list containing the measure of effectiveness
computed from the various models (one matrix with `n_sim` x `n_ints`
simulations for each model) cost is a list containing the measure of
costs computed from the various models (one matrix with `n_sim` x
`n_ints` simulations for each model).

## References

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md)

## Author

Gianluca Baio

## Examples

``` r
# load sample jags output
data("statins_base")
data("statins_HC")

interventions <- c("Atorvastatin", "Fluvastatin",
                   "Lovastatin", "Pravastatin",
                   "Rosuvastatin", "Simvastatin")

m1 <- bcea(eff = statins_base$sims.list$effect,
           cost = statins_base$sims.list$cost.tot,
           ref = 1, interventions = interventions)

m2 <- bcea(eff = statins_HC$sims.list$effect,
           cost = statins_HC$sims.list$cost.tot,
           ref = 1, interventions = interventions)

models <- list("Base"=statins_base, "Half Cauchy"=statins_HC)

effects <- list(statins_base$sims.list$effect,
                statins_HC$sims.list$effect)
costs <- list(statins_base$sims.list$cost.tot,
              statins_HC$sims.list$cost.tot)

if (FALSE) { # \dontrun{
m3 <- struct.psa(models, effects, costs,
                 ref = 1, interventions = interventions)
} # }
```
