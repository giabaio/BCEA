# Set Comparisons Group

One of the alternative way to set (e,c) comparison group. Simply
recompute all comparisons and drop unwanted.

## Usage

``` r
setComparisons(he, comparison)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- comparison:

  Selects the comparator, in case of more than two interventions being
  analysed. Default as NULL plots all the comparisons together. Any
  subset of the possible comparisons can be selected (e.g.,
  `comparison=c(1,3)` or `comparison=2`).

## See also

[`setComparisons<-()`](https://n8thangreen.github.io/BCEA/reference/setComparisons_assign.md)
