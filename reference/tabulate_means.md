# Calculate Dataset For ICERs From bcea Object

Calculate Dataset For ICERs From bcea Object

## Usage

``` r
tabulate_means(he, comp_label = NULL, ...)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- comp_label:

  Optional vector of strings with comparison labels

- ...:

  Additional arguments

## Value

A data.frame object including mean outcomes, comparison identifier,
comparison label and associated ICER

## Examples

``` r
data("Smoking")
he <- BCEA::bcea(eff, cost)
#> No reference selected. Defaulting to first intervention.
tabulate_means(he)
#>                  lambda.e   lambda.c comparison label     ICER
#> intervention 2 -0.2882396  -45.73316          1     1 158.6637
#> intervention 3 -0.4848577  -94.91904          2     2 195.7668
#> intervention 4 -0.7225198 -143.30076          3     3 198.3347
```
