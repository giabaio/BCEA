# Prepare frontier data

Prepare frontier data

## Usage

``` r
prep_frontier_data(he, threshold = NULL, start.origin = TRUE)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- threshold:

  Cost-effectiveness threshold i.e angle of line. Must be \>=0 or NULL.

- start.origin:

  Where should the frontier start from?

## Value

List with scatter.data, ceef.points, orig.avg

## See also

ceef.plot
