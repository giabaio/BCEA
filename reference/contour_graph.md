# Contour Cost-Effectiveness Plane

Choice of base R, ggplot2.

## Usage

``` r
contour_base(he, pos_legend, graph_params, ...)

contour_ggplot(he, pos_legend, graph_params, ...)

contour_plotly(he, pos_legend, graph_params, ...)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- pos_legend:

  Legend position

- graph_params:

  Plot parameters; list

- ...:

  Additional arguments

## Value

For plotly returns a plot in the Viewer

## See also

[`contour()`](https://n8thangreen.github.io/BCEA/reference/contour.md)
