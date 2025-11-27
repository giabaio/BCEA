# Diagnostic Plots For The Results Of The EVPPI

The function produces either a residual plot comparing the fitted values
from the INLA-SPDE Gaussian Process regression to the residuals. This is
a scatter plot of residuals on the y axis and fitted values (estimated
responses) on the x axis. The plot is used to detect non-linearity,
unequal error variances, and outliers. A well-behaved residual plot
supporting the appropriateness of the simple linear regression model has
the following characteristics:

1.  The residuals bounce randomly around the 0 line. This suggests that
    the assumption that the relationship is linear is reasonable.

2.  The residuals roughly form a horizontal band around the 0 line. This
    suggests that the variances of the error terms are equal.

3.  None of the residual stands out from the basic random pattern of
    residuals. This suggests that there are no outliers.

## Usage

``` r
diag.evppi(evppi, he, plot_type = c("residuals", "qqplot"), interv = 1)
```

## Arguments

- evppi:

  A `evppi` object obtained by running the function `evppi` on a `bcea`
  model.

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- plot_type:

  The type of diagnostics to be performed. It can be the 'residual plot'
  (`residuals`) or the Q-Q (quantile-quantile) plot (`qqplot`).

- interv:

  Specifies the interventions for which diagnostic tests should be
  performed (if there are many options being compared)

## Value

Plot

## Details

The second possible diagnostic is the Q-Q plot for the fitted value.
This is a graphical method for comparing the fitted values distributions
with the assumed underlying normal distribution by plotting their
quantiles against each other. First, the set of intervals for the
quantiles is chosen. A point (x,y) on the plot corresponds to one of the
quantiles of the second distribution (y-coordinate) plotted against the
same quantile of the first distribution (x-coordinate). If the two
distributions being compared are identical, the Q-Q plot follows the 45
degrees line.

## References

Baio G, Dawid AP (2011). “Probabilistic sensitivity analysis in health
economics.” *Stat. Methods Med. Res.*, 1–20. ISSN 1477-0334,
[doi:10.1177/0962280211419832](https://doi.org/10.1177/0962280211419832)
, <https://pubmed.ncbi.nlm.nih.gov/21930515/>.

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md),
[`evppi()`](https://n8thangreen.github.io/BCEA/reference/evppi.md)

## Author

Gianluca Baio, Anna Heath
