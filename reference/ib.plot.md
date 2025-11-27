# Incremental Benefit (IB) Distribution Plot

Plots the distribution of the Incremental Benefit (IB) for a given value
of the willingness to pay threshold.

## Usage

``` r
# S3 method for class 'bcea'
ib.plot(
  he,
  comparison = NULL,
  wtp = 25000,
  bw = "bcv",
  n = 512,
  xlim = NULL,
  graph = options("bcea.graph"),
  ...
)

ib.plot(he, ...)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- comparison:

  In the case of multiple interventions, specifies the one to be used in
  comparison with the reference. Default value of `NULL` forces R to
  consider the first non-reference intervention as the comparator.
  Controls which comparator is used when more than 2 interventions are
  present

- wtp:

  The value of the willingness to pay threshold. Default value at
  `25000`.

- bw:

  Identifies the smoothing bandwidth used to construct the kernel
  estimation of the IB density.

- n:

  The number of equally spaced points at which the density is to be
  estimated.

- xlim:

  The limits of the plot on the x-axis.

- graph:

  A string used to select the graphical engine to use for plotting.
  Should (partial-) match the three options `"base"`, `"ggplot2"` or
  `"plotly"`. Default value is `"base"`. This is set globally upon
  loading `BCEA` and can be modified for instance by using
  `options("bcea.graph"="gg")`, or `options("bcea.graph="plotly")`.
  Partial matching still applies (so `gg` or `pl` also work). Not all
  plotting functions have a `"plotly"` implementation, yet – see help
  for the specifics.

- ...:

  Additional arguments

## Value

- ib:

  A ggplot object containing the requested plot. Returned only if
  `graph="ggplot2"`.

The function produces a plot of the distribution of the Incremental
Benefit for a given value of the willingness to pay parameter. The
dashed area indicates the positive part of the distribution (i.e. when
the reference is more cost-effective than the comparator).

## References

Baio G, Dawid AP (2011). “Probabilistic sensitivity analysis in health
economics.” *Stat. Methods Med. Res.*, 1–20. ISSN 1477-0334,
[doi:10.1177/0962280211419832](https://doi.org/10.1177/0962280211419832)
, <https://pubmed.ncbi.nlm.nih.gov/21930515/>.

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md),
[`ceplane.plot()`](https://n8thangreen.github.io/BCEA/reference/ceplane.plot.md)

## Author

Gianluca Baio, Andrea Berardi

## Examples

``` r
data("Vaccine")
he <- BCEA::bcea(eff, cost)
#> No reference selected. Defaulting to first intervention.
ib.plot(he)

```
