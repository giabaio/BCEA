# Information-Rank Plot for bcea Class

Produces a plot similar to a tornado plot, but based on the analysis of
the EVPPI. For each parameter and value of the willingness-to-pay
threshold, a barchart is plotted to describe the ratio of EVPPI
(specific to that parameter) to EVPI. This represents the relative
\`importance' of each parameter in terms of the expected value of
information.

## Usage

``` r
# S3 method for class 'bcea'
info.rank(
  he,
  inp,
  wtp = NULL,
  howManyPars = NA,
  graph = options("bcea.graph"),
  rel = TRUE,
  ...
)

info.rank(he, ...)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- inp:

  Named list from running
  [`createInputs()`](https://n8thangreen.github.io/BCEA/reference/createInputs.md)
  containing:

  - `parameter` = A vector of parameters for which the individual EVPPI
    should be calculated. This can be given as a string (or vector of
    strings) of names or a numeric vector, corresponding to the column
    numbers of important parameters.

  - `mat` = A matrix containing the simulations for all the parameters
    monitored by the call to JAGS or BUGS. The matrix should have column
    names matching the names of the parameters and the values in the
    vector parameter should match at least one of those values.

- wtp:

  A value of the wtp for which the analysis should be performed. If not
  specified then the break-even point for the current model will be
  used.

- howManyPars:

  Optional maximum number of parameters to be included in the bar plot.
  Includes all parameters by default.

- graph:

  A string used to select the graphical engine to use for plotting.
  Should (partial-) match the three options `"base"`, `"ggplot2"` or
  `"plotly"`. Default value is `"ggplot2"`. This is set globally upon
  loading `BCEA` and can be modified for instance by using
  `options("bcea.graph"="base")`, or `options("bcea.graph="plotly")`.
  Partial matching still applies (so `gg`, or `g`, or `pl`, or `p` also
  work). Not all plotting functions have a `"plotly"` implementation,
  yet – see the help for the specific functions.

- rel:

  Logical argument that specifies whether the ratio of EVPPI to EVPI
  (`rel = TRUE`, default) or the absolute value of the EVPPI should be
  used for the analysis.

- ...:

  Additional options. These include graphical parameters that the user
  can specify:

  - `xlim` = limits of the x-axis; ca = font size for the axis label
    (default = 0.7 of full size).

  - `cn` = font size for the parameter names vector (default = 0.7 of
    full size) - base graphics only.

  - `mai` = margins of the graph (default = c(1.36, 1.5, 1,1)) - base
    graphics only.

## Value

With base graphics: A data.frame containing the ranking of the
parameters with the value of the selected summary, for the chosen wtp;
with plotly: a plotly object, incorporating in the \$rank element the
data.frame as above. The function produces a 'Info-rank' plot. This is
an extension of standard 'Tornado plots' and presents a ranking of the
model parameters in terms of their impact on the expected value of
information. For each parameter, the specific individual EVPPI is
computed and used to measure the impact of uncertainty in that parameter
over the decision-making process, in terms of how large the expected
value of gaining more information is.

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

Anna Heath, Gianluca Baio, Andrea Berardi

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the post-processed results of the MCMC simulation model
# original JAGS output is can be downloaded from here
# https://gianluca.statistica.it/books/bcea/code/vaccine.RData

data("Vaccine")
m <- bcea(eff, cost)
inp <- createInputs(vaccine_mat)
info.rank(m, inp)

info.rank(m, inp, graph = "base")
info.rank(m, inp, graph = "plotly")
info.rank(m, inp, graph = "ggplot2")
} # }
```
