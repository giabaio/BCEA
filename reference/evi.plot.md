# Expected Value of Information (EVI) Plot

Plots the Expected Value of Information (EVI) against the willingness to
pay.

## Usage

``` r
# S3 method for class 'bcea'
evi.plot(he, graph = options("bcea.graph"), ...)

evi.plot(he, ...)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- graph:

  A string used to select the graphical engine to use for plotting.
  Should (partial-) match the three options `"base"`, `"ggplot2"` or
  `"plotly"`. Default value is `"ggplot2"`. This is set globally upon
  loading `BCEA` and can be modified for instance by using
  `options("bcea.graph"="base")`, or `options("bcea.graph="plotly")`.
  Partial matching still applies (so `gg`, or `g`, or `pl`, or `p` also
  work). Not all plotting functions have a `"plotly"` implementation,
  yet – see the help for the specific functions.

- ...:

  Additional graphical arguments:

  - `line_colors` to specify the EVPI line colour - all graph types.

  - `line_types` to specify the line type (lty) - all graph types.

  - `area_include` to specify whether to include the area under the EVPI
    curve - plotly only.

  - `area_color` to specify the area under the colour curve - plotly
    only.

## Value

- eib:

  If `graph="ggplot2"` a ggplot object, or if `graph="plotly"` a plotly
  object containing the requested plot. Nothing is returned when
  `graph="base"`, the default.

The function produces a plot of the Expected Value of Information as a
function of the discrete grid approximation of the willingness to pay
parameter. The break even point(s) (i.e. the point in which the EIB=0,
ie when the optimal decision changes from one intervention to another)
is(are) also showed.

## References

Baio G, Dawid AP (2011). “Probabilistic sensitivity analysis in health
economics.” *Stat. Methods Med. Res.*, 1–20. ISSN 1477-0334,
[doi:10.1177/0962280211419832](https://doi.org/10.1177/0962280211419832)
, <https://pubmed.ncbi.nlm.nih.gov/21930515/>.

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md),
[`ceac.plot()`](https://n8thangreen.github.io/BCEA/reference/ceac.plot.md),
[`ceplane.plot()`](https://n8thangreen.github.io/BCEA/reference/ceplane.plot.md)

## Author

Gianluca Baio, Andrea Berardi

## Examples

``` r
data(Vaccine)
m <- bcea(
      e=eff,
      c=cost,               # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e, c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0, Kmax)
      plot=FALSE            # plots the results
)
evi.plot(m)


data(Smoking)
treats <- c("No intervention", "Self-help",
            "Individual counselling", "Group counselling")
m <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)
evi.plot(m)

```
