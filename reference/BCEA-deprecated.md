# Deprecated functions in package BCEA.

The functions listed below are deprecated and will be defunct in the
near future. When possible, alternative functions with similar
functionality are also mentioned. Help pages for deprecated functions
are available at `help("<function>-deprecated")`.

This function is deprecated. Use
[`ceac.plot()`](https://n8thangreen.github.io/BCEA/reference/ceac.plot.md)
instead. Plots the probability that each of the n_int interventions
being analysed is the most cost-effective.

## Usage

``` r
make.report(...)

mce.plot(mce, pos = c(1, 0.5), graph = c("base", "ggplot2"), ...)

plot.mixedAn(x, y.limits=NULL, pos=c(0,1), graph=c("base","ggplot2"),...)
```

## Arguments

- ...:

  Arguments to be passed to methods, such as graphical parameters (see
  [`par()`](https://rdrr.io/r/graphics/par.html)).

- mce:

  The output of the call to the function
  [`multi.ce()`](https://n8thangreen.github.io/BCEA/reference/multi.ce.md).

- pos:

  Parameter to set the position of the legend. Can be given in form of a
  string `(bottom|top)(right|left)` for base graphics and
  `bottom|top|left|right` for ggplot2. It can be a two-elements vector,
  which specifies the relative position on the x and y axis
  respectively, or alternatively it can be in form of a logical
  variable, with `FALSE` indicating to use the default position and
  `TRUE` to place it on the bottom of the plot. Default value is
  `c(0,1)`, that is in the topleft corner inside the plot area.

- graph:

  A string used to select the graphical engine to use for plotting.
  Should (partial-)match the two options `"base"` or `"ggplot2"`.
  Default value is `"base"`. The `"plotly"` option is not implemented
  for this particular graph.

- x:

  An object of class `mixedAn`, given as output of the call to the
  function
  [`mixedAn()`](https://n8thangreen.github.io/BCEA/reference/mixedAn-set.md).

- y.limits:

  Range of the y-axis for the graph. The default value is `NULL`, in
  which case the maximum range between the optimal and the mixed
  analysis scenarios is considered.

## Value

- mceplot:

  A ggplot object containing the plot. Returned only if
  `graph="ggplot2"`.

- evi:

  A ggplot object containing the plot. Returned only if
  `graph="ggplot2"`.

The function produces a graph showing the difference between the
”optimal” version of the EVPI (when only the most cost-effective
intervention is included in the market) and the mixed strategy one (when
more than one intervention is considered in the market).

## `plot.mixedAn`

For `plot.mixedAn`, use
[`evi.plot()`](https://n8thangreen.github.io/BCEA/reference/evi.plot.md).

Summary plot of the health economic analysis when the mixed analysis is
considered

Compares the optimal scenario to the mixed case in terms of the EVPI.

## References

Baio G, Dawid aP (2011). “Probabilistic sensitivity analysis in health
economics.” *Stat. Methods Med. Res.*, 1–20. ISSN 1477-0334,
[doi:10.1177/0962280211419832](https://doi.org/10.1177/0962280211419832)
, <https://pubmed.ncbi.nlm.nih.gov/21930515/>.

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

`BCEA-deprecated()`

## Author

Gianluca Baio, Andrea Berardi

## Examples

``` r
# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

if (FALSE) { # \dontrun{
# Load the processed results of the MCMC simulation model
data(Vaccine)
# 
# Runs the health economic evaluation using BCEA
m <- bcea(e=eff, c=cost,    # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e,c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
      plot=FALSE            # inhibits graphical output
)
#
mce <- multi.ce(m)          # uses the results of the economic analysis 
#
mce.plot(mce,               # plots the probability of being most cost-effective
      graph="base")         #  using base graphics
#
if(require(ggplot2)){
mce.plot(mce,               # the same plot
      graph="ggplot2")      #  using ggplot2 instead
}
} # }
```
