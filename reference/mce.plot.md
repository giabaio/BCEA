# Plots the probability that each intervention is the most cost-effective

This function is deprecated. Use
[`ceac.plot()`](https://n8thangreen.github.io/BCEA/reference/ceac.plot.md)
instead. Plots the probability that each of the n_int interventions
being analysed is the most cost-effective.

## Usage

``` r
mce.plot(mce, pos = c(1, 0.5), graph = c("base", "ggplot2"), ...)
```

## Arguments

- mce:

  The output of the call to the function
  [`multi.ce()`](https://n8thangreen.github.io/BCEA/reference/multi.ce.md).

- pos:

  Parameter to set the position of the legend. Can be given in form of a
  string `(bottom|top)(right|left)` for base graphics and
  `bottom|top|left|right` for ggplot2. It can be a two-elements vector,
  which specifies the relative position on the x and y axis
  respectively, or alternatively it can be in form of a logical
  variable, with `TRUE` indicating to use the first standard and `FALSE`
  to use the second one. Default value is `c(1,0.5)`, that is on the
  right inside the plot area.

- graph:

  A string used to select the graphical engine to use for plotting.
  Should (partial-)match the two options `"base"` or `"ggplot2"`.
  Default value is `"base"`.

- ...:

  Optional arguments. For example, it is possible to specify the colours
  to be used in the plot. This is done in a vector `color=c(...)`. The
  length of the vector colors needs to be the same as the number of
  comparators included in the analysis, otherwise `BCEA` will fall back
  to the default values (all black, or shades of grey)

## Value

- mceplot:

  A ggplot object containing the plot. Returned only if
  `graph="ggplot2"`.

## References

Baio G, Dawid aP (2011). “Probabilistic sensitivity analysis in health
economics.” *Stat. Methods Med. Res.*, 1–20. ISSN 1477-0334,
[doi:10.1177/0962280211419832](https://doi.org/10.1177/0962280211419832)
, <https://pubmed.ncbi.nlm.nih.gov/21930515/>.

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

[`BCEA-deprecated()`](https://n8thangreen.github.io/BCEA/reference/BCEA-deprecated.md)

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
