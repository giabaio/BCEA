# Contour Plots for the Cost-Effectiveness Plane

Contour method for objects in the class `bcea`. Produces a scatterplot
of the cost-effectiveness plane, with a contour-plot of the bivariate
density of the differentials of cost (y-axis) and effectiveness
(x-axis).

## Usage

``` r
# S3 method for class 'bcea'
contour(
  he,
  pos = "topleft",
  graph = c("base", "ggplot2", "plotly"),
  comparison = NULL,
  ...
)

contour(he, ...)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- pos:

  Parameter to set the position of the legend (only relevant for
  multiple interventions, ie more than 2 interventions being compared).
  Can be given in form of a string `(bottom|top)(right|left)` for base
  graphics and `bottom|top|left|right` for ggplot2. It can be a
  two-elements vector, which specifies the relative position on the x
  and y axis respectively, or alternatively it can be in form of a
  logical variable, with `FALSE` indicating to use the default position
  and `TRUE` to place it on the bottom of the plot.

- graph:

  A string used to select the graphical engine to use for plotting.
  Should (partial-)match the three options `"base"`, `"ggplot2"` or
  `"plotly"`. Default value is `"base"`. Not all plotting functions have
  a `"plotly"` implementation yet.

- comparison:

  Selects the comparator, in case of more than two interventions being
  analysed. Default as NULL plots all the comparisons together. Any
  subset of the possible comparisons can be selected (e.g.,
  `comparison=c(1,3)` or `comparison=2`).

- ...:

  Additional graphical arguments. The usual ggplot2 syntax is used
  regardless of graph type.

  - `xlim`: The range of the plot along the x-axis. If NULL (default) it
    is determined by the range of the simulated values for `delta_e`

  - `ylim`: The range of the plot along the y-axis. If NULL (default) it
    is determined by the range of the simulated values for `delta_c`

  - `scale`: Scales the plot as a function of the observed standard
    deviation.

  - `levels`: Numeric vector of levels at which to draw contour lines.
    Quantiles 0\<p\<1.

  - `nlevels`: Number of levels to be plotted in the contour.

## Value

- ceplane:

  A ggplot object containing the plot. Returned only if
  `graph="ggplot2"`.

Plots the cost-effectiveness plane with a scatterplot of all the
simulated values from the (posterior) bivariate distribution of
(\\\Delta_e, \Delta_c\\), the differentials of effectiveness and costs;
superimposes a contour of the distribution and prints the estimated
value of the probability of each quadrant (combination of
positive/negative values for both \\\Delta_e\\ and \\\Delta_c\\)

## References

Baio G, Dawid aP (2011). “Probabilistic sensitivity analysis in health
economics.” *Stat. Methods Med. Res.*, 1–20. ISSN 1477-0334,
[doi:10.1177/0962280211419832](https://doi.org/10.1177/0962280211419832)
, <https://pubmed.ncbi.nlm.nih.gov/21930515/>.

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md),
[`ceplane.plot()`](https://n8thangreen.github.io/BCEA/reference/ceplane.plot.md),
[`contour2()`](https://n8thangreen.github.io/BCEA/reference/contour2.md)

## Author

Gianluca Baio, Andrea Berardi

## Examples

``` r
data(Vaccine)

# run the health economic evaluation using BCEA
m <- bcea(e=eff,
          c=cost,           # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e,c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
      plot=TRUE             # plots the results
)


contour(m)

contour(m, graph = "ggplot2")


contour(m,          # uses the results of the economic evaluation 
                    #  (a "bcea" object)
      comparison=1, # if more than 2 interventions, selects the 
                    #  pairwise comparison 
      nlevels=10,   # selects the number of levels to be 
                    #  plotted (default=4)
      levels=NULL,  # specifies the actual levels to be plotted 
                    #  (default=NULL, so that R will decide)
      scale=1,      # scales the bandwidths for both x- and 
                    #  y-axis (default=0.5)
      graph="base"  # uses base graphics to produce the plot
)

```
