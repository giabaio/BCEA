# Deprecated functions in package BCEA.

The functions listed below are deprecated and will be defunct in the
near future. When possible, alternative functions with similar
functionality are also mentioned. Help pages for deprecated functions
are available at `help("<function>-deprecated")`.

## Usage

``` r
make.report(...)

plot.mixedAn(x, y.limits=NULL, pos=c(0,1), graph=c("base","ggplot2"),...)
```

## Arguments

- ...:

  Arguments to be passed to methods, such as graphical parameters (see
  [`par()`](https://rdrr.io/r/graphics/par.html)).

- x:

  An object of class `mixedAn`, given as output of the call to the
  function
  [`mixedAn()`](https://n8thangreen.github.io/BCEA/reference/mixedAn-set.md).

- y.limits:

  Range of the y-axis for the graph. The default value is `NULL`, in
  which case the maximum range between the optimal and the mixed
  analysis scenarios is considered.

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
  Default value is `"base"`.

## Value

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

## Author

Gianluca Baio, Andrea Berardi
