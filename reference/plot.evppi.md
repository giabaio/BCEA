# Plot Expected Value of Partial Information With Respect to a Set of Parameters

Plot Expected Value of Partial Information With Respect to a Set of
Parameters

## Usage

``` r
# S3 method for class 'evppi'
plot(x, pos = c(0, 0.8), graph = options("bcea.graph"), col = c(1, 1), ...)
```

## Arguments

- x:

  An object in the class `evppi`, obtained by the call to the function
  [`evppi()`](https://n8thangreen.github.io/BCEA/reference/evppi.md).

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
  Should (partial-) match the three options `"base"`, `"ggplot2"` or
  `"plotly"`. Default value is `"ggplot2"`. This is set globally upon
  loading `BCEA` and can be modified for instance by using
  `options("bcea.graph"="base")`, or `options("bcea.graph="plotly")`.
  Partial matching still applies (so `gg`, or `g`, or `pl`, or `p` also
  work). Not all plotting functions have a `"plotly"` implementation,
  yet – see the help for the specific functions.

- col:

  Sets the colour for the lines depicted in the graph.

- ...:

  Arguments to be passed to methods, such as graphical parameters (see
  [`par()`](https://rdrr.io/r/graphics/par.html)).

## Value

Plot with base R or ggplot2.

## References

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md),
[`evppi()`](https://n8thangreen.github.io/BCEA/reference/evppi.md)

## Author

Gianluca Baio, Andrea Berardi

## Examples

``` r
if (FALSE) { # \dontrun{
data(Vaccine, package = "BCEA")
treats <- c("Status quo", "Vaccination")

# Run the health economic evaluation using BCEA
m <- bcea(e.pts, c.pts, ref = 2, interventions = treats)

# Compute the EVPPI for a bunch of parameters
inp <- createInputs(vaccine_mat)

# Compute the EVPPI using INLA/SPDE
if (require("INLA")) {
  x0 <- BCEA::evppi(m, c("beta.1." , "beta.2."), input = inp$mat)
  
  plot(x0, pos = c(0,1))

  x1 <- BCEA::evppi(m, c(32,48,49), input = inp$mat)
  plot(x1, pos = "topright")

  plot(x0, col = c("black", "red"), pos = "topright")
  plot(x0, col = c(2,3), pos = "bottomright")

  plot(x0, pos = c(0,1), graph = "ggplot2")
  plot(x1, pos = "top", graph = "ggplot2")

  plot(x0, col = c("black", "red"), pos = "right", graph = "ggplot2")
  plot(x0, col = c(2,3), size = c(1,2), pos = "bottom", graph = "ggplot2")

  plot(x0, graph = "ggplot2", theme = ggplot2::theme_linedraw())
}

if (FALSE)
 plot(x0, col = 3, pos = "topright")
# The vector 'col' must have the number of elements for an EVPI
# colour and each of the EVPPI parameters. Forced to black
} # }
```
