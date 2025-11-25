# bcea Print Method

bcea Print Method

## Usage

``` r
# S3 method for class 'bcea'
print(x, digits = getOption("digits"), give.attr = FALSE, no.list = TRUE, ...)
```

## Arguments

- x:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- digits:

  Minimal number of significant digits, see
  [`print.default()`](https://rdrr.io/r/base/print.default.html).

- give.attr:

  Logical; if TRUE (default), show attributes as sub structures.

- no.list:

  Logical; if TRUE, no ‘list of ...’ nor the class are printed.

- ...:

  Potential further arguments.

## Examples

``` r
data("Vaccine")
he <- BCEA::bcea(eff, cost)
#> No reference selected. Defaulting to first intervention.
```
