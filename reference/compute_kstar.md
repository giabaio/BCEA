# Compute k^\*

Find willingness-to-pay threshold when optimal decision changes.

## Usage

``` r
compute_kstar(k, best, ref)
```

## Arguments

- k:

  Willingness-to-pay grid approximation of the budget willing to invest
  (vector)

- best:

  Best intervention for each `k` (int)

- ref:

  Reference intervention (int)

## Value

integer representing intervention

## Details

\$\$k^\* := \min\\k : IB \< 0 \\\$\$

The value of the break-even point corresponds to the ICER and quantifies
the point at which the decision-maker is indifferent between the two
options.

## See also

[`ceac.plot()`](https://n8thangreen.github.io/BCEA/reference/ceac.plot.md)
