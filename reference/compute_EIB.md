# Compute Expected Incremental Benefit

A summary measure useful to assess the potential changes in the decision
under different scenarios.

## Usage

``` r
compute_EIB(ib)
```

## Arguments

- ib:

  Incremental benefit

## Value

Array with dimensions (interv x k)

## Details

When considering a pairwise comparison (e.g. in the simple case of a
reference intervention \\t = 1\\ and a comparator, such as the status
quo, \\t = 0\\), it is defined as the difference between the expected
utilities of the two alternatives:

\$\$eib := \mbox{E}\[u(e,c;1)\] - \mbox{E}\[u(e,c;0)\] = \mathcal{U}^1 -
\mathcal{U}^0.\$\$

Analysis of the expected incremental benefit describes how the decision
changes for different values of the threshold. The EIB marginalises out
the uncertainty, and does not incorporate and describe explicitly the
uncertainty in the outcomes. To overcome this problem the tool of choice
is the CEAC.

## See also

[`ceac.plot()`](https://n8thangreen.github.io/BCEA/reference/ceac.plot.md),
[`compute_CEAC()`](https://n8thangreen.github.io/BCEA/reference/compute_CEAC.md),
[`compute_IB()`](https://n8thangreen.github.io/BCEA/reference/compute_IB.md)
