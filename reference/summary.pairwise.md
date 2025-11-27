# Summary Method for Objects of Class `pairwise`

Produces a table printout with some summary results of the health
economic evaluation.

## Usage

``` r
# S3 method for class 'pairwise'
summary(object, wtp = 25000, ...)
```

## Arguments

- object:

  A `pairwise` object containing the results of the Bayesian modelling
  and the economic evaluation.

- wtp:

  The value of the willingness to pay threshold used in the summary
  table.

- ...:

  Additional arguments affecting the summary produced.

## Value

Prints a summary table with some information on the health economic
output and synthetic information on the economic measures (EIB, CEAC,
EVPI).

## References

Baio G, Dawid AP (2011). “Probabilistic sensitivity analysis in health
economics.” *Stat. Methods Med. Res.*, 1–20. ISSN 1477-0334,
[doi:10.1177/0962280211419832](https://doi.org/10.1177/0962280211419832)
, <https://pubmed.ncbi.nlm.nih.gov/21930515/>.

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md)
[`multi.ce()`](https://n8thangreen.github.io/BCEA/reference/multi.ce.md)

## Author

Gianluca Baio

## Examples

``` r
data(Vaccine)
he <- bcea(eff, cost, interventions = treats, ref = 2)
he_multi <- multi.ce(he)
summary(he_multi)
#> 
#> Cost-effectiveness analysis summary 
#> 
#> Intervention(s): Status Quo
#>                : Vaccination
#> 
#> Optimal decision: choose Status Quo for k < 20100 and Vaccination for k >= 20100
#> 
#> 
#> Analysis for willingness to pay parameter k = 25000
#> 
#>             Expected net benefit  CEAC
#> Status Quo               -36.054 0.471
#> Vaccination              -34.826 0.529
#> 
#> Optimal intervention (max expected net benefit) for k = 25000: Vaccination
#>            
#> EVPI 2.4145
```
