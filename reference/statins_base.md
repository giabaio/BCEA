# Statins Evidence-Synthesis Data (Base Model)

This data set contains the results of a Bayesian analysis used to model
the effectiveness of various statins. The analysis is based on the
simplest model, using vague priors.

## Format

A `BUGS` object containing the simulations for the evidence synthesis
model.

## Source

A systematic review and economic evaluation of statins for the
prevention of coronary events. Ward 2007.

## References

Baio G. (2012). *Bayesian Methods in Health Economics*. CRC/Chapman &
Hall, London.

## Examples

``` r
data(statins_base)
lapply(statins_base$sims.list, summary)
#> $cost.hosp
#>        V1                V2                V3                V4        
#>  Min.   :  24.52   Min.   :  24.62   Min.   :  51.72   Min.   :  34.9  
#>  1st Qu.: 168.60   1st Qu.: 219.17   1st Qu.: 296.99   1st Qu.: 304.8  
#>  Median : 213.52   Median : 290.17   Median : 427.93   Median : 384.7  
#>  Mean   : 238.70   Mean   : 315.63   Mean   : 523.07   Mean   : 425.0  
#>  3rd Qu.: 278.21   3rd Qu.: 372.68   3rd Qu.: 619.92   3rd Qu.: 491.7  
#>  Max.   :2084.20   Max.   :2322.60   Max.   :8250.80   Max.   :3633.3  
#>        V5                V6         
#>  Min.   :  24.19   Min.   :  25.79  
#>  1st Qu.: 217.03   1st Qu.: 217.86  
#>  Median : 272.69   Median : 273.23  
#>  Mean   : 305.20   Mean   : 301.13  
#>  3rd Qu.: 356.47   3rd Qu.: 354.16  
#>  Max.   :2835.42   Max.   :2518.23  
#> 
#> $cost.stat
#>        V1                V2                V3                V4         
#>  Min.   :  60.48   Min.   :  56.87   Min.   :  14.96   Min.   :  15.51  
#>  1st Qu.: 283.56   1st Qu.: 211.98   1st Qu.:  84.99   1st Qu.: 128.30  
#>  Median : 412.23   Median : 304.93   Median : 138.67   Median : 232.38  
#>  Mean   : 480.88   Mean   : 350.02   Mean   : 166.69   Mean   : 305.41  
#>  3rd Qu.: 596.72   3rd Qu.: 440.43   3rd Qu.: 207.76   3rd Qu.: 391.24  
#>  Max.   :2620.80   Max.   :1661.47   Max.   :1421.64   Max.   :1966.49  
#>        V5                V6         
#>  Min.   :  58.56   Min.   :  18.90  
#>  1st Qu.: 200.55   1st Qu.:  83.65  
#>  Median : 297.30   Median : 131.84  
#>  Mean   : 346.94   Mean   : 165.07  
#>  3rd Qu.: 432.82   3rd Qu.: 209.10  
#>  Max.   :1689.55   Max.   :1526.40  
#> 
#> $cost.tot
#>        V1               V2               V3               V4        
#>  Min.   : 205.4   Min.   : 212.4   Min.   : 154.5   Min.   : 156.0  
#>  1st Qu.: 500.3   1st Qu.: 487.4   1st Qu.: 444.2   1st Qu.: 498.1  
#>  Median : 645.4   Median : 618.2   Median : 592.5   Median : 660.2  
#>  Mean   : 719.6   Mean   : 665.7   Mean   : 689.8   Mean   : 730.4  
#>  3rd Qu.: 846.2   3rd Qu.: 773.5   3rd Qu.: 822.6   3rd Qu.: 860.0  
#>  Max.   :2905.5   Max.   :2993.1   Max.   :8521.3   Max.   :3697.1  
#>        V5               V6         
#>  Min.   : 219.5   Min.   :  93.14  
#>  1st Qu.: 473.8   1st Qu.: 343.45  
#>  Median : 604.8   Median : 429.51  
#>  Mean   : 652.1   Mean   : 466.20  
#>  3rd Qu.: 759.3   3rd Qu.: 536.41  
#>  Max.   :3064.3   Max.   :2769.62  
#> 
#> $deviance
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    2134    2155    2162    2163    2170    2206 
#> 
#> $duration
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#>   0.03522 143.53073 287.46333 232.06108 329.62690 364.97184 
#> 
#> $effect
#>        V1               V2               V3                V4         
#>  Min.   :0.4282   Min.   :0.3627   Min.   :-1.2638   Min.   :0.09604  
#>  1st Qu.:0.9308   1st Qu.:0.9076   1st Qu.: 0.8422   1st Qu.:0.87588  
#>  Median :0.9454   Median :0.9263   Median : 0.8920   Median :0.90160  
#>  Mean   :0.9396   Mean   :0.9201   Mean   : 0.8677   Mean   :0.89239  
#>  3rd Qu.:0.9564   3rd Qu.:0.9441   3rd Qu.: 0.9223   3rd Qu.:0.92193  
#>  Max.   :0.9929   Max.   :0.9929   Max.   : 0.9852   Max.   :0.98990  
#>        V5               V6        
#>  Min.   :0.2220   Min.   :0.3811  
#>  1st Qu.:0.9112   1st Qu.:0.9123  
#>  Median :0.9302   Median :0.9309  
#>  Mean   :0.9227   Mean   :0.9238  
#>  3rd Qu.:0.9439   3rd Qu.:0.9443  
#>  Max.   :0.9930   Max.   :0.9925  
#> 
#> $risk
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> 0.003855 0.028220 0.035040 0.038474 0.044065 0.330280 
#> 
#> $rr
#>        V1               V2               V3               V4        
#>  Min.   :0.3688   Min.   :0.4241   Min.   :0.1447   Min.   :0.6617  
#>  1st Qu.:0.5206   1st Qu.:0.6298   1st Qu.:0.6527   1st Qu.:0.7655  
#>  Median :0.5649   Median :0.6919   Median :0.8848   Median :0.7933  
#>  Mean   :0.5692   Mean   :0.6986   Mean   :0.9538   Mean   :0.7943  
#>  3rd Qu.:0.6127   3rd Qu.:0.7636   3rd Qu.:1.1610   3rd Qu.:0.8220  
#>  Max.   :0.8508   Max.   :1.3161   Max.   :4.1630   Max.   :0.9320  
#>        V5               V6        
#>  Min.   :0.4643   Min.   :0.5439  
#>  1st Qu.:0.6219   1st Qu.:0.6254  
#>  Median :0.6760   Median :0.6462  
#>  Mean   :0.6770   Mean   :0.6458  
#>  3rd Qu.:0.7284   3rd Qu.:0.6659  
#>  Max.   :0.9447   Max.   :0.7570  
#> 
#> $theta
#>        V1                 V2                 V3                 V4          
#>  Min.   :0.002759   Min.   :0.002616   Min.   :0.003801   Min.   :0.003088  
#>  1st Qu.:0.015949   1st Qu.:0.019020   1st Qu.:0.021513   1st Qu.:0.022310  
#>  Median :0.019849   Median :0.024848   Median :0.030372   Median :0.027700  
#>  Mean   :0.021893   Mean   :0.026768   Mean   :0.037050   Mean   :0.030485  
#>  3rd Qu.:0.024844   3rd Qu.:0.030746   3rd Qu.:0.043707   3rd Qu.:0.035048  
#>  Max.   :0.204387   Max.   :0.211186   Max.   :0.644395   Max.   :0.258133  
#>        V5                 V6          
#>  Min.   :0.002542   Min.   :0.002613  
#>  1st Qu.:0.018781   1st Qu.:0.018271  
#>  Median :0.023560   Median :0.022821  
#>  Mean   :0.026007   Mean   :0.024796  
#>  3rd Qu.:0.029896   3rd Qu.:0.028245  
#>  Max.   :0.260700   Max.   :0.203756  
#> 
#> $unit.cost
#>        V1               V2               V3                V4         
#>  Min.   :0.2362   Min.   :0.2099   Min.   :0.06126   Min.   :0.05583  
#>  1st Qu.:1.0382   1st Qu.:0.8055   1st Qu.:0.35483   1st Qu.:0.46957  
#>  Median :1.5190   Median :1.1675   Median :0.56877   Median :0.86294  
#>  Mean   :1.7691   Mean   :1.3298   Mean   :0.69355   Mean   :1.12256  
#>  3rd Qu.:2.2198   3rd Qu.:1.6696   3rd Qu.:0.86158   3rd Qu.:1.46508  
#>  Max.   :9.6739   Max.   :6.4013   Max.   :6.77658   Max.   :7.47723  
#>        V5               V6         
#>  Min.   :0.2238   Min.   :0.07491  
#>  1st Qu.:0.7593   1st Qu.:0.31398  
#>  Median :1.1353   Median :0.49531  
#>  Mean   :1.3120   Mean   :0.62088  
#>  3rd Qu.:1.6482   3rd Qu.:0.78443  
#>  Max.   :6.1704   Max.   :5.97102  
#> 
```
