# Statins Evidence-Synthesis Data (Robust Model)

This data set contains the results of a Bayesian analysis used to model
the effectiveness of various statins. The analysis uses robust
Half-Cauchy priors for the structured effects standard deviations.

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
data(statins_HC)
lapply(statins_HC$sims.list, summary)
#> $cost.hosp
#>        V1                V2                V3                V4         
#>  Min.   :  35.32   Min.   :  36.86   Min.   :  53.74   Min.   :  63.54  
#>  1st Qu.: 161.58   1st Qu.: 211.21   1st Qu.: 280.45   1st Qu.: 291.44  
#>  Median : 209.35   Median : 276.60   Median : 402.15   Median : 370.31  
#>  Mean   : 228.90   Mean   : 301.51   Mean   : 469.81   Mean   : 403.13  
#>  3rd Qu.: 272.93   3rd Qu.: 366.52   3rd Qu.: 583.21   3rd Qu.: 473.55  
#>  Max.   :1065.61   Max.   :1277.20   Max.   :2000.02   Max.   :2148.20  
#>        V5                V6        
#>  Min.   :  46.82   Min.   :  53.3  
#>  1st Qu.: 205.43   1st Qu.: 206.8  
#>  Median : 262.05   Median : 261.1  
#>  Mean   : 287.92   Mean   : 285.9  
#>  3rd Qu.: 338.92   3rd Qu.: 344.7  
#>  Max.   :1571.32   Max.   :1643.1  
#> 
#> $cost.stat
#>        V1                V2               V3               V4         
#>  Min.   :  78.27   Min.   :  64.2   Min.   : 13.51   Min.   :  21.36  
#>  1st Qu.: 288.06   1st Qu.: 210.1   1st Qu.: 85.36   1st Qu.: 144.02  
#>  Median : 411.12   Median : 301.6   Median :128.64   Median : 233.07  
#>  Mean   : 494.95   Mean   : 349.7   Mean   :165.27   Mean   : 305.32  
#>  3rd Qu.: 607.43   3rd Qu.: 434.4   3rd Qu.:204.00   3rd Qu.: 382.85  
#>  Max.   :3176.09   Max.   :1477.7   Max.   :905.86   Max.   :2469.83  
#>        V5                V6         
#>  Min.   :  36.77   Min.   :  15.92  
#>  1st Qu.: 202.83   1st Qu.:  81.13  
#>  Median : 297.18   Median : 126.49  
#>  Mean   : 357.13   Mean   : 160.56  
#>  3rd Qu.: 454.15   3rd Qu.: 203.76  
#>  Max.   :2665.21   Max.   :1144.20  
#> 
#> $cost.tot
#>        V1               V2               V3               V4        
#>  Min.   : 187.2   Min.   : 174.8   Min.   : 140.0   Min.   : 115.5  
#>  1st Qu.: 498.2   1st Qu.: 480.9   1st Qu.: 413.4   1st Qu.: 497.0  
#>  Median : 652.2   Median : 610.1   Median : 573.9   Median : 645.8  
#>  Mean   : 723.9   Mean   : 651.2   Mean   : 635.1   Mean   : 708.4  
#>  3rd Qu.: 854.0   3rd Qu.: 784.2   3rd Qu.: 777.5   3rd Qu.: 841.9  
#>  Max.   :3408.1   Max.   :2226.3   Max.   :2175.9   Max.   :2794.8  
#>        V5               V6         
#>  Min.   : 219.0   Min.   :  87.78  
#>  1st Qu.: 456.7   1st Qu.: 328.00  
#>  Median : 597.1   Median : 422.37  
#>  Mean   : 645.0   Mean   : 446.41  
#>  3rd Qu.: 765.5   3rd Qu.: 529.28  
#>  Max.   :2759.8   Max.   :1687.39  
#> 
#> $deviance
#>        V1      
#>  Min.   :2133  
#>  1st Qu.:2154  
#>  Median :2162  
#>  Mean   :2162  
#>  3rd Qu.:2169  
#>  Max.   :2206  
#> 
#> $duration
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#>   0.04762 145.88940 286.91476 232.33780 329.10705 364.98499 
#> 
#> $effect
#>        V1               V2               V3               V4        
#>  Min.   :0.7395   Min.   :0.6415   Min.   :0.5036   Min.   :0.4623  
#>  1st Qu.:0.9306   1st Qu.:0.9071   1st Qu.:0.8518   1st Qu.:0.8790  
#>  Median :0.9464   Median :0.9298   Median :0.8964   Median :0.9056  
#>  Mean   :0.9419   Mean   :0.9235   Mean   :0.8809   Mean   :0.8977  
#>  3rd Qu.:0.9596   3rd Qu.:0.9467   3rd Qu.:0.9288   3rd Qu.:0.9263  
#>  Max.   :0.9905   Max.   :0.9901   Max.   :0.9856   Max.   :0.9830  
#>        V5               V6        
#>  Min.   :0.6067   Min.   :0.5887  
#>  1st Qu.:0.9133   1st Qu.:0.9139  
#>  Median :0.9334   Median :0.9326  
#>  Mean   :0.9269   Mean   :0.9275  
#>  3rd Qu.:0.9485   3rd Qu.:0.9472  
#>  Max.   :0.9884   Max.   :0.9857  
#> 
#> $risk
#>        V1          
#>  Min.   :0.006353  
#>  1st Qu.:0.026629  
#>  Median :0.034172  
#>  Mean   :0.036595  
#>  3rd Qu.:0.043321  
#>  Max.   :0.205042  
#> 
#> $rr
#>        V1               V2               V3               V4        
#>  Min.   :0.3864   Min.   :0.4029   Min.   :0.2285   Min.   :0.6536  
#>  1st Qu.:0.5213   1st Qu.:0.6253   1st Qu.:0.6500   1st Qu.:0.7615  
#>  Median :0.5728   Median :0.6950   Median :0.8634   Median :0.7916  
#>  Mean   :0.5768   Mean   :0.7013   Mean   :0.9269   Mean   :0.7932  
#>  3rd Qu.:0.6271   3rd Qu.:0.7674   3rd Qu.:1.1420   3rd Qu.:0.8218  
#>  Max.   :0.8288   Max.   :1.1560   Max.   :3.9497   Max.   :0.9481  
#>        V5               V6        
#>  Min.   :0.5084   Min.   :0.5496  
#>  1st Qu.:0.6204   1st Qu.:0.6228  
#>  Median :0.6700   Median :0.6437  
#>  Mean   :0.6740   Mean   :0.6453  
#>  3rd Qu.:0.7210   3rd Qu.:0.6669  
#>  Max.   :0.9512   Max.   :0.7507  
#> 
#> $theta
#>        V1                 V2                 V3                 V4         
#>  Min.   :0.003312   Min.   :0.003387   Min.   :0.004034   Min.   :0.00481  
#>  1st Qu.:0.014808   1st Qu.:0.018084   1st Qu.:0.020191   1st Qu.:0.02083  
#>  Median :0.019503   Median :0.023500   Median :0.029083   Median :0.02682  
#>  Mean   :0.021072   Mean   :0.025616   Mean   :0.033321   Mean   :0.02900  
#>  3rd Qu.:0.024976   3rd Qu.:0.030604   3rd Qu.:0.041325   3rd Qu.:0.03428  
#>  Max.   :0.098662   Max.   :0.120574   Max.   :0.133459   Max.   :0.16190  
#>        V5                 V6          
#>  Min.   :0.004127   Min.   :0.004537  
#>  1st Qu.:0.017501   1st Qu.:0.016992  
#>  Median :0.022841   Median :0.022185  
#>  Mean   :0.024627   Mean   :0.023625  
#>  3rd Qu.:0.029512   3rd Qu.:0.028254  
#>  Max.   :0.137872   Max.   :0.137698  
#> 
#> $unit.cost
#>        V1                V2               V3                V4         
#>  Min.   : 0.2859   Min.   :0.2452   Min.   :0.05747   Min.   :0.08182  
#>  1st Qu.: 1.0773   1st Qu.:0.8006   1st Qu.:0.35644   1st Qu.:0.52322  
#>  Median : 1.5159   Median :1.1442   Median :0.53999   Median :0.85166  
#>  Mean   : 1.8216   Mean   :1.3280   Mean   :0.68854   Mean   :1.12243  
#>  3rd Qu.: 2.2296   3rd Qu.:1.6651   3rd Qu.:0.84819   3rd Qu.:1.41823  
#>  Max.   :11.3541   Max.   :6.0413   Max.   :3.86719   Max.   :8.97753  
#>        V5                V6         
#>  Min.   : 0.1349   Min.   :0.05946  
#>  1st Qu.: 0.7715   1st Qu.:0.30709  
#>  Median : 1.1402   Median :0.47073  
#>  Mean   : 1.3535   Mean   :0.60180  
#>  3rd Qu.: 1.7151   3rd Qu.:0.75056  
#>  Max.   :10.8508   Max.   :4.64273  
#> 
```
