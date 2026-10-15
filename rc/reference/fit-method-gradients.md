# Extract gradients after diagnostic mode

Return the data frame containing the gradients for all parameters.

## Usage

``` r
gradients()
```

## Value

A data frame containing the gradients for all parameters.

## See also

[`CmdStanDiagnose`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanDiagnose.md)

## Examples

``` r
# \dontrun{
test <- cmdstanr_example("logistic", method = "diagnose")

# retrieve the gradients
test$gradients()
#>   param_idx     value     model finite_diff        error
#> 1         0  0.777333  -3.95076    -3.95076  1.93379e-09
#> 2         1 -0.327811 -10.01080   -10.01080 -4.28308e-09
#> 3         2  1.407420 -26.81840   -26.81840  1.50014e-09
#> 4         3 -1.260890  24.28000    24.28000  1.44433e-09
# }
```
