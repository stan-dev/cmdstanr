# Extract gradients after diagnostic mode

Return the data frame containing the gradients for all parameters.

## Usage

``` r
gradients()
```

## Value

A data frame containing the gradients for all parameters.

## See also

[`CmdStanDiagnose`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanDiagnose.md)

## Examples

``` r
# \dontrun{
test <- cmdstanr_example("logistic", method = "diagnose")

# retrieve the gradients
test$gradients()
#>   param_idx    value    model finite_diff        error
#> 1         0  1.24714 -7.12511    -7.12511 -1.17557e-08
#> 2         1 -1.71185  5.84866     5.84866  8.16993e-09
#> 3         2 -1.79697 18.67920    18.67920  2.02890e-09
#> 4         3 -1.77290 26.93410    26.93410  9.50084e-09
# }
```
