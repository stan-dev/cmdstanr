# Extract gradients after diagnostic mode

Return the data frame containing the gradients for all parameters.

## Usage

``` r
gradients()
```

## Value

A list of lists. See **Examples**.

## See also

[`CmdStanDiagnose`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanDiagnose.md)

## Examples

``` r
# \dontrun{
test <- cmdstanr_example("logistic", method = "diagnose")

# retrieve the gradients
test$gradients()
#>   param_idx     value     model finite_diff        error
#> 1         0 -0.113431  10.19960    10.19960 -4.58818e-08
#> 2         1 -1.750030   6.98503     6.98503 -1.98203e-08
#> 3         2  1.232120 -24.32690   -24.32690 -7.56449e-09
#> 4         3  1.660180  -9.51340    -9.51340 -5.33097e-09
# }
```
