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
#>   param_idx      value    model finite_diff        error
#> 1         0 -0.0740803  11.4505     11.4505 -1.44433e-08
#> 2         1 -0.2718300 -10.4244    -10.4244  2.47002e-08
#> 3         2  1.3967400 -31.3039    -31.3039  1.56456e-08
#> 4         3 -0.5356680  15.1104     15.1104  2.07092e-08
# }
```
