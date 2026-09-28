# Extract the fitted-parameter CSV files used for generated quantities

The `$fitted_params_files()` method returns the paths to the CmdStan CSV
files used as the `fitted_params` input to standalone generated
quantities.

## Usage

``` r
fitted_params_files()
```

## Value

A character vector of file paths.

## See also

[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md),
[`$generate_quantities()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-generate-quantities.md)
