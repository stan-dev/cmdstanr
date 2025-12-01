# Save fitted model object to a file

This method is a wrapper around
[`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html) that ensures
that all posterior draws and diagnostics are saved when saving a fitted
model object. Because the contents of the CmdStan output CSV files are
only read into R lazily (i.e., as needed), the `$save_object()` method
is the safest way to guarantee that everything has been read in before
saving.

See the "Saving fitted model objects" section of the [*Getting started
with CmdStanR*](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)
vignette for some suggestions on faster model saving for large models.

## Usage

``` r
save_object(file, ...)
```

## Arguments

- file:

  (string) Path where the file should be saved.

- ...:

  Other arguments to pass to
  [`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html) besides
  `object` and `file`.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("logistic")

temp_rds_file <- tempfile(fileext = ".RDS")
fit$save_object(file = temp_rds_file)
rm(fit)

fit <- readRDS(temp_rds_file)
fit$summary()
#> # A tibble: 105 × 10
#>    variable      mean  median     sd    mad       q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -65.9   -65.6   1.39   1.20   -68.6    -64.3    1.000    1923.
#>  2 alpha        0.378   0.378 0.210  0.211    0.0373   0.725  1.00     4192.
#>  3 beta[1]     -0.667  -0.658 0.241  0.241   -1.07    -0.270  1.000    3920.
#>  4 beta[2]     -0.273  -0.273 0.229  0.229   -0.658    0.0969 1.000    4228.
#>  5 beta[3]      0.680   0.678 0.268  0.262    0.243    1.13   1.00     4100.
#>  6 log_lik[1]  -0.516  -0.508 0.0963 0.0940  -0.685   -0.367  1.000    4194.
#>  7 log_lik[2]  -0.402  -0.381 0.145  0.139   -0.665   -0.200  1.000    4540.
#>  8 log_lik[3]  -0.498  -0.462 0.217  0.204   -0.904   -0.208  1.00     4443.
#>  9 log_lik[4]  -0.450  -0.432 0.152  0.143   -0.726   -0.236  1.00     3935.
#> 10 log_lik[5]  -1.18   -1.16  0.277  0.274   -1.68    -0.758  1.00     4757.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
# }
```
