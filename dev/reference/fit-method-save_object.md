# Save fitted model object to a file

This method is a wrapper around
[`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html) that ensures
that all posterior draws and diagnostics are saved when saving a fitted
model object. Because the contents of the CmdStan output CSV files are
only read into R lazily (i.e., as needed), the `$save_object()` method
is the safest way to guarantee that everything has been read in before
saving.

If you have a big object to save, use `format = "qs2"` to save using the
**qs2** package.

See the "Saving fitted model objects" section of the [*Getting started
with CmdStanR*](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)
vignette for some suggestions on faster model saving for large models.

## Usage

``` r
save_object(file, format = c("rds", "qs2"), ...)
```

## Arguments

- file:

  (string) Path where the file should be saved.

- format:

  (string) Serialization format for the object. The default is `"rds"`.
  The `"qs2"` format uses
  [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html) and
  requires the **qs2** package.

- ...:

  Other arguments to pass to
  [`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html) (for
  `format = "rds"`) or
  [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html) (for
  `format = "qs2"`).

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md),
[`materialize`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-materialize.md)

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
#>    variable      mean  median     sd    mad       q5     q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>   <dbl> <dbl>    <dbl>
#>  1 lp__       -65.9   -65.6   1.40   1.23   -68.6    -64.3   1.00     2050.
#>  2 alpha        0.375   0.374 0.217  0.211    0.0109   0.736 1.00     4049.
#>  3 beta[1]     -0.660  -0.659 0.243  0.239   -1.06    -0.261 1.00     4553.
#>  4 beta[2]     -0.272  -0.274 0.229  0.227   -0.648    0.101 1.00     4003.
#>  5 beta[3]      0.679   0.674 0.266  0.272    0.244    1.12  1.00     4373.
#>  6 log_lik[1]  -0.518  -0.511 0.0985 0.0967  -0.691   -0.365 1.00     4150.
#>  7 log_lik[2]  -0.402  -0.383 0.144  0.142   -0.666   -0.198 1.00     4705.
#>  8 log_lik[3]  -0.500  -0.469 0.217  0.210   -0.893   -0.206 1.000    4100.
#>  9 log_lik[4]  -0.450  -0.431 0.152  0.149   -0.731   -0.234 1.00     3910.
#> 10 log_lik[5]  -1.18   -1.16  0.279  0.272   -1.67    -0.758 1.00     4108.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
# }
```
