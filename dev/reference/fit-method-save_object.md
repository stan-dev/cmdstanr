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
#>    variable      mean  median     sd    mad       q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.6   1.43   1.27   -68.7    -64.3     1.00    1613.
#>  2 alpha        0.379   0.376 0.215  0.220    0.0235   0.736   1.00    4393.
#>  3 beta[1]     -0.666  -0.666 0.250  0.247   -1.08    -0.257   1.00    3990.
#>  4 beta[2]     -0.271  -0.271 0.224  0.221   -0.642    0.0996  1.00    3587.
#>  5 beta[3]      0.680   0.672 0.271  0.264    0.239    1.14    1.00    3944.
#>  6 log_lik[1]  -0.515  -0.509 0.0963 0.0940  -0.684   -0.367   1.00    4319.
#>  7 log_lik[2]  -0.403  -0.382 0.149  0.142   -0.682   -0.193   1.00    4375.
#>  8 log_lik[3]  -0.495  -0.463 0.212  0.205   -0.875   -0.207   1.00    4074.
#>  9 log_lik[4]  -0.450  -0.430 0.150  0.144   -0.723   -0.231   1.00    3561.
#> 10 log_lik[5]  -1.18   -1.17  0.282  0.275   -1.68    -0.759   1.00    4692.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
# }
```
