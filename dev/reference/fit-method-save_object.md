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
#>    variable      mean  median     sd    mad       q5     q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>   <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.7   1.47   1.24   -68.8    -64.3   1.00     2169.
#>  2 alpha        0.376   0.375 0.215  0.214    0.0281   0.733 1.00     3933.
#>  3 beta[1]     -0.661  -0.656 0.247  0.248   -1.08    -0.273 1.000    3672.
#>  4 beta[2]     -0.271  -0.272 0.228  0.226   -0.642    0.102 1.00     4140.
#>  5 beta[3]      0.670   0.662 0.278  0.274    0.226    1.13  1.00     3939.
#>  6 log_lik[1]  -0.516  -0.511 0.0976 0.0981  -0.684   -0.366 1.00     3881.
#>  7 log_lik[2]  -0.408  -0.387 0.154  0.147   -0.687   -0.199 1.00     4312.
#>  8 log_lik[3]  -0.499  -0.461 0.219  0.194   -0.908   -0.207 1.00     3953.
#>  9 log_lik[4]  -0.454  -0.433 0.156  0.153   -0.729   -0.238 1.00     4018.
#> 10 log_lik[5]  -1.17   -1.15  0.283  0.279   -1.67    -0.742 1.00     4170.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
# }
```
