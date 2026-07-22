# Save fitted model object to a file

This method calls
[`$materialize()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-materialize.md)
internally to ensure that all posterior draws and diagnostics are saved
when saving a fitted model object. Because the contents of the CmdStan
output CSV files are only read into R lazily (i.e., as needed), the
`$save_object()` method is the safest way to guarantee that everything
has been read in before saving.

By default objects are saved using
[`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html). If you have a
big object to save, we recommend setting `format = "qs2"`, which is
faster and more memory efficient. Internally this will use
[`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html) to save the
object and you can read it back into R later with
[`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html).

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
  requires the qs2 package.

- ...:

  Other arguments to pass to
  [`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html) (for
  `format = "rds"`) or
  [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html) (for
  `format = "qs2"`).

## Value

The fitted model object, invisibly.

## See also

[`materialize`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-materialize.md)

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("logistic")

# using default format = "rds"
temp_rds_file <- tempfile(fileext = ".rds")
fit$save_object(file = temp_rds_file)
rm(fit)

fit <- readRDS(temp_rds_file)
fit$summary()
#> # A tibble: 105 × 10
#>    variable      mean  median    sd    mad       q5     q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl> <dbl>  <dbl>    <dbl>   <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.7   1.46  1.29   -68.9    -64.3    1.00    1954.
#>  2 alpha        0.379   0.378 0.223 0.222    0.0135   0.743  1.00    3531.
#>  3 beta[1]     -0.663  -0.660 0.247 0.244   -1.07    -0.266  1.00    4569.
#>  4 beta[2]     -0.276  -0.275 0.233 0.232   -0.659    0.102  1.00    4007.
#>  5 beta[3]      0.686   0.680 0.268 0.265    0.258    1.13   1.00    4070.
#>  6 log_lik[1]  -0.517  -0.510 0.100 0.0991  -0.691   -0.365  1.00    3486.
#>  7 log_lik[2]  -0.401  -0.383 0.146 0.138   -0.679   -0.197  1.00    4475.
#>  8 log_lik[3]  -0.501  -0.464 0.222 0.204   -0.924   -0.207  1.00    3933.
#>  9 log_lik[4]  -0.448  -0.430 0.154 0.148   -0.735   -0.233  1.00    3604.
#> 10 log_lik[5]  -1.19   -1.17  0.289 0.288   -1.71    -0.755  1.00    4412.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>

# using format = "qs2"
temp_qs2_file <- tempfile(fileext = ".qs2")
fit$save_object(file = temp_qs2_file, format = "qs2")
rm(fit)

fit <- qs2::qs_read(temp_qs2_file)
fit$summary()
#> # A tibble: 105 × 10
#>    variable      mean  median    sd    mad       q5     q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl> <dbl>  <dbl>    <dbl>   <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.7   1.46  1.29   -68.9    -64.3    1.00    1954.
#>  2 alpha        0.379   0.378 0.223 0.222    0.0135   0.743  1.00    3531.
#>  3 beta[1]     -0.663  -0.660 0.247 0.244   -1.07    -0.266  1.00    4569.
#>  4 beta[2]     -0.276  -0.275 0.233 0.232   -0.659    0.102  1.00    4007.
#>  5 beta[3]      0.686   0.680 0.268 0.265    0.258    1.13   1.00    4070.
#>  6 log_lik[1]  -0.517  -0.510 0.100 0.0991  -0.691   -0.365  1.00    3486.
#>  7 log_lik[2]  -0.401  -0.383 0.146 0.138   -0.679   -0.197  1.00    4475.
#>  8 log_lik[3]  -0.501  -0.464 0.222 0.204   -0.924   -0.207  1.00    3933.
#>  9 log_lik[4]  -0.448  -0.430 0.154 0.148   -0.735   -0.233  1.00    3604.
#> 10 log_lik[5]  -1.19   -1.17  0.289 0.288   -1.71    -0.755  1.00    4412.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
# }
```
