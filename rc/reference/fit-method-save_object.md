# Save fitted model object to a file

This method calls
[`$materialize()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-materialize.md)
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

The data file is not part of the saved object. When the data was passed
as a list, the file is in the session's temporary directory, so a fit
that will use the compiled model methods in a later session needs either
`$save_data_file()` first or the data passed to `$init_model_methods()`
after reloading.

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

[`materialize`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-materialize.md)

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
#>    variable      mean  median     sd    mad       q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.6   1.47   1.23   -68.8    -64.3    1.00     2020.
#>  2 alpha        0.382   0.379 0.217  0.218    0.0248   0.743  1.00     3982.
#>  3 beta[1]     -0.663  -0.660 0.251  0.250   -1.09    -0.263  1.00     4322.
#>  4 beta[2]     -0.272  -0.269 0.230  0.230   -0.649    0.0941 1.000    3751.
#>  5 beta[3]      0.674   0.663 0.267  0.267    0.250    1.12   1.00     3703.
#>  6 log_lik[1]  -0.514  -0.508 0.0986 0.0954  -0.686   -0.364  1.00     4051.
#>  7 log_lik[2]  -0.406  -0.388 0.148  0.143   -0.680   -0.202  1.00     3962.
#>  8 log_lik[3]  -0.498  -0.464 0.218  0.208   -0.892   -0.203  1.00     3812.
#>  9 log_lik[4]  -0.454  -0.435 0.155  0.152   -0.732   -0.235  1.00     3979.
#> 10 log_lik[5]  -1.18   -1.16  0.282  0.272   -1.69    -0.770  1.00     3864.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>

# using format = "qs2"
temp_qs2_file <- tempfile(fileext = ".qs2")
fit$save_object(file = temp_qs2_file, format = "qs2")
rm(fit)

fit <- qs2::qs_read(temp_qs2_file)
fit$summary()
#> # A tibble: 105 × 10
#>    variable      mean  median     sd    mad       q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.6   1.47   1.23   -68.8    -64.3    1.00     2020.
#>  2 alpha        0.382   0.379 0.217  0.218    0.0248   0.743  1.00     3982.
#>  3 beta[1]     -0.663  -0.660 0.251  0.250   -1.09    -0.263  1.00     4322.
#>  4 beta[2]     -0.272  -0.269 0.230  0.230   -0.649    0.0941 1.000    3751.
#>  5 beta[3]      0.674   0.663 0.267  0.267    0.250    1.12   1.00     3703.
#>  6 log_lik[1]  -0.514  -0.508 0.0986 0.0954  -0.686   -0.364  1.00     4051.
#>  7 log_lik[2]  -0.406  -0.388 0.148  0.143   -0.680   -0.202  1.00     3962.
#>  8 log_lik[3]  -0.498  -0.464 0.218  0.208   -0.892   -0.203  1.00     3812.
#>  9 log_lik[4]  -0.454  -0.435 0.155  0.152   -0.732   -0.235  1.00     3979.
#> 10 log_lik[5]  -1.18   -1.16  0.282  0.272   -1.69    -0.770  1.00     3864.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
# }
```
