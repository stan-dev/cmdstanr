# Materialize model object

This method collects all posterior draws and diagnostics of a fitted
model object into R, since the contents of the CmdStan output CSV files
are only read into R lazily (i.e., as needed).

## Usage

``` r
materialize()
```

## See also

[`save_object`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_object.md)

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("logistic")
object.size(fit)
#> 432 bytes

fit$materialize()
object.size(fit)
#> 432 bytes
# }
```
