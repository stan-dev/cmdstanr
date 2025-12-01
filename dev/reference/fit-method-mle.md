# Extract point estimate after optimization

The `$mle()` method is only available for
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md)
objects. It returns the point estimate as a numeric vector with one
element per variable. The returned vector does *not* include `lp__`, the
total log probability (`target`) accumulated in the model block of the
Stan program, which is available via the
[`$lp()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-lp.md)
method and also included in the
[`$draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)
method.

For models with constrained parameters that are fit with
`jacobian=TRUE`, the `$mle()` method actually returns the maximum a
posteriori (MAP) estimate (posterior mode) rather than the MLE. See
[`$optimize()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-optimize.md)
and the CmdStan User's Guide for more details.

## Usage

``` r
mle(variables = NULL)
```

## Arguments

- variables:

  (character vector) The variables (parameters, transformed parameters,
  and generated quantities) to include. If NULL (the default) then all
  variables are included.

## Value

A numeric vector. See **Examples**.

## See also

[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md)

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("logistic", method = "optimize")
fit$mle("alpha")
#>     alpha 
#> 0.3644656 
fit$mle("beta")
#>    beta[1]    beta[2]    beta[3] 
#> -0.6315517 -0.2589577  0.6484951 
fit$mle("beta[2]")
#>    beta[2] 
#> -0.2589577 
# }
```
