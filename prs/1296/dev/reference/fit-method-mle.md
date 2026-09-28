# Extract point estimate after optimization

The `$mle()` method is only available for
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md)
objects. The method name is retained for historical reasons. It returns
the point estimate as a numeric vector with one element per variable.
The returned vector does *not* include `lp__`, the target log density
evaluated by Stan, up to an additive constant. `lp__` is available via
the
[`$lp()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-lp.md)
method and also included in the
[`$draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)
method.

With `jacobian = FALSE`, the point estimate is a mode of the target in
the constrained parameter space. With `jacobian = TRUE`, it is a mode of
the corresponding density in the unconstrained parameter space. See
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
#> 0.3644665 
fit$mle("beta")
#>    beta[1]    beta[2]    beta[3] 
#> -0.6315625 -0.2589803  0.6484943 
fit$mle("beta[2]")
#>    beta[2] 
#> -0.2589803 
# }
```
