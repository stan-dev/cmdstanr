# Transform a set of parameter values to the unconstrained scale

The `$unconstrain_variables()` method transforms input parameters to the
unconstrained scale.

## Usage

``` r
unconstrain_variables(variables)
```

## Arguments

- variables:

  (list) A list of parameter values to transform, in the same format as
  provided to the `init` argument of the `$sample()` method.

## See also

[`log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-log_prob.md),
[`grad_log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-grad_log_prob.md),
[`constrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-constrain_variables.md),
`unconstrain_variables()`,
[`unconstrain_draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_draws.md),
[`variable_skeleton()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-variable_skeleton.md),
[`hessian()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-hessian.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
fit_mcmc$unconstrain_variables(list(alpha = 0.5, beta = c(0.7, 1.1, 0.2)))
#> [1] 0.5 0.7 1.1 0.2
# }
```
