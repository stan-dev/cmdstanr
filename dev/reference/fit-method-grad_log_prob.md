# Calculate the log-probability and the gradient w.r.t. each input for a given vector of unconstrained parameters

The `$grad_log_prob()` method provides access to the Stan model's
`log_prob` function and its derivative.

## Usage

``` r
grad_log_prob(
  unconstrained_variables,
  jacobian = TRUE,
  jacobian_adjustment = NULL
)
```

## Arguments

- unconstrained_variables:

  (numeric) A vector of unconstrained parameters.

- jacobian:

  (logical) Whether to include the log-density adjustments from
  un/constraining variables.

- jacobian_adjustment:

  Deprecated. Please use `jacobian` instead.

## See also

[`log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-log_prob.md),
`grad_log_prob()`,
[`constrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-constrain_variables.md),
[`unconstrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_variables.md),
[`unconstrain_draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_draws.md),
[`variable_skeleton()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-variable_skeleton.md),
[`hessian()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-hessian.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
fit_mcmc$grad_log_prob(unconstrained_variables = c(0.5, 1.2, 1.1, 2.2))
#> [1]   1.462151 -26.619534 -25.528776 -14.286822
#> attr(,"log_prob")
#> [1] -130.2141
# }
```
