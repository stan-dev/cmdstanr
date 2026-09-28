# Calculate the log-probability, the gradient w.r.t. each input, and the Hessian for a given vector of unconstrained parameters

The `$hessian()` method provides access to the Stan model's `log_prob`,
its derivative, and its Hessian.

## Usage

``` r
hessian(unconstrained_variables, jacobian = TRUE)
```

## Arguments

- unconstrained_variables:

  (numeric) A vector of unconstrained parameters.

- jacobian:

  (logical) Whether to include the log-density adjustments from
  constraining or unconstraining variables.

## Value

A named list with elements `log_prob`, `grad_log_prob`, and `hessian`.

## See also

[`log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-log_prob.md),
[`grad_log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-grad_log_prob.md),
[`constrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-constrain_variables.md),
[`unconstrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_variables.md),
[`unconstrain_draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_draws.md),
[`variable_skeleton()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-variable_skeleton.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
# fit_mcmc$init_model_methods()
# fit_mcmc$hessian(unconstrained_variables = c(0.5, 1.2, 1.1, 2.2))
# }
```
