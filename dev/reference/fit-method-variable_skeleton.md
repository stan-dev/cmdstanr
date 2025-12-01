# Return the variable skeleton for `relist`

The `$variable_skeleton()` method returns the variable skeleton needed
by [`utils::relist()`](https://rdrr.io/r/utils/relist.html) to
re-structure a vector of constrained parameter values to a named list.

## Usage

``` r
variable_skeleton(transformed_parameters = TRUE, generated_quantities = TRUE)
```

## Arguments

- transformed_parameters:

  (logical) Whether to include transformed parameters in the skeleton
  (defaults to `TRUE`).

- generated_quantities:

  (logical) Whether to include generated quantities in the skeleton
  (defaults to `TRUE`).

## See also

[`log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-log_prob.md),
[`grad_log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-grad_log_prob.md),
[`constrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-constrain_variables.md),
[`unconstrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_variables.md),
[`unconstrain_draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_draws.md),
`variable_skeleton()`,
[`hessian()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-hessian.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
fit_mcmc$variable_skeleton()
#> $alpha
#> [1] 0
#> 
#> $beta
#> [1] 0 0 0
#> 
#> $log_lik
#>   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#>  [38] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#>  [75] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> 
# }
```
