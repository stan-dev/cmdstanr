# Compile additional methods for accessing the model log-probability function and parameter constraining and unconstraining.

The `$init_model_methods()` method compiles and initializes the
`log_prob`, `grad_log_prob`, `constrain_variables`,
`unconstrain_variables` and `unconstrain_draws` functions. These are
then available as methods of the fitted model object. This requires the
additional `Rcpp` package, which are not required for fitting models
using CmdStanR.

Note: there may be many compiler warnings emitted during compilation but
these can be ignored so long as they are warnings and not errors.

## Usage

``` r
init_model_methods(seed = 1, verbose = FALSE, hessian = FALSE)
```

## Arguments

- seed:

  (integer) The random seed to use when initializing the model.

- verbose:

  (logical) Whether to show verbose logging during compilation.

- hessian:

  (logical) Whether to expose the (experimental) hessian method.

## See also

[`log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-log_prob.md),
[`grad_log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-grad_log_prob.md),
[`constrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-constrain_variables.md),
[`unconstrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_variables.md),
[`unconstrain_draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_draws.md),
[`variable_skeleton()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-variable_skeleton.md),
[`hessian()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-hessian.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
# }
```
