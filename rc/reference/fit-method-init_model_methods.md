# Compile additional methods for accessing the model log-probability function and parameter constraining and unconstraining.

The `$init_model_methods()` method compiles and initializes the
`log_prob`, `grad_log_prob`, `hessian`, `constrain_variables`,
`unconstrain_variables` and `unconstrain_draws` functions. These are
then available as methods of the fitted model object. This requires the
additional Rcpp package. The methods compile once per model object, so
later fits of the same model reuse them. To avoid compiling them again
in every new R session, set the `rcpp.cache.dir` option as described for
[`$expose_functions()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-expose_functions.md).

If a model or fit object was saved with
[`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html) and later
reloaded, any previously compiled model-method bindings will be rebuilt
in the current R session when this method is called. A reloaded fit also
needs the data the model was fit with. Data originally passed as a file
is found again as long as the file still exists. However, data passed as
a list was written to the temporary directory, so either call
`$save_data_file()` *before* saving the fit (to keep that file) or pass
the original data again via the `data` argument.

Note: there may be many compiler warnings emitted during compilation but
these can be ignored so long as they are warnings and not errors.

## Usage

``` r
init_model_methods(data = NULL, seed = 1, verbose = FALSE, quiet = FALSE)
```

## Arguments

- data:

  (multiple options) A named list or the path to a JSON data file. Pass
  the data the fit was run with when a fit reloaded in a new R session
  has lost its data file, which happens when the data was originally
  passed as a list instead of a file. Other data with the same structure
  is accepted, so pass it only when you mean to evaluate the draws under
  other data. `$data_file()` then returns the new path. A list is
  written to the temporary directory again, so call `$save_data_file()`
  before saving the fit if you'll reload it in another session.

- seed:

  (integer) The random seed to use when initializing the model.

- verbose:

  (logical) Whether to show verbose logging during compilation.

- quiet:

  (logical) Should the message announcing the compilation be suppressed?
  The default is `FALSE`. Compiler output is controlled by `verbose`.

## Value

`NULL`, invisibly.

## See also

[`log_prob()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-log_prob.md),
[`grad_log_prob()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-grad_log_prob.md),
[`constrain_variables()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-constrain_variables.md),
[`unconstrain_variables()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-unconstrain_variables.md),
[`unconstrain_draws()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-unconstrain_draws.md),
[`hessian()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-hessian.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
# fit_mcmc$init_model_methods()
# }
```
