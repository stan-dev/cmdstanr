# CmdStanMCMC objects

A `CmdStanMCMC` object is the fitted model object returned by the
[`$sample()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample.md)
method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object. Like `CmdStanModel` objects, `CmdStanMCMC` objects are
[R6](https://r6.r-lib.org/reference/R6Class.html) objects.

## Methods

`CmdStanMCMC` objects have the following associated methods, all of
which have their own (linked) documentation pages.

### Extract contents of fitted model object

|                                                                                                          |                                                                                                            |
|----------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------|
| **Method**                                                                                               | **Description**                                                                                            |
| [`$draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)                             | Return posterior draws using formats from the posterior package.                                           |
| [`$sampler_diagnostics()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-sampler_diagnostics.md) | Return sampler diagnostics as a [`draws_array`](https://mc-stan.org/posterior/reference/draws_array.html). |
| [`$lp()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-lp.md)                                   | Return the total log probability density (`target`).                                                       |
| [`$inv_metric()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-inv_metric.md)                   | Return the inverse metric for each chain.                                                                  |
| [`$init()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-init.md)                               | Return user-specified initial values.                                                                      |
| [`$metadata()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-metadata.md)                       | Return a list of metadata gathered from the CmdStan CSV files.                                             |
| [`$num_chains()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-num_chains.md)                   | Return the number of MCMC chains.                                                                          |
| [`$code()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-code.md)                               | Return Stan code as a character vector.                                                                    |

### Summarize inferences and diagnostics

|                                                                                                        |                                                                                                   |
|--------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------|
| **Method**                                                                                             | **Description**                                                                                   |
| [`$print()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-summary.md)                         | Run [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html). |
| [`$summary()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-summary.md)                       | Run [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html). |
| [`$diagnostic_summary()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-diagnostic_summary.md) | Get summaries of sampler diagnostics and warning messages.                                        |
| [`$cmdstan_summary()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-cmdstan_summary.md)       | Run and print CmdStan's `bin/stansummary`.                                                        |
| [`$cmdstan_diagnose()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-cmdstan_summary.md)      | Run and print CmdStan's `bin/diagnose`.                                                           |
| [`$loo()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-loo.md)                               | Run [`loo::loo.array()`](https://mc-stan.org/loo/reference/loo.html) for approximate LOO-CV       |

### Save fitted model object and temporary files

|                                                                                                               |                                                    |
|---------------------------------------------------------------------------------------------------------------|----------------------------------------------------|
| **Method**                                                                                                    | **Description**                                    |
| [`$save_object()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_object.md)                      | Save fitted model object to a file.                |
| [`$save_output_files()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md)          | Save output CSV files to a specified location.     |
| [`$save_data_file()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md)             | Save JSON data file to a specified location.       |
| [`$save_latent_dynamics_files()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md) | Save diagnostic CSV files to a specified location. |

### Report run times, console output, return codes

|                                                                                            |                                                                                           |
|--------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------|
| **Method**                                                                                 | **Description**                                                                           |
| [`$output()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-output.md)             | Return the stdout and stderr of all chains or pretty print the output for a single chain. |
| [`$time()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-time.md)                 | Report total and chain-specific run times.                                                |
| [`$return_codes()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-return_codes.md) | Return the return codes from the CmdStan runs.                                            |

### Expose Stan functions and additional methods to R

|                                                                                                              |                                                                                           |
|--------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------|
| **Method**                                                                                                   | **Description**                                                                           |
| [`$expose_functions()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md)         | Expose Stan functions for use in R.                                                       |
| [`$init_model_methods()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-init_model_methods.md)       | Expose methods for log-probability, gradients, parameter constraining and unconstraining. |
| [`$log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-log_prob.md)                           | Calculate log-prob.                                                                       |
| [`$grad_log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-grad_log_prob.md)                 | Calculate log-prob and gradient.                                                          |
| [`$hessian()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-hessian.md)                             | Calculate log-prob, gradient, and hessian.                                                |
| [`$constrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-constrain_variables.md)     | Transform a set of unconstrained parameter values to the constrained scale.               |
| [`$unconstrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_variables.md) | Transform a set of parameter values to the unconstrained scale.                           |
| [`$unconstrain_draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_draws.md)         | Transform all parameter draws to the unconstrained scale.                                 |
| [`$variable_skeleton()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-variable_skeleton.md)         | Helper function to re-structure a vector of constrained parameter values.                 |

## See also

The CmdStanR website
([mc-stan.org/cmdstanr](https://mc-stan.org/cmdstanr/)) for online
documentation and tutorials.

The Stan and CmdStan documentation:

- Stan documentation:
  [mc-stan.org/users/documentation](https://mc-stan.org/users/documentation/)

- CmdStan User’s Guide:
  [mc-stan.org/docs/cmdstan-guide](https://mc-stan.org/docs/cmdstan-guide/)

Other fitted model objects:
[`CmdStanDiagnose`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanDiagnose.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md),
[`CmdStanLaplace`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanLaplace.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanPathfinder.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md)
