# Get CmdStan default argument values

The `$cmdstan_defaults()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object queries the compiled model binary for the default argument values
used by a given inference method. The returned list uses cmdstanr-style
argument names (e.g., `iter_sampling` instead of CmdStan's
`num_samples`).

The model must be compiled before calling this method.

## Usage

``` r
cmdstan_defaults(
  method = c("sample", "optimize", "variational", "pathfinder", "laplace")
)
```

## Arguments

- method:

  (string) The inference method whose defaults to retrieve. One of
  `"sample"`, `"optimize"`, `"variational"`, `"pathfinder"`, or
  `"laplace"`.

## Value

A named list of default argument values for the specified method, with
cmdstanr-style argument names.

## See also

The CmdStanR website
([mc-stan.org/cmdstanr](https://mc-stan.org/cmdstanr/)) for online
documentation and tutorials.

The Stan and CmdStan documentation:

- Stan documentation:
  [mc-stan.org/users/documentation](https://mc-stan.org/users/documentation/)

- CmdStan User’s Guide:
  [mc-stan.org/docs/cmdstan-guide](https://mc-stan.org/docs/cmdstan-guide/)

Other CmdStanModel methods:
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md),
[`model-method-compile`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/dev/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/dev/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/dev/reference/model-method-generate-quantities.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/dev/reference/model-method-laplace.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/dev/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/dev/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample_mpi.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variational.md)

## Examples

``` r
# \dontrun{
mod <- cmdstan_model(file.path(cmdstan_path(),
                               "examples/bernoulli/bernoulli.stan"))
mod$cmdstan_defaults("sample")
#> $iter_sampling
#> [1] 1000
#> 
#> $iter_warmup
#> [1] 1000
#> 
#> $save_warmup
#> [1] FALSE
#> 
#> $thin
#> [1] 1
#> 
#> $adapt_engaged
#> [1] TRUE
#> 
#> $adapt_delta
#> [1] 0.8
#> 
#> $init_buffer
#> [1] 75
#> 
#> $term_buffer
#> [1] 50
#> 
#> $window
#> [1] 25
#> 
#> $save_metric
#> [1] FALSE
#> 
#> $max_treedepth
#> [1] 10
#> 
#> $metric
#> [1] "diag_e"
#> 
#> $metric_file
#> [1] ""
#> 
#> $step_size
#> [1] 1
#> 
mod$cmdstan_defaults("optimize")
#> $algorithm
#> [1] "lbfgs"
#> 
#> $init_alpha
#> [1] 0.001
#> 
#> $tol_obj
#> [1] 1e-12
#> 
#> $tol_rel_obj
#> [1] 10000
#> 
#> $tol_grad
#> [1] 1e-08
#> 
#> $tol_rel_grad
#> [1] 1e+07
#> 
#> $tol_param
#> [1] 1e-08
#> 
#> $history_size
#> [1] 5
#> 
#> $jacobian
#> [1] FALSE
#> 
#> $iter
#> [1] 2000
#> 
# }
```
