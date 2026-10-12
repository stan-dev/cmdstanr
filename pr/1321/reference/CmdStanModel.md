# CmdStanModel objects

A `CmdStanModel` object is an
[R6](https://r6.r-lib.org/reference/R6Class.html) object created by the
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/pr/1321/reference/cmdstan_model.md)
function. The object stores the path to a Stan program and compiled
executable (once created), and provides methods for fitting the model
using Stan's algorithms.

## Methods

`CmdStanModel` objects have the following associated methods, many of
which have their own (linked) documentation pages:

### The Stan program

|  |  |
|----|----|
| **Method** | **Description** |
| [`$stan_file()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Return the file path to the Stan program. |
| [`$has_stan_file()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Check whether the model was created with a Stan file. |
| [`$model_name()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Return the model name. |
| [`$code()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Return Stan program as a character vector. |
| [`$print()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Print readable version of Stan program. |
| [`$include_paths()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Return the Stan include paths. |
| [`$variables()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-variables.md) | Return the input and output variables of the program, by block. |
| [`$check_syntax()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-check_syntax.md) | Check Stan syntax without having to compile. |
| [`$format()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-format.md) | Format and canonicalize the Stan model code. |

### The executable

|  |  |
|----|----|
| **Method** | **Description** |
| [`$exe_file()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Return the file path to the compiled executable. |
| [`$build_info()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-build_info.md) | Report how the executable was built, from its build record. |
| [`$is_current()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-is_current.md) | Check whether the model can run without a rebuild. |
| [`$cmdstan_version()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Return the CmdStan version that built the executable. |
| [`$cpp_options()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Return the C++ options associated with the model. |
| [`$user_header()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Return the path to the user header, if the model has one. |
| [`$hpp_file()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Return the file path to the `.hpp` file containing the generated C++ code. |
| [`$save_hpp_file()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md) | Save the `.hpp` file containing the generated C++ code. |
| [`$cmdstan_defaults()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-cmdstan_defaults.md) | Get CmdStan default argument values for a method. |
| [`$expose_functions()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-expose_functions.md) | Expose Stan functions for use in R. |

### Diagnostics

|  |  |
|----|----|
| **Method** | **Description** |
| [`$diagnose()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-diagnose.md) | Run CmdStan's `"diagnose"` method to test gradients, return [`CmdStanDiagnose`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanDiagnose.md) object. |

### Model fitting

|  |  |
|----|----|
| **Method** | **Description** |
| [`$sample()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-sample.md) | Run CmdStan's `"sample"` method, return [`CmdStanMCMC`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanMCMC.md) object. |
| [`$sample_mpi()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-sample_mpi.md) | Run CmdStan's `"sample"` method with [MPI](https://mc-stan.org/math/md_doxygen_2parallelism__support_2mpi__parallelism.html), return [`CmdStanMCMC`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanMCMC.md) object. |
| [`$optimize()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-optimize.md) | Run CmdStan's `"optimize"` method, return [`CmdStanMLE`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanMLE.md) object. |
| [`$laplace()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-laplace.md) | Run CmdStan's `"laplace"` method, return [`CmdStanLaplace`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanLaplace.md) object. |
| [`$variational()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-variational.md) | Run CmdStan's `"variational"` method, return [`CmdStanVB`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanVB.md) object. |
| [`$pathfinder()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-pathfinder.md) | Run CmdStan's `"pathfinder"` method, return [`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanPathfinder.md) object. |
| [`$generate_quantities()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-generate-quantities.md) | Run CmdStan's `"generate quantities"` method, return [`CmdStanGQ`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanGQ.md) object. |

## See also

The CmdStanR website
([mc-stan.org/cmdstanr](https://mc-stan.org/cmdstanr/)) for online
documentation and tutorials.

The Stan and CmdStan documentation:

- Stan documentation:
  [mc-stan.org/users/documentation](https://mc-stan.org/users/documentation/)

- CmdStan User’s Guide:
  [mc-stan.org/docs/cmdstan-guide](https://mc-stan.org/docs/cmdstan-guide/)

## Examples

``` r
# \dontrun{
library(cmdstanr)

# If you installed CmdStan with install_cmdstan() the path is already set.
# Otherwise point CmdStanR at your installation with set_cmdstan_path().
cmdstan_path()
#> [1] "/home/runner/.cmdstan/cmdstan-2.40.0"

# Create a CmdStanModel object from a Stan program. This compiles the
# program, or reuses the executable if it's already up to date.
file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
mod <- cmdstan_model(file)
mod$print()
#> data {
#>   int<lower=0> N;
#>   array[N] int<lower=0, upper=1> y;
#> }
#> parameters {
#>   real<lower=0, upper=1> theta;
#> }
#> model {
#>   theta ~ beta(1, 1); // uniform prior on interval 0,1
#>   y ~ bernoulli(theta);
#> }

# Options for the Stan compiler (stanc_options) and the C++ build
# (cpp_options). dir puts this executable in its own directory instead of
# next to the Stan file, so it doesn't replace the one mod uses. Replace
# tempdir() with a directory you'd keep.
mod_threads <- cmdstan_model(
  file,
  dir = tempdir(),
  stanc_options = list("O1"),
  cpp_options = list(stan_threads = TRUE)
)

# What an executable was built from
mod_threads$build_info()
#> Build record: available
#> Reported features:
#>   stan_threads: TRUE
#>   stan_mpi: FALSE
#>   stan_opencl: FALSE
#>   stan_no_range_checks: FALSE
#>   stan_version: 2.40.0
#> Configuration:
#>   cpp_options: STAN_THREADS=true
#>   stanc_options: --O1
#>   stanc_options_from_make: none
#>   include_paths: none
#> Dependencies:
#>   stan_file: /home/runner/.cmdstan/cmdstan-2.40.0/examples/bernoulli/bernoulli.stan
#>   make_local: /home/runner/.cmdstan/cmdstan-2.40.0/make/local
#> CmdStan 2.40.0 at /home/runner/.cmdstan/cmdstan-2.40.0

# Data is a named list matching the data block
stan_data <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

# Fit the model with MCMC
fit <- mod$sample(data = stan_data, seed = 123)
#> Running MCMC with 4 sequential chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 0.0 seconds.
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 finished in 0.0 seconds.
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 0.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.5 seconds.
#> 
fit$summary()
#> # A tibble: 2 × 10
#>   variable   mean median    sd   mad      q5    q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#> 1 lp__     -7.30  -7.01  0.805 0.345 -8.82   -6.75   1.00    1834.    2187.
#> 2 theta     0.257  0.243 0.124 0.126  0.0792  0.486  1.00    1165.    1581.
fit$diagnostic_summary()
#> $num_divergent
#> [1] 0 0 0 0
#> 
#> $num_max_treedepth
#> [1] 0 0 0 0
#> 
#> $ebfmi
#> [1] 1.114870 1.030279 1.053383 1.077471
#> 

# Posterior draws, here as a posterior::draws_df
fit$draws(format = "df")
#> # A draws_df: 1000 iterations, 4 chains, and 2 variables
#>    lp__ theta
#> 1  -7.0  0.17
#> 2  -7.9  0.46
#> 3  -7.4  0.41
#> 4  -6.7  0.25
#> 5  -6.9  0.18
#> 6  -6.9  0.33
#> 7  -7.2  0.15
#> 8  -6.8  0.29
#> 9  -6.8  0.24
#> 10 -6.8  0.24
#> # ... with 3990 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}

# Plot with bayesplot
bayesplot::mcmc_hist(fit$draws("theta"))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

# }
```
