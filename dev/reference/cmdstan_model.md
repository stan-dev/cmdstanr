# Create a new CmdStanModel object

Create a new
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object from a file containing a Stan program or from an existing Stan
executable. The
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object stores the path to a Stan program and compiled executable (once
created), and provides methods for fitting the model using Stan's
algorithms.

See the `compile` and `...` arguments for control over whether and how
compilation happens.

## Usage

``` r
cmdstan_model(stan_file = NULL, exe_file = NULL, compile = TRUE, ...)
```

## Arguments

- stan_file:

  (string) The path to a `.stan` file containing a Stan program. The
  helper function
  [`write_stan_file()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_file.md)
  is provided for cases when it is more convenient to specify the Stan
  program as a string. If `stan_file` is not specified then `exe_file`
  must be specified.

- exe_file:

  (string) The path to an existing Stan model executable. Can be
  provided instead of or in addition to `stan_file` (if `stan_file` is
  omitted some `CmdStanModel` methods like `$code()` and `$print()` will
  not work). This argument can only be used with CmdStan 2.27+.

- compile:

  (logical) Do compilation? The default is `TRUE`. If `FALSE`
  compilation can be done later via the
  [`$compile()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
  method.

- ...:

  Optionally, additional arguments to pass to the
  [`$compile()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
  method if `compile=TRUE`. These options include specifying the
  directory for saving the executable, turning on pedantic mode,
  specifying include paths, configuring C++ options, and more. See
  [`$compile()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
  for details.

## Value

A
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object.

## See also

[`install_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md),
[`$compile()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md),
[`$check_syntax()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md)

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
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

# Set path to CmdStan
# (Note: if you installed CmdStan via install_cmdstan() with default settings
# then setting the path is unnecessary but the default below should still work.
# Otherwise use the `path` argument to specify the location of your
# CmdStan installation.)
set_cmdstan_path(path = NULL)
#> CmdStan path set to: /home/runner/.cmdstan/cmdstan-2.37.0

# Create a CmdStanModel object from a Stan program,
# here using the example model that comes with CmdStan
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
# Print with line numbers. This can be set globally using the
# `cmdstanr_print_line_numbers` option.
mod$print(line_numbers = TRUE)
#>  1: data {
#>  2:   int<lower=0> N;
#>  3:   array[N] int<lower=0, upper=1> y;
#>  4: }
#>  5: parameters {
#>  6:   real<lower=0, upper=1> theta;
#>  7: }
#>  8: model {
#>  9:   theta ~ beta(1, 1); // uniform prior on interval 0,1
#> 10:   y ~ bernoulli(theta);
#> 11: }

# Data as a named list (like RStan)
stan_data <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

# Run MCMC using the 'sample' method
fit_mcmc <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 2,
  parallel_chains = 2
)
#> Running MCMC with 2 parallel chains...
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
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> 
#> Both chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.1 seconds.
#> 

# Use 'posterior' package for summaries
fit_mcmc$summary()
#> # A tibble: 2 × 10
#>   variable   mean median    sd   mad      q5    q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#> 1 lp__     -7.32  -7.02  0.799 0.355 -8.89   -6.75   1.00     819.    1020.
#> 2 theta     0.255  0.240 0.127 0.130  0.0773  0.491  1.00     557.     616.

# Check sampling diagnostics
fit_mcmc$diagnostic_summary()
#> $num_divergent
#> [1] 0 0
#> 
#> $num_max_treedepth
#> [1] 0 0
#> 
#> $ebfmi
#> [1] 1.114870 1.030279
#> 

# Get posterior draws
draws <- fit_mcmc$draws()
print(draws)
#> # A draws_array: 1000 iterations, 2 chains, and 2 variables
#> , , variable = lp__
#> 
#>          chain
#> iteration    1    2
#>         1 -7.0 -6.8
#>         2 -7.9 -6.9
#>         3 -7.4 -6.9
#>         4 -6.7 -6.8
#>         5 -6.9 -6.8
#> 
#> , , variable = theta
#> 
#>          chain
#> iteration    1    2
#>         1 0.17 0.28
#>         2 0.46 0.19
#>         3 0.41 0.19
#>         4 0.25 0.28
#>         5 0.18 0.23
#> 
#> # ... with 995 more iterations

# Convert to data frame using posterior::as_draws_df
as_draws_df(draws)
#> # A draws_df: 1000 iterations, 2 chains, and 2 variables
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
#> # ... with 1990 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}

# Plot posterior using bayesplot (ggplot2)
mcmc_hist(fit_mcmc$draws("theta"))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# Run 'optimize' method to get a point estimate (default is Stan's LBFGS algorithm)
# and also demonstrate specifying data as a path to a file instead of a list
my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.json")
fit_optim <- mod$optimize(data = my_data_file, seed = 123)
#> Initial log joint probability = -16.144 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>        6      -5.00402   0.000246518   8.73164e-07           1           1        9    
#> Optimization terminated normally:  
#>   Convergence detected: relative gradient magnitude is below tolerance 
#> Finished in  0.1 seconds.
fit_optim$summary()
#> # A tibble: 2 × 2
#>   variable estimate
#>   <chr>       <dbl>
#> 1 lp__       -5.00 
#> 2 theta       0.200

# Run 'optimize' again with 'jacobian=TRUE' and then draw from Laplace approximation
# to the posterior
fit_optim <- mod$optimize(data = my_data_file, jacobian = TRUE)
#> Initial log joint probability = -11.4115 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>        5      -6.74802    0.00104032   1.63334e-05           1           1        8    
#> Optimization terminated normally:  
#>   Convergence detected: relative gradient magnitude is below tolerance 
#> Finished in  0.1 seconds.
fit_laplace <- mod$laplace(data = my_data_file, mode = fit_optim, draws = 2000)
#> Calculating Hessian 
#> Calculating inverse of Cholesky factor 
#> Generating draws 
#> iteration: 0 
#> iteration: 100 
#> iteration: 200 
#> iteration: 300 
#> iteration: 400 
#> iteration: 500 
#> iteration: 600 
#> iteration: 700 
#> iteration: 800 
#> iteration: 900 
#> iteration: 1000 
#> iteration: 1100 
#> iteration: 1200 
#> iteration: 1300 
#> iteration: 1400 
#> iteration: 1500 
#> iteration: 1600 
#> iteration: 1700 
#> iteration: 1800 
#> iteration: 1900 
#> Finished in  0.1 seconds.
fit_laplace$summary()
#> # A tibble: 3 × 7
#>   variable      mean median    sd   mad      q5      q95
#>   <chr>        <dbl>  <dbl> <dbl> <dbl>   <dbl>    <dbl>
#> 1 lp__        -7.23  -6.98  0.685 0.315 -8.55   -6.75   
#> 2 lp_approx__ -0.500 -0.228 0.731 0.313 -1.87   -0.00129
#> 3 theta        0.265  0.249 0.121 0.121  0.0994  0.491  

# Run 'variational' method to use ADVI to approximate posterior
fit_vb <- mod$variational(data = stan_data, seed = 123)
#> ------------------------------------------------------------ 
#> EXPERIMENTAL ALGORITHM: 
#>   This procedure has not been thoroughly tested and may be unstable 
#>   or buggy. The interface is subject to change. 
#> ------------------------------------------------------------ 
#> Gradient evaluation took 2e-06 seconds 
#> 1000 transitions using 10 leapfrog steps per transition would take 0.02 seconds. 
#> Adjust your expectations accordingly! 
#> Begin eta adaptation. 
#> Iteration:   1 / 250 [  0%]  (Adaptation) 
#> Iteration:  50 / 250 [ 20%]  (Adaptation) 
#> Iteration: 100 / 250 [ 40%]  (Adaptation) 
#> Iteration: 150 / 250 [ 60%]  (Adaptation) 
#> Iteration: 200 / 250 [ 80%]  (Adaptation) 
#> Success! Found best value [eta = 1] earlier than expected. 
#> Begin stochastic gradient ascent. 
#>   iter             ELBO   delta_ELBO_mean   delta_ELBO_med   notes  
#>    100           -6.164             1.000            1.000 
#>    200           -6.225             0.505            1.000 
#>    300           -6.186             0.339            0.010   MEDIAN ELBO CONVERGED 
#> Drawing a sample of size 1000 from the approximate posterior...  
#> COMPLETED. 
#> Finished in  0.1 seconds.
fit_vb$summary()
#> # A tibble: 3 × 7
#>   variable      mean median    sd   mad     q5      q95
#>   <chr>        <dbl>  <dbl> <dbl> <dbl>  <dbl>    <dbl>
#> 1 lp__        -7.14  -6.93  0.528 0.247 -8.21  -6.75   
#> 2 lp_approx__ -0.520 -0.244 0.740 0.326 -1.90  -0.00227
#> 3 theta        0.251  0.236 0.107 0.108  0.100  0.446  
mcmc_hist(fit_vb$draws("theta"))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# Run 'pathfinder' method, a new alternative to the variational method
fit_pf <- mod$pathfinder(data = stan_data, seed = 123)
#> Path [1] :Initial log joint density = -18.273334 
#> Path [1] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               5      -6.748e+00      7.082e-04   1.432e-05    1.000e+00  1.000e+00       156 -6.216e+00 -6.145e+00                   
#> Path [1] :Best Iter: [5] ELBO (-6.145070) evaluations: (156) 
#> Path [2] :Initial log joint density = -19.192715 
#> Path [2] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               5      -6.748e+00      2.015e-04   2.228e-06    1.000e+00  1.000e+00       156 -6.170e+00 -6.223e+00                   
#> Path [2] :Best Iter: [2] ELBO (-6.170358) evaluations: (156) 
#> Path [3] :Initial log joint density = -6.774820 
#> Path [3] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               4      -6.748e+00      1.137e-04   2.596e-07    1.000e+00  1.000e+00       123 -6.243e+00 -6.178e+00                   
#> Path [3] :Best Iter: [4] ELBO (-6.177909) evaluations: (123) 
#> Path [4] :Initial log joint density = -7.949193 
#> Path [4] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               5      -6.748e+00      2.145e-04   1.301e-06    1.000e+00  1.000e+00       156 -6.235e+00 -6.197e+00                   
#> Path [4] :Best Iter: [5] ELBO (-6.197118) evaluations: (156) 
#> Finished in  0.1 seconds.
fit_pf$summary()
#> # A tibble: 4 × 7
#>   variable      mean median    sd   mad      q5    q95
#>   <chr>        <dbl>  <dbl> <dbl> <dbl>   <dbl>  <dbl>
#> 1 lp_approx__ -1.07  -0.724 0.871 0.306 -2.82   -0.451
#> 2 path__       2.46   2     1.12  1.48   1       4    
#> 3 lp__        -7.26  -6.97  0.720 0.304 -8.77   -6.75 
#> 4 theta        0.258  0.241 0.121 0.119  0.0840  0.475
mcmc_hist(fit_pf$draws("theta"))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# Run 'pathfinder' again with more paths, fewer draws per path,
# better covariance approximation, and fewer LBFGSs iterations
fit_pf <- mod$pathfinder(data = stan_data, num_paths=10, single_path_draws=40,
                         history_size=50, max_lbfgs_iters=100)
#> Warning: Number of PSIS draws is larger than the total number of draws returned by the single Pathfinders. This is likely unintentional and leads to re-sampling from the same draws. 
#> Path [1] :Initial log joint density = -7.541509 
#> Path [1] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               5      -6.748e+00      1.220e-04   5.203e-07    1.000e+00  1.000e+00       156 -6.228e+00 -6.188e+00                   
#> Path [1] :Best Iter: [5] ELBO (-6.187606) evaluations: (156) 
#> Path [2] :Initial log joint density = -6.951324 
#> Path [2] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               4      -6.748e+00      1.680e-03   2.312e-05    1.000e+00  1.000e+00       123 -6.234e+00 -6.217e+00                   
#> Path [2] :Best Iter: [4] ELBO (-6.217454) evaluations: (123) 
#> Path [3] :Initial log joint density = -8.444499 
#> Path [3] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               5      -6.748e+00      3.020e-04   2.291e-06    1.000e+00  1.000e+00       156 -6.195e+00 -6.212e+00                   
#> Path [3] :Best Iter: [3] ELBO (-6.195295) evaluations: (156) 
#> Path [4] :Initial log joint density = -7.136182 
#> Path [4] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               5      -6.748e+00      1.564e-04   1.111e-07    1.000e+00  1.000e+00       156 -6.233e+00 -6.227e+00                   
#> Path [4] :Best Iter: [5] ELBO (-6.226708) evaluations: (156) 
#> Path [5] :Initial log joint density = -13.899523 
#> Path [5] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               5      -6.748e+00      1.939e-03   5.211e-05    1.000e+00  1.000e+00       156 -6.184e+00 -6.201e+00                   
#> Path [5] :Best Iter: [4] ELBO (-6.183815) evaluations: (156) 
#> Path [6] :Initial log joint density = -14.787394 
#> Path [6] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               5      -6.748e+00      2.041e-03   5.986e-05    1.000e+00  1.000e+00       156 -6.177e+00 -6.214e+00                   
#> Path [6] :Best Iter: [4] ELBO (-6.176573) evaluations: (156) 
#> Path [7] :Initial log joint density = -7.325251 
#> Path [7] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               4      -6.748e+00      5.480e-03   1.650e-04    1.000e+00  1.000e+00       123 -6.231e+00 -6.128e+00                   
#> Path [7] :Best Iter: [4] ELBO (-6.127784) evaluations: (123) 
#> Path [8] :Initial log joint density = -14.766564 
#> Path [8] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               5      -6.748e+00      2.041e-03   5.977e-05    1.000e+00  1.000e+00       156 -6.220e+00 -6.209e+00                   
#> Path [8] :Best Iter: [5] ELBO (-6.209131) evaluations: (156) 
#> Path [9] :Initial log joint density = -6.870337 
#> Path [9] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               4      -6.748e+00      1.289e-04   1.880e-05    9.410e-01  9.410e-01       123 -6.258e+00 -6.238e+00                   
#> Path [9] :Best Iter: [4] ELBO (-6.238179) evaluations: (123) 
#> Path [10] :Initial log joint density = -7.310036 
#> Path [10] : Iter      log prob        ||dx||      ||grad||     alpha      alpha0      # evals       ELBO    Best ELBO        Notes  
#>               4      -6.748e+00      5.332e-03   1.577e-04    1.000e+00  1.000e+00       123 -6.304e+00 -6.212e+00                   
#> Path [10] :Best Iter: [4] ELBO (-6.211510) evaluations: (123) 
#> Pareto k value (0.74) is greater than 0.7. Importance resampling was not able to improve the approximation, which may indicate that the approximation itself is poor. 
#> Finished in  0.1 seconds.

# Specifying initial values as a function
fit_mcmc_w_init_fun <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 2,
  refresh = 0,
  init = function() list(theta = runif(1))
)
#> Running MCMC with 2 sequential chains...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> 
#> Both chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.3 seconds.
#> 
fit_mcmc_w_init_fun_2 <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 2,
  refresh = 0,
  init = function(chain_id) {
    # silly but demonstrates optional use of chain_id
    list(theta = 1 / (chain_id + 1))
  }
)
#> Running MCMC with 2 sequential chains...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> 
#> Both chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.3 seconds.
#> 
fit_mcmc_w_init_fun_2$init()
#> [[1]]
#> [[1]]$theta
#> [1] 0.5
#> 
#> 
#> [[2]]
#> [[2]]$theta
#> [1] 0.3333333
#> 
#> 

# Specifying initial values as a list of lists
fit_mcmc_w_init_list <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 2,
  refresh = 0,
  init = list(
    list(theta = 0.75), # chain 1
    list(theta = 0.25)  # chain 2
  )
)
#> Running MCMC with 2 sequential chains...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> 
#> Both chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.2 seconds.
#> 
fit_optim_w_init_list <- mod$optimize(
  data = stan_data,
  seed = 123,
  init = list(
    list(theta = 0.75)
  )
)
#> Initial log joint probability = -11.6657 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>        6      -5.00402   0.000237915   9.55309e-07           1           1        9    
#> Optimization terminated normally:  
#>   Convergence detected: relative gradient magnitude is below tolerance 
#> Finished in  0.1 seconds.
fit_optim_w_init_list$init()
#> [[1]]
#> [[1]]$theta
#> [1] 0.75
#> 
#> 
# }
```
