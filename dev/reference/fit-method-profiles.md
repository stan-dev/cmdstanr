# Return profiling data

The `$profiles()` method returns a list of data frames with profiling
data if any profiling data was written to the profile CSV files. See
[`save_profile_files()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md)
to control where the files are saved.

Support for profiling Stan programs is available with CmdStan \>= 2.26
and requires adding profiling statements to the Stan program.

## Usage

``` r
profiles()
```

## Value

A list of data frames with profiling data if the profiling CSV files
were created.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
# first fit a model using MCMC
mcmc_program <- write_stan_file(
  'data {
    int<lower=0> N;
    array[N] int<lower=0,upper=1> y;
  }
  parameters {
    real<lower=0,upper=1> theta;
  }
  model {
    profile("likelihood") {
      y ~ bernoulli(theta);
    }
  }
  generated quantities {
    array[N] int y_rep;
    profile("gq") {
      y_rep = bernoulli_rng(rep_vector(theta, N));
    }
  }
'
)
mod_mcmc <- cmdstan_model(mcmc_program)

data <- list(N = 10, y = c(1,1,0,0,0,1,0,1,0,0))
fit <- mod_mcmc$sample(data = data, seed = 123, refresh = 0)
#> Running MCMC with 4 sequential chains...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> Chain 3 finished in 0.0 seconds.
#> Chain 4 finished in 0.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.5 seconds.
#> 

fit$profiles()
#> [[1]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1         gq 139972978435904 0.000173650  0.000173650  0.000000000           0
#> 2 likelihood 139972978435904 0.000732015  0.000526632  0.000205383        6721
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1              0              0              1000
#> 2           6721           6721                 1
#> 
#> [[2]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1         gq 140466339747648 0.000165270  0.000165270  0.000000000           0
#> 2 likelihood 140466339747648 0.000818326  0.000610683  0.000207643        6792
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1              0              0              1000
#> 2           6792           6792                 1
#> 
#> [[3]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1         gq 139998794786624 0.000167139  0.000167139  0.000000000           0
#> 2 likelihood 139998794786624 0.000743371  0.000535433  0.000207938        6797
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1              0              0              1000
#> 2           6797           6797                 1
#> 
#> [[4]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1         gq 140349425620800 0.000165303  0.000165303  0.000000000           0
#> 2 likelihood 140349425620800 0.000762344  0.000539388  0.000222956        6979
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1              0              0              1000
#> 2           6979           6979                 1
#> 
# }
```
