# Return profiling data

The `$profiles()` method returns a list of data frames with profiling
data if any profiling data was written to the profile CSV files. See
[`save_profile_files()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md)
to control where the files are saved.

Profiling requires adding profiling statements to the Stan program. See
**Examples** for a demonstration.

## Usage

``` r
profiles()
```

## Value

A list of data frames with profiling data if the profiling CSV files
were created.

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
#> 1         gq 139741480122176 0.000166421  0.000166421  0.000000000           0
#> 2 likelihood 139741480122176 0.000680569  0.000522425  0.000158144        6721
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1              0              0              1000
#> 2           6721           6721                 1
#> 
#> [[2]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1         gq 139655239468864 0.000178979  0.000178979   0.00000000           0
#> 2 likelihood 139655239468864 0.000682388  0.000526168   0.00015622        6792
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1              0              0              1000
#> 2           6792           6792                 1
#> 
#> [[3]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1 likelihood 139910338606912 0.000698027  0.000541395  0.000156632        6797
#> 2         gq 139910338606912 0.000176065  0.000176065  0.000000000           0
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1           6797           6797                 1
#> 2              0              0              1000
#> 
#> [[4]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1         gq 139704726275904 0.000234245  0.000234245  0.000000000           0
#> 2 likelihood 139704726275904 0.000729942  0.000565918  0.000164024        6979
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1              0              0              1000
#> 2           6979           6979                 1
#> 
# }
```
