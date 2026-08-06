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
#> 1         gq 140714466465600 0.000174241  0.000174241  0.000000000           0
#> 2 likelihood 140714466465600 0.000732493  0.000524338  0.000208155        6721
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1              0              0              1000
#> 2           6721           6721                 1
#> 
#> [[2]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1 likelihood 140717637392192 0.000746904  0.000536899  0.000210005        6792
#> 2         gq 140717637392192 0.000168313  0.000168313  0.000000000           0
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1           6792           6792                 1
#> 2              0              0              1000
#> 
#> [[3]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1 likelihood 140252638684992 0.000739297  0.000529403  0.000209894        6797
#> 2         gq 140252638684992 0.000170880  0.000170880  0.000000000           0
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1           6797           6797                 1
#> 2              0              0              1000
#> 
#> [[4]]
#>         name       thread_id  total_time forward_time reverse_time chain_stack
#> 1 likelihood 140384760555328 0.000772808  0.000545527  0.000227281        6979
#> 2         gq 140384760555328 0.000172519  0.000172519  0.000000000           0
#>   no_chain_stack autodiff_calls no_autodiff_calls
#> 1           6979           6979                 1
#> 2              0              0              1000
#> 
# }
```
