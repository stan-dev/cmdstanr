# CmdStanGQ objects

A `CmdStanGQ` object is the fitted model object returned by the
[`$generate_quantities()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-generate-quantities.md)
method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object.

## Methods

`CmdStanGQ` objects have the following associated methods, all of which
have their own (linked) documentation pages.

### Extract contents of generated quantities object

|                                                                                    |                                                                                                                 |
|------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------|
| **Method**                                                                         | **Description**                                                                                                 |
| [`$draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)       | Return the generated quantities as a [`draws_array`](https://mc-stan.org/posterior/reference/draws_array.html). |
| [`$metadata()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-metadata.md) | Return a list of metadata gathered from the CmdStan CSV files.                                                  |
| [`$code()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-code.md)         | Return Stan code as a character vector.                                                                         |

### Summarize inferences

|                                                                                  |                                                                                                   |
|----------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------|
| **Method**                                                                       | **Description**                                                                                   |
| [`$summary()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-summary.md) | Run [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html). |

### Save fitted model object and temporary files

|                                                                                                      |                                                |
|------------------------------------------------------------------------------------------------------|------------------------------------------------|
| **Method**                                                                                           | **Description**                                |
| [`$save_object()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_object.md)             | Save fitted model object to a file.            |
| [`$save_output_files()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md) | Save output CSV files to a specified location. |
| [`$save_data_file()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md)    | Save JSON data file to a specified location.   |

### Report run times, console output, return codes

|                                                                                            |                                                                                           |
|--------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------|
| **Method**                                                                                 | **Description**                                                                           |
| [`$time()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-time.md)                 | Report the total run time.                                                                |
| [`$output()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-output.md)             | Return the stdout and stderr of all chains or pretty print the output for a single chain. |
| [`$return_codes()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-return_codes.md) | Return the return codes from the CmdStan runs.                                            |

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
[`CmdStanLaplace`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanLaplace.md),
[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanPathfinder.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md)

## Examples

``` r
# \dontrun{
# first fit a model using MCMC
mcmc_program <- write_stan_file(
  "data {
    int<lower=0> N;
    array[N] int<lower=0,upper=1> y;
  }
  parameters {
    real<lower=0,upper=1> theta;
  }
  model {
    y ~ bernoulli(theta);
  }"
)
mod_mcmc <- cmdstan_model(mcmc_program)

data <- list(N = 10, y = c(1,1,0,0,0,1,0,1,0,0))
fit_mcmc <- mod_mcmc$sample(data = data, seed = 123, refresh = 0)
#> Running MCMC with 4 sequential chains...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> Chain 3 finished in 0.0 seconds.
#> Chain 4 finished in 0.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.6 seconds.
#> 

# stan program for standalone generated quantities
# (could keep model block, but not necessary so removing it)
gq_program <- write_stan_file(
  "data {
    int<lower=0> N;
    array[N] int<lower=0,upper=1> y;
  }
  parameters {
    real<lower=0,upper=1> theta;
  }
  generated quantities {
    array[N] int y_rep = bernoulli_rng(rep_vector(theta, N));
  }"
)

mod_gq <- cmdstan_model(gq_program)
fit_gq <- mod_gq$generate_quantities(fit_mcmc, data = data, seed = 123)
#> Running standalone generated quantities after 4 MCMC chains, 1 chain at a time ...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> Chain 3 finished in 0.0 seconds.
#> Chain 4 finished in 0.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.5 seconds.
str(fit_gq$draws())
#>  'draws_array' int [1:1000, 1:4, 1:10] 0 0 0 1 1 0 1 1 0 1 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:1000] "1" "2" "3" "4" ...
#>   ..$ chain    : chr [1:4] "1" "2" "3" "4"
#>   ..$ variable : chr [1:10] "y_rep[1]" "y_rep[2]" "y_rep[3]" "y_rep[4]" ...

library(posterior)
#> This is posterior version 1.6.1
#> 
#> Attaching package: ‘posterior’
#> The following objects are masked from ‘package:stats’:
#> 
#>     mad, sd, var
#> The following objects are masked from ‘package:base’:
#> 
#>     %in%, match
as_draws_df(fit_gq$draws())
#> # A draws_df: 1000 iterations, 4 chains, and 10 variables
#>    y_rep[1] y_rep[2] y_rep[3] y_rep[4] y_rep[5] y_rep[6] y_rep[7] y_rep[8]
#> 1         0        0        0        0        0        1        1        1
#> 2         0        0        0        0        1        1        0        0
#> 3         0        0        0        1        0        0        1        1
#> 4         1        1        0        0        0        0        1        0
#> 5         1        0        1        0        1        0        1        0
#> 6         0        0        0        1        1        0        0        0
#> 7         1        1        0        1        1        1        0        0
#> 8         1        1        1        1        1        0        1        1
#> 9         0        1        0        1        0        1        1        0
#> 10        1        1        1        1        1        1        1        1
#> # ... with 3990 more draws, and 2 more variables
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
# }
```
