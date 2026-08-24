# Report timing of CmdStan runs

Report the run time in seconds. For MCMC and standalone generated
quantities additional information is provided about the run times of
individual chains or processes. For MCMC, timing information is also
provided for the warmup and sampling phases. For Laplace approximation
the reported time includes only the time for drawing the approximate
sample and does not include the time taken to run the `$optimize()`
method.

## Usage

``` r
time()
```

## Value

A list with elements

- `total`: (scalar) The total run time. For MCMC and standalone
  generated quantities this may differ from the sum of the individual
  run times if parallelization was used.

- `chains`: (data frame) For MCMC and standalone generated quantities,
  timing information for the individual chains. For MCMC the data frame
  has columns `"chain_id"`, `"warmup"`, `"sampling"`, and `"total"`. For
  standalone generated quantities, each row corresponds to one
  fitted-parameter CSV file and one CmdStan process, and the data frame
  has columns `"chain_id"` and `"total"`. Variational or optimization
  input therefore produces one row. With CmdStan versions before 2.39,
  standalone generated quantities process times are reported as zero.

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample")
fit_mcmc$time()
#> $total
#> [1] 0.4653265
#> 
#> $chains
#>   chain_id warmup sampling total
#> 1        1  0.015    0.043 0.058
#> 2        2  0.015    0.042 0.057
#> 3        3  0.016    0.042 0.058
#> 4        4  0.015    0.041 0.056
#> 

fit_vb <- cmdstanr_example("logistic", method = "variational")
fit_vb$time()
#> $total
#> [1] 0.1112988
#> 

fit_optim <- cmdstanr_example("logistic", method = "optimize", jacobian = TRUE)
fit_optim$time()
#> $total
#> [1] 0.1109755
#> 

# use fit_optim to draw samples from laplace approximation
fit_laplace <- cmdstanr_example("logistic", method = "laplace", mode = fit_optim)
fit_laplace$time() # just time for drawing sample not for running optimize
#> $total
#> [1] 0.1101856
#> 
fit_laplace$time()$total + fit_optim$time()$total # total time
#> [1] 0.2211611
# }
```
