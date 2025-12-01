# Report timing of CmdStan runs

Report the run time in seconds. For MCMC additional information is
provided about the run times of individual chains and the warmup and
sampling phases. For Laplace approximation the time only include the
time for drawing the approximate sample and does not include the time
taken to run the `$optimize()` method.

## Usage

``` r
time()
```

## Value

A list with elements

- `total`: (scalar) The total run time. For MCMC this may be different
  than the sum of the chain run times if parallelization was used.

- `chains`: (data frame) For MCMC only, timing info for the individual
  chains. The data frame has columns `"chain_id"`, `"warmup"`,
  `"sampling"`, and `"total"`.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample")
fit_mcmc$time()
#> $total
#> [1] 0.5207887
#> 
#> $chains
#>   chain_id warmup sampling total
#> 1        1  0.019    0.057 0.076
#> 2        2  0.019    0.052 0.071
#> 3        3  0.019    0.057 0.076
#> 4        4  0.019    0.058 0.077
#> 

fit_vb <- cmdstanr_example("logistic", method = "variational")
fit_vb$time()
#> $total
#> [1] 0.1233594
#> 

fit_mle <- cmdstanr_example("logistic", method = "optimize", jacobian = TRUE)
fit_mle$time()
#> $total
#> [1] 0.1240866
#> 

# use fit_mle to draw samples from laplace approximation
fit_laplace <- cmdstanr_example("logistic", method = "laplace", mode = fit_mle)
fit_laplace$time() # just time for drawing sample not for running optimize
#> $total
#> [1] 0.1228893
#> 
fit_laplace$time()$total + fit_mle$time()$total # total time
#> [1] 0.2469759
# }
```
