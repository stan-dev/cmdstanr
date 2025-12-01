# Convert `CmdStanMCMC` to `mcmc.list`

This function converts a `CmdStanMCMC` object to an `mcmc.list` object
compatible with the coda package. This is primarily intended for users
of Stan coming from BUGS/JAGS who are used to coda for plotting and
diagnostics. In general we recommend the more recent MCMC diagnostics in
posterior and the ggplot2-based plotting functions in bayesplot, but for
users who prefer coda this function provides compatibility.

## Usage

``` r
as_mcmc.list(x)
```

## Arguments

- x:

  A
  [CmdStanMCMC](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md)
  object.

## Value

An `mcmc.list` object compatible with the coda package.

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example()
x <- as_mcmc.list(fit)
# }
```
