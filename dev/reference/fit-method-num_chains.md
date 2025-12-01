# Extract number of chains after MCMC

The `$num_chains()` method returns the number of MCMC chains.

## Usage

``` r
num_chains()
```

## Value

An integer.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example(chains = 2)
fit_mcmc$num_chains()
#> [1] 2
# }
```
