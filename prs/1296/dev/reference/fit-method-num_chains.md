# Extract the number of chains

The `$num_chains()` method returns the number of chains in a
[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md)
object or the number of chains used for standalone generated quantities
in a
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)
object.

## Usage

``` r
num_chains()
```

## Value

An integer.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example(chains = 2)
fit_mcmc$num_chains()
#> [1] 2
# }
```
