# Extract return codes from CmdStan

The `$return_codes()` method returns a vector of return codes from the
CmdStan run(s). A return code of 0 indicates a successful run.

## Usage

``` r
return_codes()
```

## Value

An integer vector of return codes with length equal to the number of
CmdStan runs (number of chains for MCMC and one otherwise).

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
# example with return codes all zero
fit_mcmc <- cmdstanr_example("schools", method = "sample")
#> Warning: 198 of 4000 (5.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 4 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
fit_mcmc$return_codes() # should be all zero
#> [1] 0 0 0 0

# example of non-zero return code (optimization fails for hierarchical model)
fit_opt <- cmdstanr_example("schools", method = "optimize")
#> Chain 1 Optimization terminated with error: 
#> Chain 1   Line search failed to achieve a sufficient decrease, no more progress can be made
#> Warning: Fitting finished unexpectedly! Use the $output() method for more information.
fit_opt$return_codes() # should be non-zero
#> [1] 1
# }
```
