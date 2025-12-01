# Extract user-specified initial values

Return user-specified initial values. If the user provided initial
values files or R objects (list of lists or function) via the `init`
argument when fitting the model then these are returned (always in the
list of lists format). Currently it is not possible to extract initial
values generated automatically by CmdStan, although CmdStan may support
this in the future.

## Usage

``` r
init()
```

## Value

A list of lists. See **Examples**.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md)

## Examples

``` r
# \dontrun{
init_fun <- function() list(alpha = rnorm(1), beta = rnorm(3))
fit <- cmdstanr_example("logistic", init = init_fun, chains = 2)
str(fit$init())
#> List of 2
#>  $ :List of 2
#>   ..$ alpha: num 0.227
#>   ..$ beta : num [1:3] 0.978 -0.209 -1.399
#>  $ :List of 2
#>   ..$ alpha: num 0.259
#>   ..$ beta : num [1:3] -0.442 0.569 2.127

# partial inits (only specifying for a subset of parameters)
init_list <- list(
  list(mu = 10, tau = 2),
  list(mu = -10, tau = 1)
)
fit <- cmdstanr_example("schools_ncp", init = init_list, chains = 2, adapt_delta = 0.9)
#> Init values were only set for a subset of parameters. 
#> Missing init values for the following parameters:
#>  - chain 1: theta_raw
#>  - chain 2: theta_raw
#> 
#> To disable this message use options(cmdstanr_warn_inits = FALSE).

# only user-specified inits returned
str(fit$init())
#> List of 2
#>  $ :List of 2
#>   ..$ mu : int 10
#>   ..$ tau: int 2
#>  $ :List of 2
#>   ..$ mu : int -10
#>   ..$ tau: int 1
# }
```
