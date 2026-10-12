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

## Examples

``` r
# \dontrun{
init_fun <- function() list(alpha = rnorm(1), beta = rnorm(3))
fit <- cmdstanr_example("logistic", init = init_fun, chains = 2)
str(fit$init())
#> List of 2
#>  $ :List of 2
#>   ..$ alpha: num -0.795
#>   ..$ beta : num [1:3] -1.57 -1.04 1.02
#>  $ :List of 2
#>   ..$ alpha: num -0.702
#>   ..$ beta : num [1:3] 0.9733 -0.0768 0.8929

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
