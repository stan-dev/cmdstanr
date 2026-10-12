# Extract sampler diagnostics after MCMC

Extract the values of sampler diagnostics for each iteration and chain
of MCMC. To instead get summaries of these diagnostics and associated
warning messages use the
[`$diagnostic_summary()`](https://mc-stan.org/cmdstanr/pr/1321/reference/fit-method-diagnostic_summary.md)
method.

## Usage

``` r
sampler_diagnostics(
  inc_warmup = FALSE,
  format = getOption("cmdstanr_draws_format", "draws_array")
)
```

## Arguments

- inc_warmup:

  (logical) Should warmup draws be included? Defaults to `FALSE`.

- format:

  (string) The draws format to return. See
  [draws](https://mc-stan.org/cmdstanr/pr/1321/reference/fit-method-draws.md)
  for details.

## Value

Depends on `format`, but the default is a 3-D
[`draws_array`](https://mc-stan.org/posterior/reference/draws_array.html)
object (iteration x chain x variable). The variables for Stan's default
MCMC algorithm are `"accept_stat__"`, `"stepsize__"`, `"treedepth__"`,
`"n_leapfrog__"`, `"divergent__"`, `"energy__"`.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanMCMC.md)

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("logistic")
sampler_diagnostics <- fit$sampler_diagnostics()
str(sampler_diagnostics)
#>  'draws_array' num [1:1000, 1:4, 1:6] 0.993 0.875 0.848 0.988 0.99 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:1000] "1" "2" "3" "4" ...
#>   ..$ chain    : chr [1:4] "1" "2" "3" "4"
#>   ..$ variable : chr [1:6] "accept_stat__" "stepsize__" "treedepth__" "n_leapfrog__" ...

library(posterior)
as_draws_df(sampler_diagnostics)
#> # A draws_df: 1000 iterations, 4 chains, and 6 variables
#>    accept_stat__ stepsize__ treedepth__ n_leapfrog__ divergent__ energy__
#> 1           0.99       0.82           3            7           0       66
#> 2           0.87       0.82           2            7           0       68
#> 3           0.85       0.82           2            3           0       70
#> 4           0.99       0.82           2            7           0       70
#> 5           0.99       0.82           3            7           0       67
#> 6           0.70       0.82           3            7           0       70
#> 7           0.88       0.82           2            7           0       73
#> 8           0.88       0.82           2            3           0       73
#> 9           1.00       0.82           2            3           0       70
#> 10          0.90       0.82           3            7           0       69
#> # ... with 3990 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}

# or specify format to get a data frame instead of calling as_draws_df
fit$sampler_diagnostics(format = "df")
#> # A draws_df: 1000 iterations, 4 chains, and 6 variables
#>    accept_stat__ stepsize__ treedepth__ n_leapfrog__ divergent__ energy__
#> 1           0.99       0.82           3            7           0       66
#> 2           0.87       0.82           2            7           0       68
#> 3           0.85       0.82           2            3           0       70
#> 4           0.99       0.82           2            7           0       70
#> 5           0.99       0.82           3            7           0       67
#> 6           0.70       0.82           3            7           0       70
#> 7           0.88       0.82           2            7           0       73
#> 8           0.88       0.82           2            3           0       73
#> 9           1.00       0.82           2            3           0       70
#> 10          0.90       0.82           3            7           0       69
#> # ... with 3990 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
# }
```
