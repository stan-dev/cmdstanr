# Extract sampler diagnostics after MCMC

Extract the values of sampler diagnostics for each iteration and chain
of MCMC. To instead get summaries of these diagnostics and associated
warning messages use the
[`$diagnostic_summary()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-diagnostic_summary.md)
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
  [draws](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)
  for details.

## Value

Depends on `format`, but the default is a 3-D
[`draws_array`](https://mc-stan.org/posterior/reference/draws_array.html)
object (iteration x chain x variable). The variables for Stan's default
MCMC algorithm are `"accept_stat__"`, `"stepsize__"`, `"treedepth__"`,
`"n_leapfrog__"`, `"divergent__"`, `"energy__"`.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md)

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("logistic")
sampler_diagnostics <- fit$sampler_diagnostics()
str(sampler_diagnostics)
#>  'draws_array' num [1:1000, 1:4, 1:6] 3 2 2 2 2 2 2 3 3 2 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:1000] "1" "2" "3" "4" ...
#>   ..$ chain    : chr [1:4] "1" "2" "3" "4"
#>   ..$ variable : chr [1:6] "treedepth__" "divergent__" "energy__" "accept_stat__" ...

library(posterior)
as_draws_df(sampler_diagnostics)
#> # A draws_df: 1000 iterations, 4 chains, and 6 variables
#>    treedepth__ divergent__ energy__ accept_stat__ stepsize__ n_leapfrog__
#> 1            3           0       67          0.84       0.77            7
#> 2            2           0       68          0.75       0.77            3
#> 3            2           0       67          0.92       0.77            3
#> 4            2           0       69          0.69       0.77            3
#> 5            2           0       65          0.95       0.77            3
#> 6            2           0       66          0.96       0.77            7
#> 7            2           0       65          1.00       0.77            3
#> 8            3           0       66          0.96       0.77            7
#> 9            3           0       65          0.97       0.77            7
#> 10           2           0       67          0.83       0.77            3
#> # ... with 3990 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}

# or specify format to get a data frame instead of calling as_draws_df
fit$sampler_diagnostics(format = "df")
#> # A draws_df: 1000 iterations, 4 chains, and 6 variables
#>    treedepth__ divergent__ energy__ accept_stat__ stepsize__ n_leapfrog__
#> 1            3           0       67          0.84       0.77            7
#> 2            2           0       68          0.75       0.77            3
#> 3            2           0       67          0.92       0.77            3
#> 4            2           0       69          0.69       0.77            3
#> 5            2           0       65          0.95       0.77            3
#> 6            2           0       66          0.96       0.77            7
#> 7            2           0       65          1.00       0.77            3
#> 8            3           0       66          0.96       0.77            7
#> 9            3           0       65          0.97       0.77            7
#> 10           2           0       67          0.83       0.77            3
#> # ... with 3990 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
# }
```
