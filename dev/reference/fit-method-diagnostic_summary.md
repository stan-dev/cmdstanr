# Sampler diagnostic summaries and warnings

Warnings and summaries of sampler diagnostics. To instead get the
underlying values of the sampler diagnostics for each iteration and
chain use the
[`$sampler_diagnostics()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-sampler_diagnostics.md)
method.

Currently parameter-specific diagnostics like R-hat and effective sample
size are *not* handled by this method. Those diagnostics are provided
via the
[`$summary()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-summary.md)
method (using
[`posterior::summarize_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html)).

## Usage

``` r
diagnostic_summary(
  diagnostics = c("divergences", "treedepth", "ebfmi"),
  quiet = FALSE
)
```

## Arguments

- diagnostics:

  (character vector) One or more diagnostics to check. The currently
  supported diagnostics are `"divergences`, `"treedepth"`, and `"ebfmi`.
  The default is to check all of them.

- quiet:

  (logical) Should warning messages about the diagnostics be suppressed?
  The default is `FALSE`, in which case warning messages are printed in
  addition to returning the values of the diagnostics.

## Value

A list with as many named elements as `diagnostics` selected. The
possible elements and their values are:

- `"num_divergent"`: A vector of the number of divergences per chain.

- `"num_max_treedepth"`: A vector of the number of times `max_treedepth`
  was hit per chain.

- `"ebfmi"`: A vector of E-BFMI values per chain.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md)
and the
[`$sampler_diagnostics()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-sampler_diagnostics.md)
method

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("schools")
#> Warning: 126 of 4000 (3.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
fit$diagnostic_summary()
#> Warning: 126 of 4000 (3.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> $num_divergent
#> [1] 28 34  4 60
#> 
#> $num_max_treedepth
#> [1] 0 0 0 0
#> 
#> $ebfmi
#> [1] 0.3853404 0.3642792 0.3826401 0.3337103
#> 
fit$diagnostic_summary(quiet = TRUE)
#> $num_divergent
#> [1] 28 34  4 60
#> 
#> $num_max_treedepth
#> [1] 0 0 0 0
#> 
#> $ebfmi
#> [1] 0.3853404 0.3642792 0.3826401 0.3337103
#> 
# }
```
