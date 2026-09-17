# Extract posterior draws

Extract draws or point estimates from fitted model objects using formats
provided by the posterior package. Depending on the fitting method,
these are posterior draws from MCMC, approximate posterior draws from
variational inference, Laplace approximation, or Pathfinder, standalone
generated quantities, or a point estimate from optimization.

The variables include the parameters, transformed parameters, and
generated quantities from the Stan program as well as `lp__`, the target
log density evaluated by Stan, up to an additive constant. See
[`$lp()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-lp.md)
for details.

## Usage

``` r
draws(
  variables = NULL,
  inc_warmup = FALSE,
  format = getOption("cmdstanr_draws_format")
)
```

## Arguments

- variables:

  (character vector) Optionally, the names of the variables (parameters,
  transformed parameters, and generated quantities) to read in.

  - If `NULL` (the default) then all variables are included.

  - If an empty string (`variables=""`) then none are included.

  - For non-scalar variables all elements or specific elements can be
    selected:

    - `variables = "theta"` selects all elements of `theta`;

    - `variables = c("theta[1]", "theta[3]")` selects only the 1st and
      3rd elements.

- inc_warmup:

  (logical) Should warmup draws be included? Defaults to `FALSE`.
  Ignored except when used with
  [CmdStanMCMC](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md)
  objects.

- format:

  (string) The format of the returned draws or point estimates. Must be
  a valid format from the posterior package. The defaults are the
  following.

  - For sampling and generated quantities the default is
    [`"draws_array"`](https://mc-stan.org/posterior/reference/draws_array.html).
    This format keeps the chains separate. To combine the chains use any
    of the other formats (e.g. `"draws_matrix"`).

  - For point estimates from optimization and approximate draws from
    variational inference, Laplace approximation, and Pathfinder the
    default is
    [`"draws_matrix"`](https://mc-stan.org/posterior/reference/draws_matrix.html).

  To use a different format it can be specified as the full name of the
  format from the posterior package (e.g. `format = "draws_df"`) or
  omitting the `"draws_"` prefix (e.g. `format = "df"`).

  **Changing the default format**: To change the default format for an
  entire R session use `options(cmdstanr_draws_format = format)`, where
  `format` is the name (in quotes) of a valid format from the posterior
  package. For example `options(cmdstanr_draws_format = "draws_df")`
  will change the default to a data frame.

  **Note about efficiency**: For models with a large number of
  parameters (20k+) we recommend using the `"draws_list"` format, which
  is the most efficient and RAM friendly when combining draws from
  multiple chains. If speed or memory is not a constraint we recommend
  selecting the format that most suits the coding style of the post
  processing phase.

## Value

Depends on the value of `format`. The defaults are:

- For
  [MCMC](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample.md),
  a 3-D
  [`draws_array`](https://mc-stan.org/posterior/reference/draws_array.html)
  object (iteration x chain x variable).

- For standalone [generated
  quantities](https://mc-stan.org/cmdstanr/dev/reference/model-method-generate-quantities.md),
  a 3-D
  [`draws_array`](https://mc-stan.org/posterior/reference/draws_array.html)
  object (iteration x chain x variable).

- For [variational
  inference](https://mc-stan.org/cmdstanr/dev/reference/model-method-variational.md)
  and [Laplace
  approximation](https://mc-stan.org/cmdstanr/dev/reference/model-method-laplace.md),
  a 2-D
  [`draws_matrix`](https://mc-stan.org/posterior/reference/draws_matrix.html)
  object (draw x variable). An additional variable `lp_approx__`
  containing the log density of the corresponding approximation is also
  included.

- For
  [Pathfinder](https://mc-stan.org/cmdstanr/dev/reference/model-method-pathfinder.md),
  a 2-D
  [`draws_matrix`](https://mc-stan.org/posterior/reference/draws_matrix.html)
  object (draw x variable). Additional variables `lp_approx__` and
  `path__` are also included, with `path__` identifying the path
  associated with each draw.

- For
  [optimization](https://mc-stan.org/cmdstanr/dev/reference/model-method-optimize.md),
  a 1-row
  [`draws_matrix`](https://mc-stan.org/posterior/reference/draws_matrix.html)
  with one column per variable. These are *not* actually draws, just
  point estimates stored in the `draws_matrix` format. See
  [`$mle()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-mle.md)
  to extract them as a numeric vector.

## Examples

``` r
# \dontrun{
# logistic regression with intercept alpha and coefficients beta
fit <- cmdstanr_example("logistic", method = "sample")

# returned as 3-D array (see ?posterior::draws_array)
draws <- fit$draws()
dim(draws)
#> [1] 1000    4  105
str(draws)
#>  'draws_array' num [1:1000, 1:4, 1:105] -64.6 -64.6 -65 -64.5 -64.5 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:1000] "1" "2" "3" "4" ...
#>   ..$ chain    : chr [1:4] "1" "2" "3" "4"
#>   ..$ variable : chr [1:105] "lp__" "alpha" "beta[1]" "beta[2]" ...

# can easily convert to other formats (data frame, matrix, list)
# using the posterior package
head(posterior::as_draws_matrix(draws))
#> # A draws_matrix: 6 iterations, 1 chains, and 105 variables
#>     variable
#> draw lp__ alpha beta[1] beta[2] beta[3] log_lik[1] log_lik[2] log_lik[3]
#>    1  -65  0.22   -0.72  -0.385    0.86      -0.59      -0.28      -0.58
#>    2  -65  0.52   -0.51  -0.098    0.63      -0.45      -0.44      -0.33
#>    3  -65  0.16   -0.45  -0.363    0.45      -0.63      -0.48      -0.75
#>    4  -64  0.42   -0.46  -0.234    0.45      -0.50      -0.55      -0.50
#>    5  -64  0.42   -0.46  -0.234    0.45      -0.50      -0.55      -0.50
#>    6  -65  0.66   -0.77  -0.087    0.58      -0.37      -0.47      -0.25
#> # ... with 97 more variables

# or can specify 'format' argument to avoid manual conversion
# matrix format combines all chains
draws <- fit$draws(format = "matrix")
head(draws)
#> # A draws_matrix: 6 iterations, 1 chains, and 105 variables
#>     variable
#> draw lp__ alpha beta[1] beta[2] beta[3] log_lik[1] log_lik[2] log_lik[3]
#>    1  -65  0.22   -0.72  -0.385    0.86      -0.59      -0.28      -0.58
#>    2  -65  0.52   -0.51  -0.098    0.63      -0.45      -0.44      -0.33
#>    3  -65  0.16   -0.45  -0.363    0.45      -0.63      -0.48      -0.75
#>    4  -64  0.42   -0.46  -0.234    0.45      -0.50      -0.55      -0.50
#>    5  -64  0.42   -0.46  -0.234    0.45      -0.50      -0.55      -0.50
#>    6  -65  0.66   -0.77  -0.087    0.58      -0.37      -0.47      -0.25
#> # ... with 97 more variables

# can select specific parameters
fit$draws("alpha")
#> # A draws_array: 1000 iterations, 4 chains, and 1 variables
#> , , variable = alpha
#> 
#>          chain
#> iteration    1       2      3       4
#>         1 0.22  0.4504  0.302  0.0374
#>         2 0.52  0.0236  0.347  0.3266
#>         3 0.16 -0.0037  0.675  0.3474
#>         4 0.42  0.0710 -0.014 -0.0021
#>         5 0.42  0.3987  0.431  0.1068
#> 
#> # ... with 995 more iterations
fit$draws("beta")  # selects entire vector beta
#> # A draws_array: 1000 iterations, 4 chains, and 3 variables
#> , , variable = beta[1]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.72 -0.51 -0.46 -0.19
#>         2 -0.51 -0.39 -0.71 -0.96
#>         3 -0.45 -0.53 -0.31 -0.34
#>         4 -0.46 -0.83 -0.56 -0.69
#>         5 -0.46 -0.96 -0.99 -0.89
#> 
#> , , variable = beta[2]
#> 
#>          chain
#> iteration      1      2       3     4
#>         1 -0.385 -0.349 -0.3685 -0.29
#>         2 -0.098 -0.396 -0.0247 -0.22
#>         3 -0.363  0.077 -0.3280 -0.27
#>         4 -0.234 -0.095 -0.0098 -0.23
#>         5 -0.234 -0.191 -0.5145 -0.22
#> 
#> , , variable = beta[3]
#> 
#>          chain
#> iteration    1    2    3    4
#>         1 0.86 0.70 0.85 0.70
#>         2 0.63 0.46 0.76 0.50
#>         3 0.45 0.80 0.17 0.86
#>         4 0.45 0.40 0.35 0.35
#>         5 0.45 0.35 0.28 0.30
#> 
#> # ... with 995 more iterations
fit$draws(c("alpha", "beta[2]"))
#> # A draws_array: 1000 iterations, 4 chains, and 2 variables
#> , , variable = alpha
#> 
#>          chain
#> iteration    1       2      3       4
#>         1 0.22  0.4504  0.302  0.0374
#>         2 0.52  0.0236  0.347  0.3266
#>         3 0.16 -0.0037  0.675  0.3474
#>         4 0.42  0.0710 -0.014 -0.0021
#>         5 0.42  0.3987  0.431  0.1068
#> 
#> , , variable = beta[2]
#> 
#>          chain
#> iteration      1      2       3     4
#>         1 -0.385 -0.349 -0.3685 -0.29
#>         2 -0.098 -0.396 -0.0247 -0.22
#>         3 -0.363  0.077 -0.3280 -0.27
#>         4 -0.234 -0.095 -0.0098 -0.23
#>         5 -0.234 -0.191 -0.5145 -0.22
#> 
#> # ... with 995 more iterations

# can be passed directly to bayesplot plotting functions
bayesplot::color_scheme_set("brightblue")
bayesplot::mcmc_dens(fit$draws(c("alpha", "beta")))

bayesplot::mcmc_scatter(fit$draws(c("beta[1]", "beta[2]")), alpha = 0.3)



# example using variational inference
fit <- cmdstanr_example("logistic", method = "variational")
head(fit$draws("beta")) # a matrix by default
#> # A draws_matrix: 6 iterations, 1 chains, and 3 variables
#>     variable
#> draw beta[1] beta[2] beta[3]
#>    1   -0.69   0.043    0.41
#>    2   -0.95  -0.515    1.25
#>    3   -0.83  -0.268    0.85
#>    4   -1.02  -0.516    0.51
#>    5   -0.32  -0.397    0.74
#>    6   -0.61  -0.331    0.90
head(fit$draws("beta", format = "df"))
#> # A draws_df: 6 iterations, 1 chains, and 3 variables
#>   beta[1] beta[2] beta[3]
#> 1   -0.69   0.043    0.41
#> 2   -0.95  -0.515    1.25
#> 3   -0.83  -0.268    0.85
#> 4   -1.02  -0.516    0.51
#> 5   -0.32  -0.397    0.74
#> 6   -0.61  -0.331    0.90
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
# }
```
