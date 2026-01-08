# Run Stan's standalone generated quantities method

The `$generate_quantities()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object runs Stan's standalone generated quantities to obtain generated
quantities based on previously fitted parameters.

## Usage

``` r
generate_quantities(
  fitted_params,
  data = NULL,
  seed = NULL,
  output_dir = getOption("cmdstanr_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  parallel_chains = getOption("mc.cores", 1),
  threads_per_chain = NULL,
  opencl_ids = NULL
)
```

## Arguments

- fitted_params:

  (multiple options) The parameter draws to use. One of the following:

  - A
    [CmdStanMCMC](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md)
    or
    [CmdStanVB](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md)
    fitted model object.

  - A
    [posterior::draws_array](https://mc-stan.org/posterior/reference/draws_array.html)
    (for MCMC) or
    [posterior::draws_matrix](https://mc-stan.org/posterior/reference/draws_matrix.html)
    (for VB) object returned by CmdStanR's
    [`$draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)
    method.

  - A character vector of paths to CmdStan CSV output files.

  NOTE: if you plan on making many calls to `$generate_quantities()`
  then the most efficient option is to pass the paths of the CmdStan CSV
  output files (this avoids CmdStanR having to rewrite the draws
  contained in the fitted model object to CSV each time). If you no
  longer have the CSV files you can use
  [`draws_to_csv()`](https://mc-stan.org/cmdstanr/dev/reference/draws_to_csv.md)
  once to write them and then pass the resulting file paths to
  `$generate_quantities()` as many times as needed.

- data:

  (multiple options) The data to use for the variables specified in the
  data block of the Stan program. One of the following:

  - A named list of R objects with the names corresponding to variables
    declared in the data block of the Stan program. Internally this list
    is then written to JSON for CmdStan using
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_json.md).
    See
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_json.md)
    for details on the conversions performed on R objects before they
    are passed to Stan.

  - A path to a data file compatible with CmdStan (JSON or R dump). See
    the appendices in the CmdStan guide for details on using these
    formats.

  - `NULL` or an empty list if the Stan program has no data block.

- seed:

  (positive integer(s)) A seed for the (P)RNG to pass to CmdStan. In the
  case of multi-chain sampling the single `seed` will automatically be
  augmented by the the run (chain) ID so that each chain uses a
  different seed. The exception is the transformed data block, which
  defaults to using same seed for all chains so that the same data is
  generated for all chains if RNG functions are used. The only time
  `seed` should be specified as a vector (one element per chain) is if
  RNG functions are used in transformed data and the goal is to generate
  *different* data for each chain.

- output_dir:

  (string) A path to a directory where CmdStan should write its output
  CSV files. For MCMC there will be one file per chain; for other
  methods there will be a single file. For interactive use this can
  typically be left at `NULL` (temporary directory) since CmdStanR makes
  the CmdStan output (posterior draws and diagnostics) available in R
  via methods of the fitted model objects. This can be set for an entire
  R session using `options(cmdstanr_output_dir)`. The behavior of
  `output_dir` is as follows:

  - If `NULL` (the default), then the CSV files are written to a
    temporary directory and only saved permanently if the user calls one
    of the `$save_*` methods of the fitted model object (e.g.,
    [`$save_output_files()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md)).
    These temporary files are removed when the fitted model object is
    [garbage collected](https://rdrr.io/r/base/gc.html) (manually or
    automatically).

  - If a path, then the files are created in `output_dir` with names
    corresponding to the defaults used by `$save_output_files()`.

- output_basename:

  (string) A string to use as a prefix for the names of the output CSV
  files of CmdStan. If `NULL` (the default), the basename of the output
  CSV files will be comprised from the model name, timestamp, and 5
  random characters.

- sig_figs:

  (positive integer) The number of significant figures used when storing
  the output values. By default, CmdStan represent the output values
  with 6 significant figures. The upper limit for `sig_figs` is 18.
  Increasing this value will result in larger output CSV files and thus
  an increased usage of disk space.

- parallel_chains:

  (positive integer) The *maximum* number of MCMC chains to run in
  parallel. If `parallel_chains` is not specified then the default is to
  look for the option `"mc.cores"`, which can be set for an entire R
  session by `options(mc.cores=value)`. If the `"mc.cores"` option has
  not been set then the default is `1`.

- threads_per_chain:

  (positive integer) If the model was
  [compiled](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
  with threading support, the number of threads to use in parallelized
  sections *within* an MCMC chain (e.g., when using the Stan functions
  `reduce_sum()` or `map_rect()`). This is in contrast with
  `parallel_chains`, which specifies the number of chains to run in
  parallel. The actual number of CPU cores used is
  `parallel_chains*threads_per_chain`. For an example of using threading
  see the Stan case study [Reduce Sum: A Minimal
  Example](https://mc-stan.org/users/documentation/case-studies/reduce_sum_tutorial.html).

- opencl_ids:

  (integer vector of length 2) The platform and device IDs of the OpenCL
  device to use for fitting. The model must be compiled with
  `cpp_options = list(stan_opencl = TRUE)` for this argument to have an
  effect.

## Value

A [`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)
object.

## See also

The CmdStanR website
([mc-stan.org/cmdstanr](https://mc-stan.org/cmdstanr/)) for online
documentation and tutorials.

The Stan and CmdStan documentation:

- Stan documentation:
  [mc-stan.org/users/documentation](https://mc-stan.org/users/documentation/)

- CmdStan User’s Guide:
  [mc-stan.org/docs/cmdstan-guide](https://mc-stan.org/docs/cmdstan-guide/)

Other CmdStanModel methods:
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md),
[`model-method-compile`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/dev/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/dev/reference/model-method-format.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/dev/reference/model-method-laplace.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/dev/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/dev/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample_mpi.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variational.md)

## Examples

``` r
# \dontrun{
# first fit a model using MCMC
mcmc_program <- write_stan_file(
  "data {
    int<lower=0> N;
    array[N] int<lower=0,upper=1> y;
  }
  parameters {
    real<lower=0,upper=1> theta;
  }
  model {
    y ~ bernoulli(theta);
  }"
)
mod_mcmc <- cmdstan_model(mcmc_program)

data <- list(N = 10, y = c(1,1,0,0,0,1,0,1,0,0))
fit_mcmc <- mod_mcmc$sample(data = data, seed = 123, refresh = 0)
#> Running MCMC with 4 sequential chains...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> Chain 3 finished in 0.0 seconds.
#> Chain 4 finished in 0.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.5 seconds.
#> 

# stan program for standalone generated quantities
# (could keep model block, but not necessary so removing it)
gq_program <- write_stan_file(
  "data {
    int<lower=0> N;
    array[N] int<lower=0,upper=1> y;
  }
  parameters {
    real<lower=0,upper=1> theta;
  }
  generated quantities {
    array[N] int y_rep = bernoulli_rng(rep_vector(theta, N));
  }"
)

mod_gq <- cmdstan_model(gq_program)
fit_gq <- mod_gq$generate_quantities(fit_mcmc, data = data, seed = 123)
#> Running standalone generated quantities after 4 MCMC chains, 1 chain at a time ...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> Chain 3 finished in 0.0 seconds.
#> Chain 4 finished in 0.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.4 seconds.
str(fit_gq$draws())
#>  'draws_array' int [1:1000, 1:4, 1:10] 0 0 0 1 1 0 1 1 0 1 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:1000] "1" "2" "3" "4" ...
#>   ..$ chain    : chr [1:4] "1" "2" "3" "4"
#>   ..$ variable : chr [1:10] "y_rep[1]" "y_rep[2]" "y_rep[3]" "y_rep[4]" ...

library(posterior)
as_draws_df(fit_gq$draws())
#> # A draws_df: 1000 iterations, 4 chains, and 10 variables
#>    y_rep[1] y_rep[2] y_rep[3] y_rep[4] y_rep[5] y_rep[6] y_rep[7] y_rep[8]
#> 1         0        0        0        0        0        1        1        1
#> 2         0        0        0        0        1        1        0        0
#> 3         0        0        0        1        0        0        1        1
#> 4         1        1        0        0        0        0        1        0
#> 5         1        0        1        0        1        0        1        0
#> 6         0        0        0        1        1        0        0        0
#> 7         1        1        0        1        1        1        0        0
#> 8         1        1        1        1        1        0        1        1
#> 9         0        1        0        1        0        1        1        0
#> 10        1        1        1        1        1        1        1        1
#> # ... with 3990 more draws, and 2 more variables
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
# }
```
