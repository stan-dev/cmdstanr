# Run Stan's diagnose method

The `$diagnose()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object runs Stan's basic diagnostic feature that will calculate the
gradients of the initial state and compare them with gradients
calculated by finite differences. Discrepancies between the two indicate
that there is a problem with the model or initial states or else there
is a bug in Stan.

## Usage

``` r
diagnose(
  data = NULL,
  seed = NULL,
  init = NULL,
  output_dir = getOption("cmdstanr_output_dir"),
  output_basename = NULL,
  epsilon = NULL,
  error = NULL
)
```

## Arguments

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

- init:

  (multiple options) The initialization method to use for the variables
  declared in the parameters block of the Stan program. One of the
  following:

  - A real number `x>0`. This initializes *all* parameters randomly
    between `[-x,x]` on the *unconstrained* parameter space.;

  - The number `0`. This initializes *all* parameters to `0`;

  - A character vector of paths (one per chain) to JSON or Rdump files
    containing initial values for all or some parameters. See
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_json.md)
    to write R objects to JSON files compatible with CmdStan.

  - A list of lists containing initial values for all or some
    parameters. For MCMC the list should contain a sublist for each
    chain. For other model fitting methods there should be just one
    sublist. The sublists should have named elements corresponding to
    the parameters for which you are specifying initial values. See
    **Examples**.

  - A function that returns a single list with names corresponding to
    the parameters for which you are specifying initial values. The
    function can take no arguments or a single argument `chain_id`. For
    MCMC, if the function has argument `chain_id` it will be supplied
    with the chain id (from 1 to number of chains) when called to
    generate the initial values. See **Examples**.

  - A
    [`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
    [`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
    [`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
    [`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanPathfinder.md),
    or
    [`CmdStanLaplace`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanLaplace.md)
    fit object. If the fit object's parameters are only a subset of the
    model parameters then the other parameters will be drawn by Stan's
    default initialization. The fit object must have at least some
    parameters that are the same name and dimensions as the current Stan
    model. For the `sample` and `pathfinder` method, if the fit object
    has fewer draws than the requested number of chains/paths then the
    inits will be drawn using sampling with replacement. Otherwise
    sampling without replacement will be used. When a
    [`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanPathfinder.md)
    fit object is used as the init, if . `psis_resample` was set to
    `FALSE` and `calculate_lp` was set to `TRUE` (default), then
    resampling without replacement with Pareto smoothed weights will be
    used. If `psis_resample` was set to `TRUE` or `calculate_lp` was set
    to `FALSE` then sampling without replacement with uniform weights
    will be used to select the draws. PSIS resampling is used to select
    the draws for
    [`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
    and
    [`CmdStanLaplace`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanLaplace.md)
    fit objects.

  - A type inheriting from
    [`posterior::draws`](https://mc-stan.org/posterior/reference/draws.html).
    If the draws object has less samples than the number of requested
    chains/paths then the inits will be drawn using sampling with
    replacement. Otherwise sampling without replacement will be used. If
    the draws object's parameters are only a subset of the model
    parameters then the other parameters will be drawn by Stan's default
    initialization. The fit object must have at least some parameters
    that are the same name and dimensions as the current Stan model.

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

- epsilon:

  (positive real) The finite difference step size. Default value is
  1e-6.

- error:

  (positive real) The error threshold. Default value is 1e-6.

## Value

A
[`CmdStanDiagnose`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanDiagnose.md)
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
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/dev/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/dev/reference/model-method-generate-quantities.md),
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
test <- cmdstanr_example("logistic", method = "diagnose")

# retrieve the gradients
test$gradients()
#>   param_idx     value      model finite_diff        error
#> 1         0  0.319022   3.751800    3.751800  1.28391e-08
#> 2         1 -1.074650  -0.461525   -0.461525 -1.71248e-08
#> 3         2  1.095310 -19.869200  -19.869200 -1.71546e-08
#> 4         3 -1.702040  28.964000   28.964000 -2.61942e-08
# }
```
