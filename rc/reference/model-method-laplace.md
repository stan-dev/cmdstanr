# Run Stan's Laplace algorithm

The `$laplace()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanModel.md)
object produces a sample from a normal approximation centered at the
mode of a distribution in the unconstrained space. When the mode was
found with the Jacobian adjustment, the draws provide an estimate of the
mean and standard deviation of the posterior distribution. See the
`jacobian` argument below for how this setting relates to the value used
when running optimization, and the [CmdStan User's
Guide](https://mc-stan.org/docs/cmdstan-guide/) for more details.

Any argument left as `NULL` will default to the default value used by
the installed version of CmdStan. See the [CmdStan User's
Guide](https://mc-stan.org/docs/cmdstan-guide/) for more details on the
default arguments. These values are also available via the
[`$cmdstan_defaults`](https://mc-stan.org/cmdstanr/rc/reference/model-method-cmdstan_defaults.md)
method.

## Usage

``` r
laplace(
  data = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  output_dir = getOption("cmdstanr_output_dir"),
  output_basename = NULL,
  sig_figs = NULL,
  threads = NULL,
  opencl_ids = NULL,
  mode = NULL,
  opt_args = NULL,
  jacobian = TRUE,
  draws = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE,
  save_cmdstan_config = getOption("cmdstanr_save_config", FALSE),
  compress = getOption("cmdstanr_compress", "none")
)
```

## Arguments

- data:

  (multiple options) The data to use for the variables specified in the
  data block of the Stan program. One of the following:

  - A named list of R objects with the names corresponding to variables
    declared in the data block of the Stan program. Internally this list
    is then written to JSON for CmdStan using
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_json.md).
    See
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_json.md)
    for details on the conversions performed on R objects before they
    are passed to Stan.

  - A path to a data file compatible with CmdStan (JSON or R dump). See
    the appendices in the CmdStan guide for details on using these
    formats.

  - `NULL` or an empty list if the Stan program has no data block.

- seed:

  (non-negative integer(s)) A seed for the (P)RNG to pass to CmdStan. In
  the case of multi-chain sampling the single `seed` will automatically
  be augmented by the run (chain) ID so that each chain uses a different
  seed. The exception is the transformed data block, which defaults to
  using same seed for all chains so that the same data is generated for
  all chains if RNG functions are used. The only time `seed` should be
  specified as a vector (one element per chain) is if RNG functions are
  used in transformed data and the goal is to generate *different* data
  for each chain.

- refresh:

  (non-negative integer) The number of iterations between printed screen
  updates. If `refresh = 0`, only error messages will be printed.

- init:

  (multiple options) The initialization method to use for the variables
  declared in the parameters block of the Stan program. One of the
  following:

  - A real number `x > 0`. This initializes *all* parameters randomly
    between `[-x, x]` on the *unconstrained* parameter space.

  - The number `0`. This initializes *all* parameters to `0` on the
    *unconstrained* parameter space.

  - A character vector of paths to JSON or Rdump files containing
    initial values for all or some parameters. For MCMC and Pathfinder,
    if only a single file is provided it will be reused for all chains
    and paths. See
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_json.md)
    to write R objects to JSON files compatible with CmdStan.

  - A list of lists containing initial values for all or some
    parameters. For MCMC the list should contain a sublist for each
    chain, and for Pathfinder it should contain a sublist for each path.
    For other model fitting methods there should be just one sublist.
    The sublists should have named elements corresponding to the
    parameters for which you are specifying initial values. See
    **Examples**. A tuple parameter is given as an unnamed list of its
    elements and a complex parameter as an R complex value, see
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_json.md).

  - A function that returns a single list with names corresponding to
    the parameters for which you are specifying initial values. The
    function can take no arguments or a single argument `chain_id`. For
    MCMC and Pathfinder, the function is called once for each chain or
    path. If the function has the `chain_id` argument, it receives the
    chain or path number, starting at 1. See **Examples**.

  - A
    [`CmdStanMCMC`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanMCMC.md),
    [`CmdStanMLE`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanMLE.md),
    [`CmdStanVB`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanVB.md),
    [`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanPathfinder.md),
    or
    [`CmdStanLaplace`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanLaplace.md)
    fit object. If the fit object's parameters are only a subset of the
    model parameters then the other parameters will be drawn by Stan's
    default initialization. The fit object must have at least some
    parameters that are the same name and dimensions as the current Stan
    model. For the `sample` and `pathfinder` methods, which need one
    initialization per chain or path, the inits are drawn from the fit
    object without replacement, so it must contain at least as many
    draws as the number of chains/paths. For
    [`CmdStanVB`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanVB.md),
    [`CmdStanLaplace`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanLaplace.md),
    and
    [`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanPathfinder.md)
    fit objects the draws must additionally be *distinct*. A
    [`CmdStanMLE`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanMLE.md)
    fit object is the exception: its single draw (the mode) is used to
    initialize every chain or path. When a
    [`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanPathfinder.md)
    fit object is used as the init, if CmdStan actually performed PSIS
    resampling (which requires `num_paths > 1`, `psis_resample = TRUE`,
    and `calculate_lp = TRUE`), CmdStanR selects from the returned draws
    using uniform weights to avoid applying importance weights again. If
    CmdStan did not PSIS-resample the output and `calculate_lp = TRUE`,
    CmdStanR selects draws using Pareto-smoothed importance weights.
    This includes single-path fits with `psis_resample = TRUE`, because
    CmdStan does not PSIS-resample single-path output. If
    `calculate_lp = FALSE`, uniform weights are used because importance
    weights cannot be calculated. PSIS resampling is used to select the
    draws for
    [`CmdStanVB`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanVB.md),
    and
    [`CmdStanLaplace`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanLaplace.md)
    fit objects.

  - A type inheriting from
    [`posterior::draws`](https://mc-stan.org/posterior/reference/draws.html).
    If the draws object has fewer draws than the number of requested
    chains/paths, the draws are reused in their existing order until
    each chain/path has an initialization. If there are more draws than
    requested chains/paths, draws are selected uniformly without
    replacement. If the draws object's parameters are only a subset of
    the model parameters then the other parameters will be drawn by
    Stan's default initialization. The draws object must have at least
    some parameters that are the same name and dimensions as the current
    Stan model.

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
    [`$save_output_files()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-save_output_files.md)).
    These temporary files are removed when the fitted model object is
    [garbage collected](https://rdrr.io/r/base/gc.html) (manually or
    automatically).

  - If a path, then the files are created in `output_dir` with names
    corresponding to the defaults used by `$save_output_files()`.

- output_basename:

  (string) A string to use as a prefix for the names of the output CSV
  files of CmdStan. If `NULL` (the default), the basename of the output
  CSV files is composed of the model name, timestamp, and a
  six-character random hexadecimal suffix.

- sig_figs:

  (positive integer) The number of significant figures (up to a maximum
  of 18) to use when storing the output values. If `NULL` (the default),
  the default from the installed CmdStan version is used. Use
  [`$cmdstan_defaults()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-cmdstan_defaults.md)
  to check that default. Increasing this value will result in larger
  output CSV files and thus an increased usage of disk space.

- threads:

  (positive integer) If the model was compiled with threading support,
  the number of threads to use in parallelized sections (e.g., when
  using the Stan functions `reduce_sum()` or `map_rect()`).

- opencl_ids:

  (integer vector of length 2) The platform and device IDs of the OpenCL
  device to use for fitting. The model must be compiled with
  `cpp_options = list(stan_opencl = TRUE)`, otherwise setting this
  argument is an error.

- mode:

  (multiple options) The mode to center the approximation at. One of the
  following:

  - A
    [`CmdStanMLE`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanMLE.md)
    object from a previous run of
    [`$optimize()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-optimize.md).

  - The path to a CmdStan CSV file (`.csv`, `.csv.gz` or `.csv.bz2`)
    from running optimization.

  - `NULL`, in which case
    [\$optimize()](https://mc-stan.org/cmdstanr/rc/reference/model-method-optimize.md)
    will be run with `jacobian=jacobian` (see the `jacobian` argument
    below).

  In all cases the total time reported by
  [`$time()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-time.md)
  will be the time of the Laplace sampling step only and does not
  include the time taken to run the `$optimize()` method.

- opt_args:

  (named list) A named list of optional arguments to pass to
  [\$optimize()](https://mc-stan.org/cmdstanr/rc/reference/model-method-optimize.md)
  if `mode=NULL`.

- jacobian:

  (logical) Whether or not to enable the Jacobian adjustment for
  constrained parameters. The default is `TRUE`. See the [Laplace
  Sampling](https://mc-stan.org/docs/cmdstan-guide/laplace_sample_config.html)
  section of the CmdStan User's Guide for more details. If `mode` is not
  `NULL` then the value of `jacobian` must match the value used when
  optimization was originally run so the mode and the Laplace
  approximation use the same target density. If `mode` is `NULL` then
  the value of `jacobian` specified here is used when running
  optimization.

- draws:

  (positive integer) The number of draws to take.

- show_messages:

  (logical) When `TRUE` (the default), prints all output during the
  execution process, such as iteration numbers and elapsed times. If the
  output is silenced then the
  [`$output()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-output.md)
  method of the resulting fit object can be used to display the silenced
  messages.

- show_exceptions:

  (logical) When `TRUE` (the default), prints all informational
  messages, for example rejection of the current proposal. Disable if
  you wish to silence these messages, but this is not usually
  recommended unless you are very confident that the model is correct up
  to numerical error. If the messages are silenced then the
  [`$output()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-output.md)
  method of the resulting fit object can be used to display the silenced
  messages.

- save_cmdstan_config:

  (logical) When `TRUE`, call CmdStan with argument
  `"output save_config=1"` to save a JSON file which contains the
  argument tree and extra information (equivalent to the output CSV file
  header). The default is `FALSE` but can be set to `TRUE` for an entire
  R session by `options(cmdstanr_save_config = TRUE)`.

- compress:

  (string) Compression to apply to the output CSV files after CmdStan
  finishes (including latent dynamics files, if saved): `"none"` (the
  default), `"gzip"`, or `"bzip2"`. The fit reads compressed files by
  decompressing them to a temporary copy first, which makes each read
  slower in exchange for using less disk space. The `cmdstanr_compress`
  option sets the default for every fitting method.

## Value

A
[`CmdStanLaplace`](https://mc-stan.org/cmdstanr/rc/reference/CmdStanLaplace.md)
object.

## References

- Stan Development Team. Stan Reference Manual (Algorithms section,
  Laplace approximation): https://mc-stan.org/docs/reference-manual/

- Stan Development Team. Stan documentation:
  https://mc-stan.org/users/documentation/

- Stan Development Team. CmdStan User's Guide:
  https://mc-stan.org/docs/cmdstan-guide/

## See also

Other CmdStanModel methods:
[`model-method-build_info`](https://mc-stan.org/cmdstanr/rc/reference/model-method-build_info.md),
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/rc/reference/model-method-check_syntax.md),
[`model-method-cmdstan_defaults`](https://mc-stan.org/cmdstanr/rc/reference/model-method-cmdstan_defaults.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/rc/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/rc/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/rc/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/rc/reference/model-method-generate-quantities.md),
[`model-method-is_current`](https://mc-stan.org/cmdstanr/rc/reference/model-method-is_current.md),
[`model-method-model-info`](https://mc-stan.org/cmdstanr/rc/reference/model-method-model-info.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/rc/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/rc/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/rc/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/rc/reference/model-method-sample_mpi.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/rc/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/rc/reference/model-method-variational.md)

## Examples

``` r
# \dontrun{
file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
mod <- cmdstan_model(file)
mod$print()
#> data {
#>   int<lower=0> N;
#>   array[N] int<lower=0, upper=1> y;
#> }
#> parameters {
#>   real<lower=0, upper=1> theta;
#> }
#> model {
#>   theta ~ beta(1, 1); // uniform prior on interval 0,1
#>   y ~ bernoulli(theta);
#> }

stan_data <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
fit_mode <- mod$optimize(data = stan_data, jacobian = TRUE)
#> Initial log joint probability = -18.5224 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>        5      -6.74802   0.000557352   1.00725e-05           1           1        8    
#> Optimization terminated normally:  
#>   Convergence detected: relative gradient magnitude is below tolerance 
#> Finished in  0.1 seconds.
fit <- mod$laplace(data = stan_data, mode = fit_mode)
#> Calculating Hessian 
#> Calculating inverse of Cholesky factor 
#> Generating draws 
#> iteration: 0 
#> iteration: 100 
#> iteration: 200 
#> iteration: 300 
#> iteration: 400 
#> iteration: 500 
#> iteration: 600 
#> iteration: 700 
#> iteration: 800 
#> iteration: 900 
#> Finished in  0.1 seconds.
fit$summary()
#> # A tibble: 3 × 7
#>   variable      mean median    sd   mad     q5      q95
#>   <chr>        <dbl>  <dbl> <dbl> <dbl>  <dbl>    <dbl>
#> 1 lp__        -7.22  -6.96  0.665 0.285 -8.64  -6.75   
#> 2 lp_approx__ -0.481 -0.207 0.684 0.286 -1.98  -0.00147
#> 3 theta        0.268  0.251 0.120 0.117  0.103  0.496  

# If mode isn't specified optimize is run internally first.
# show_messages = FALSE here and below hides CmdStan's output. That's to
# keep these examples short, not a recommendation.
fit <- mod$laplace(data = stan_data, show_messages = FALSE)
fit$summary()
#> # A tibble: 3 × 7
#>   variable      mean median    sd   mad      q5      q95
#>   <chr>        <dbl>  <dbl> <dbl> <dbl>   <dbl>    <dbl>
#> 1 lp__        -7.25  -6.99  0.745 0.319 -8.57   -6.75   
#> 2 lp_approx__ -0.514 -0.241 0.748 0.322 -1.85   -0.00224
#> 3 theta        0.271  0.257 0.124 0.127  0.0988  0.500  

# plot approximate posterior
bayesplot::mcmc_hist(fit$draws("theta"))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

# }
# \dontrun{
# The rest of these examples work the same way for every fitting method.

# Draws as a posterior draws object
fit$draws()
#> # A draws_matrix: 1000 iterations, 1 chains, and 3 variables
#>     variable
#> draw lp__ lp_approx__ theta
#>   1  -6.9       -0.13  0.19
#>   2  -7.1       -0.34  0.16
#>   3  -9.6       -2.44  0.59
#>   4  -8.2       -1.27  0.49
#>   5  -7.8       -0.97  0.46
#>   6  -7.8       -0.95  0.46
#>   7  -8.6       -1.58  0.52
#>   8  -7.1       -0.45  0.15
#>   9  -8.4       -1.40  0.50
#>   10 -7.5       -0.89  0.12
#> # ... with 990 more draws
fit$draws("theta", format = "df")
#> # A draws_df: 1000 iterations, 1 chains, and 1 variables
#>    theta
#> 1   0.19
#> 2   0.16
#> 3   0.59
#> 4   0.49
#> 5   0.46
#> 6   0.46
#> 7   0.52
#> 8   0.15
#> 9   0.50
#> 10  0.12
#> # ... with 990 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}

# Data as a path to a JSON or R dump file instead of a list
data_file <- file.path(
  cmdstan_path(),
  "examples/bernoulli/bernoulli.data.json"
)
fit <- mod$laplace(data = data_file, seed = 123, show_messages = FALSE)

# Initial values from a function
fit <- mod$laplace(
  data = stan_data,
  seed = 123,
  init = function() list(theta = runif(1)),
  show_messages = FALSE
)
fit$init()
#> [[1]]
#> [[1]]$theta
#> [1] 0.0959265
#> 
#> 

# Initial values from a previous fit (can be a fit from a different method)
fit2 <- mod$laplace(
  data = stan_data,
  seed = 123,
  init = fit,
  show_messages = FALSE
)

# Initial values from a posterior::draws object
inits <- fit$draws()
fit <- mod$laplace(
  data = stan_data,
  seed = 123,
  init = inits,
  show_messages = FALSE
)

# Suppress output from CmdStan while it runs
fit <- mod$laplace(
  data = stan_data,
  seed = 123,
  show_messages = FALSE,
  show_exceptions = FALSE
)

# The CSV files CmdStan wrote, in a temporary directory by default. Move
# them to keep them past the R session. Replace tempdir below with a
# directory you'll keep.
fit$output_files()
#> [1] "/tmp/RtmpggNdLO/bernoulli-202610151440-01-04d6f5.csv"

my_dir <- tempdir()
fit$save_output_files(dir = my_dir, basename = "bernoulli")
#> Moved 1 files and set internal paths to new locations:
#> - /tmp/RtmpggNdLO/bernoulli-202610151440-01-1b5c13.csv

# Or use output_dir to have CmdStan write them there in the first place,
# here gzip-compressed to save disk space
fit <- mod$laplace(
  data = stan_data,
  seed = 123,
  output_dir = my_dir,
  output_basename = "bernoulli",
  compress = "gzip",
  show_messages = FALSE
)
fit$output_files()
#> [1] "/tmp/RtmpggNdLO/bernoulli-01.csv.gz"
# }
```
