# Run Stan's MCMC algorithms with MPI

The `$sample_mpi()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanModel.md)
object is identical to the `$sample()` method but with support for MPI
(message passing interface). The target audience for MPI are those with
large computer clusters. For other users, the
[`$sample()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-sample.md)
method provides both parallelization of chains and threading support for
within-chain parallelization.

In order to use MPI with Stan, an MPI implementation must be installed.
For Unix systems the most commonly used implementations are MPICH and
OpenMPI. The implementations provide an MPI C++ compiler wrapper (for
example mpicxx), which is required to compile the model.

An example of compiling with MPI:

    mpi_options = list(stan_mpi = TRUE, CXX = "mpicxx", TBB_CXX_TYPE = "gcc")
    mod = cmdstan_model("model.stan", cpp_options = mpi_options)

The C++ options that must be supplied to the
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/pr/1321/reference/cmdstan_model.md)
call are:

- `stan_mpi`: Enables the use of MPI with Stan if `TRUE`.

- `CXX`: The name of the MPI C++ compiler wrapper. Typically `"mpicxx"`.

- `TBB_CXX_TYPE`: The C++ compiler the MPI wrapper wraps. Typically
  `"gcc"` on Linux and `"clang"` on macOS.

In the call to the `$sample_mpi()` method it is also possible to provide
the name of the MPI launcher (`mpi_cmd`, defaulting to `"mpiexec"`) and
any other MPI launch arguments (`mpi_args`). In most cases, it is enough
to only define the number of processes. To use `n_procs` processes
specify `mpi_args = list("n" = n_procs)`.

## Usage

``` r
sample_mpi(
  data = NULL,
  mpi_cmd = "mpiexec",
  mpi_args = NULL,
  seed = NULL,
  refresh = NULL,
  init = NULL,
  save_latent_dynamics = FALSE,
  output_dir = getOption("cmdstanr_output_dir"),
  output_basename = NULL,
  chains = 1,
  chain_ids = seq_len(chains),
  iter_warmup = NULL,
  iter_sampling = NULL,
  save_warmup = FALSE,
  thin = NULL,
  max_treedepth = NULL,
  adapt_engaged = TRUE,
  adapt_delta = NULL,
  step_size = NULL,
  metric = NULL,
  metric_file = NULL,
  inv_metric = NULL,
  init_buffer = NULL,
  term_buffer = NULL,
  window = NULL,
  fixed_param = FALSE,
  sig_figs = NULL,
  show_messages = TRUE,
  show_exceptions = TRUE,
  show_progress_bar = getOption("cmdstanr_progress_bar", FALSE),
  show_iteration_messages = !show_progress_bar,
  diagnostics = c("divergences", "treedepth", "ebfmi"),
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
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/pr/1321/reference/write_stan_json.md).
    See
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/pr/1321/reference/write_stan_json.md)
    for details on the conversions performed on R objects before they
    are passed to Stan.

  - A path to a data file compatible with CmdStan (JSON or R dump). See
    the appendices in the CmdStan guide for details on using these
    formats.

  - `NULL` or an empty list if the Stan program has no data block.

- mpi_cmd:

  (string) The MPI launcher used for launching MPI processes. The
  default launcher is `"mpiexec"`.

- mpi_args:

  (list) A list of arguments to use when launching MPI processes. For
  example, `mpi_args = list("n" = 4)` launches the executable as
  `mpiexec -n 4 model_executable`, followed by CmdStan arguments for the
  model executable.

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
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/pr/1321/reference/write_stan_json.md)
    to write R objects to JSON files compatible with CmdStan.

  - A list of lists containing initial values for all or some
    parameters. For MCMC the list should contain a sublist for each
    chain, and for Pathfinder it should contain a sublist for each path.
    For other model fitting methods there should be just one sublist.
    The sublists should have named elements corresponding to the
    parameters for which you are specifying initial values. See
    **Examples**. A tuple parameter is given as an unnamed list of its
    elements and a complex parameter as an R complex value, see
    [`write_stan_json()`](https://mc-stan.org/cmdstanr/pr/1321/reference/write_stan_json.md).

  - A function that returns a single list with names corresponding to
    the parameters for which you are specifying initial values. The
    function can take no arguments or a single argument `chain_id`. For
    MCMC and Pathfinder, the function is called once for each chain or
    path. If the function has the `chain_id` argument, it receives the
    chain or path number, starting at 1. See **Examples**.

  - A
    [`CmdStanMCMC`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanMCMC.md),
    [`CmdStanMLE`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanMLE.md),
    [`CmdStanVB`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanVB.md),
    [`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanPathfinder.md),
    or
    [`CmdStanLaplace`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanLaplace.md)
    fit object. If the fit object's parameters are only a subset of the
    model parameters then the other parameters will be drawn by Stan's
    default initialization. The fit object must have at least some
    parameters that are the same name and dimensions as the current Stan
    model. For the `sample` and `pathfinder` methods, which need one
    initialization per chain or path, the inits are drawn from the fit
    object without replacement, so it must contain at least as many
    draws as the number of chains/paths. For
    [`CmdStanVB`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanVB.md),
    [`CmdStanLaplace`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanLaplace.md),
    and
    [`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanPathfinder.md)
    fit objects the draws must additionally be *distinct*. A
    [`CmdStanMLE`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanMLE.md)
    fit object is the exception: its single draw (the mode) is used to
    initialize every chain or path. When a
    [`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanPathfinder.md)
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
    [`CmdStanVB`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanVB.md),
    and
    [`CmdStanLaplace`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanLaplace.md)
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

- save_latent_dynamics:

  (logical) Should auxiliary diagnostic information about the sampler or
  variational algorithm be written to diagnostic CSV files? This
  argument replaces CmdStan's `diagnostic_file` argument. The content is
  controlled by the user's CmdStan installation. The default is `FALSE`,
  which is appropriate for almost every use case. To save temporary
  diagnostic files permanently, use
  [`$save_latent_dynamics_files()`](https://mc-stan.org/cmdstanr/pr/1321/reference/fit-method-save_output_files.md).

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
    [`$save_output_files()`](https://mc-stan.org/cmdstanr/pr/1321/reference/fit-method-save_output_files.md)).
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

- chains:

  (positive integer) The number of Markov chains to run. The default is
  4.

- chain_ids:

  (integer vector) A vector of chain IDs. Must contain as many unique
  positive integers as the number of chains. If not set, the default
  chain IDs are used (integers starting from `1`).

- iter_warmup:

  (positive integer) The number of warmup iterations to run per chain.
  Note: in the CmdStan User's Guide this is referred to as `num_warmup`.

- iter_sampling:

  (positive integer) The number of post-warmup iterations to run per
  chain. Note: in the CmdStan User's Guide this is referred to as
  `num_samples`.

- save_warmup:

  (logical) Should warmup iterations be saved? The default is `FALSE`.

- thin:

  (positive integer) The period between saved samples. This should
  typically be left at its default (no thinning) unless memory is a
  problem.

- max_treedepth:

  (positive integer) The maximum allowed tree depth for the NUTS engine.
  See the *Tree Depth* section of the CmdStan User's Guide for more
  details.

- adapt_engaged:

  (logical) Do warmup adaptation? The default is `TRUE`. If a
  precomputed inverse metric is specified via the `inv_metric` argument
  (or `metric_file`) then, if `adapt_engaged=TRUE`, Stan will use the
  provided inverse metric just as an initial guess during adaptation. To
  turn off adaptation when using a precomputed inverse metric set
  `adapt_engaged=FALSE`.

- adapt_delta:

  (real in `(0,1)`) The adaptation target acceptance statistic.

- step_size:

  (positive real) The *initial* step size for the discrete approximation
  to continuous Hamiltonian dynamics. This is further tuned during
  warmup.

- metric:

  (string) One of `"diag_e"`, `"dense_e"`, or `"unit_e"`, specifying the
  geometry of the base manifold. See the *Euclidean Metric* section of
  the CmdStan User's Guide for more details. To specify a precomputed
  (inverse) metric, see the `inv_metric` argument below.

- metric_file:

  (character vector) One path, used for all chains, or one path per
  chain to JSON or Rdump files compatible with CmdStan that contain
  precomputed inverse metrics. The `metric_file` argument is inherited
  from CmdStan but is confusing in that the entry in JSON or Rdump
  file(s) must be named `inv_metric`, referring to the *inverse* metric.
  An alternative to `metric_file` is the `inv_metric` argument (see
  below), which allows specifying an inverse metric directly using a
  vector or matrix from your R session.

- inv_metric:

  (vector, matrix, or list) A vector (if `metric = "diag_e"`) or a
  matrix (if `metric = "dense_e"`) for initializing the inverse metric.
  A single vector or matrix is used for all chains. To initialize the
  chains with different inverse metrics, provide a list containing one
  vector or matrix per chain. A length-one list is also used for all
  chains. This can be used as an alternative to the `metric_file`
  argument. The inverse metric is usually set to an estimate of the
  posterior covariance. See the `adapt_engaged` argument above for
  details about (and control over) how specifying a precomputed inverse
  metric interacts with adaptation.

- init_buffer:

  (nonnegative integer) Width of initial fast timestep adaptation
  interval during warmup.

- term_buffer:

  (nonnegative integer) Width of final fast timestep adaptation interval
  during warmup.

- window:

  (nonnegative integer) Initial width of slow timestep/metric adaptation
  interval.

- fixed_param:

  (logical) When `TRUE`, call CmdStan with argument
  `"algorithm=fixed_param"`. The default is `FALSE`. The fixed parameter
  sampler generates a new sample without changing the current state of
  the Markov chain; only generated quantities may change. This can be
  useful when, for example, trying to generate pseudo-data using the
  generated quantities block.

- sig_figs:

  (positive integer) The number of significant figures (up to a maximum
  of 18) to use when storing the output values. If `NULL` (the default),
  the default from the installed CmdStan version is used. Use
  [`$cmdstan_defaults()`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-cmdstan_defaults.md)
  to check that default. Increasing this value will result in larger
  output CSV files and thus an increased usage of disk space.

- show_messages:

  (logical) When `TRUE` (the default), prints all output during the
  execution process, such as iteration numbers and elapsed times. If the
  output is silenced then the
  [`$output()`](https://mc-stan.org/cmdstanr/pr/1321/reference/fit-method-output.md)
  method of the resulting fit object can be used to display the silenced
  messages.

- show_exceptions:

  (logical) When `TRUE` (the default), prints all informational
  messages, for example rejection of the current proposal. Disable if
  you wish to silence these messages, but this is not usually
  recommended unless you are very confident that the model is correct up
  to numerical error. If the messages are silenced then the
  [`$output()`](https://mc-stan.org/cmdstanr/pr/1321/reference/fit-method-output.md)
  method of the resulting fit object can be used to display the silenced
  messages.

- show_progress_bar:

  (logical) **Experimental** and may change in a later release. When
  `TRUE`, shows a progress bar through progressr in place of CmdStan's
  iteration lines. The default is `FALSE` but can be set to `TRUE` for
  an entire session by `options(cmdstanr_progress_bar = TRUE)`. The bar
  also needs progressr's reporting turned on once per session. See the
  **Progress bar** section below for details.

- show_iteration_messages:

  (logical) Whether to print CmdStan's iteration lines. The default is
  `!show_progress_bar`, so the progress bar replaces them.

- diagnostics:

  (character vector) The diagnostics to automatically check and warn
  about after sampling. Setting this to an empty string `""` or `NULL`
  can be used to prevent CmdStanR from automatically reading in the
  sampler diagnostics from CSV if you wish to manually read in the
  results and validate them yourself, for example using
  [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/pr/1321/reference/read_cmdstan_csv.md).
  The currently available diagnostics are `"divergences"`,
  `"treedepth"`, and `"ebfmi"` (the default is to check all of them).

  These diagnostics are also available after fitting. The
  [`$sampler_diagnostics()`](https://mc-stan.org/cmdstanr/pr/1321/reference/fit-method-sampler_diagnostics.md)
  method provides access to the diagnostic values for each iteration and
  the
  [`$diagnostic_summary()`](https://mc-stan.org/cmdstanr/pr/1321/reference/fit-method-diagnostic_summary.md)
  method provides summaries of the diagnostics and can regenerate the
  warning messages.

  Diagnostics like R-hat and effective sample size are *not* currently
  available via the `diagnostics` argument but can be checked after
  fitting using the
  [`$summary()`](https://mc-stan.org/cmdstanr/pr/1321/reference/fit-method-summary.md)
  method.

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
[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/pr/1321/reference/CmdStanMCMC.md)
object.

## Progress bar

### Setup

The bar needs two things: `show_progress_bar = TRUE`, either in each
`$sample()` call or once per session with
`options(cmdstanr_progress_bar = TRUE)`, and progressr's reporting
turned on once per session with `progressr::handlers(global = TRUE)`.
The following turns on the progress bar for an entire R session:

    options(cmdstanr_progress_bar = TRUE)
    progressr::handlers(global = TRUE)

This also works to enable the bar when another package such as brms
calls `$sample()` for you.

CmdStanR shows one bar for all chains rather than one per chain, because
older versions of RStudio's console only support single-line bars. The
default is to show the progress bar instead of CmdStan's iteration
lines, but those can be kept with `show_iteration_messages = TRUE`. The
per-chain timing lines print once the bar is done.

### Choosing a bar

The default progress bar is very simple. To customize it, select one of
the many available handlers, for example `progressr::handlers("cli")`.
The progressr vignette on
[handlers](https://progressr.futureverse.org/articles/progressr-11-handlers.html)
lists the bars it ships and their options.

### Overhead and the `refresh` argument

The bar advances with CmdStan's iteration lines, one every `refresh`
iterations (CmdStan's default is 100). With `refresh = 0` CmdStan prints
none, so no bar is shown. A small `refresh` adds overhead, because
CmdStanR reads every line CmdStan prints, with or without the bar. For a
model that samples quickly the overhead can be noticeable. For a slow
model it is small next to the sampling time, and `refresh = 1` gives the
smoothest bar.

### Quarto and R Markdown

In a Quarto or R Markdown document, write
`if (interactive()) progressr::handlers(global = TRUE)` in the setup
chunk, because the call fails while the document renders. Add
`options(cmdstanr_progress_bar = interactive())` to keep CmdStan's
iteration lines in the rendered output.

## References

- Hoffman, M. D., and Gelman, A. (2014). The No-U-Turn sampler:
  adaptively setting path lengths in Hamiltonian Monte Carlo. *Journal
  of Machine Learning Research*, 15(47), 1593-1623.

- Betancourt, M. (2017). A conceptual introduction to Hamiltonian Monte
  Carlo. arXiv:1701.02434. Appendix A describes Stan's dynamic HMC/NUTS
  implementation.

- Stan Development Team. Stan Reference Manual (Algorithms section):
  https://mc-stan.org/docs/reference-manual/

- Stan Development Team. Stan documentation:
  https://mc-stan.org/users/documentation/

- Stan Development Team. CmdStan User's Guide:
  https://mc-stan.org/docs/cmdstan-guide/

## See also

The Stan Math Library's documentation
([mc-stan.org/math](https://mc-stan.org/math/)) for more details on MPI
support in Stan.

Other CmdStanModel methods:
[`model-method-build_info`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-build_info.md),
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-check_syntax.md),
[`model-method-cmdstan_defaults`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-cmdstan_defaults.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-generate-quantities.md),
[`model-method-is_current`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-is_current.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-laplace.md),
[`model-method-model-info`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-model-info.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-sample.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/pr/1321/reference/model-method-variational.md)

## Examples

``` r
# \dontrun{
# mpi_options <- list(stan_mpi = TRUE, CXX = "mpicxx", TBB_CXX_TYPE = "gcc")
# mod <- cmdstan_model("model.stan", cpp_options = mpi_options)
# fit <- mod$sample_mpi(..., mpi_args = list("n" = 4))
# }
```
