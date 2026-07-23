#' @param data (multiple options) The data to use for the variables specified in
#'   the data block of the Stan program. One of the following:
#'  * A named list of \R objects with the names corresponding to variables
#'   declared in the data block of the Stan program. Internally this list is
#'   then written to JSON for CmdStan using [write_stan_json()]. See
#'   [write_stan_json()] for details on the conversions performed on \R objects
#'   before they are passed to Stan.
#'  * A path to a data file compatible with CmdStan (JSON or \R dump). See the
#'   appendices in the CmdStan guide for details on using these formats.
#'  * `NULL` or an empty list if the Stan program has no data block.
#'
#' @param seed (non-negative integer(s)) A seed for the (P)RNG to pass to
#'   CmdStan. In the case of multi-chain sampling the single `seed` will
#'   automatically be augmented by the run (chain) ID so that each chain uses a
#'   different seed. The exception is the transformed data block, which defaults
#'   to using the same seed for all chains so that the same data is generated for
#'   all chains if RNG functions are used. The only time `seed` should be
#'   specified as a vector (one element per chain) is if RNG functions are used
#'   in transformed data and the goal is to generate *different* data for each
#'   chain. Vector seeds retain this behavior when the method uses one CmdStan
#'   invocation per chain. In threaded multi-chain sampling or standalone
#'   generated quantities, all chains belong to one invocation, so CmdStanR uses
#'   the first value for every chain and warns about the effective seed.
#'
#' @param refresh (non-negative integer) The number of iterations between
#'   printed screen updates. If `refresh = 0`, only error messages will be
#'   printed.
#'
#' @param init (multiple options) The initialization method to use for the
#'   variables declared in the parameters block of the Stan program. One of the
#'   following:
#'  * A real number `x > 0`. This initializes _all_ parameters randomly between
#'  `[-x, x]` on the _unconstrained_ parameter space.
#'  * The number `0`. This initializes _all_ parameters to `0` on the
#'  _unconstrained_ parameter space.
#'  * A character vector of paths to JSON or Rdump files containing initial
#'  values for all or some parameters. For MCMC and Pathfinder, if only a single
#'  file is provided it will be reused for all chains and paths. See
#'  [write_stan_json()] to write \R objects to JSON files compatible with
#'  CmdStan.
#'  * A list of lists containing initial values for all or some parameters. For
#'  MCMC the list should contain a sublist for each chain, and for Pathfinder it
#'  should contain a sublist for each path. For other model fitting methods
#'  there should be just one sublist. The sublists should have named elements
#'  corresponding to the parameters for which you are specifying initial
#'  values. See **Examples**.
#'  * A function that returns a single list with names corresponding to the
#'  parameters for which you are specifying initial values. The function can
#'  take no arguments or a single argument `chain_id`. For MCMC and Pathfinder,
#'  the function is called once for each chain or path. If the function has the
#'  `chain_id` argument, it receives the chain or path number, starting at 1.
#'  See **Examples**.
#'  * A [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanPathfinder`],
#'  or [`CmdStanLaplace`] fit object. If the fit object's parameters are only a
#'  subset of the model parameters then the other parameters will be drawn by
#'  Stan's default initialization. The fit object must have at least some
#'  parameters that are the same name and dimensions as the current Stan model.
#'  For the `sample` and `pathfinder` methods, which need one initialization per
#'  chain or path, the inits are drawn from the fit object without replacement,
#'  so it must contain at least as many draws as the number of chains/paths. For
#'  [`CmdStanVB`], [`CmdStanLaplace`], and [`CmdStanPathfinder`] fit objects the
#'  draws must additionally be _distinct_. A [`CmdStanMLE`] fit object is the
#'  exception: its single draw (the mode) is used to initialize every chain or
#'  path. When a [`CmdStanPathfinder`] fit object is used as the init, if
#'  CmdStan actually performed PSIS resampling (which requires `num_paths > 1`,
#'  `psis_resample = TRUE`, and `calculate_lp = TRUE`), CmdStanR selects from
#'  the returned draws using uniform weights to avoid applying importance
#'  weights again. If CmdStan did not PSIS-resample the output and
#'  `calculate_lp = TRUE`, CmdStanR selects draws using Pareto-smoothed
#'  importance weights. This includes single-path fits with
#'  `psis_resample = TRUE`, because CmdStan does not PSIS-resample single-path
#'  output. If `calculate_lp = FALSE`, uniform weights are used because
#'  importance weights cannot be calculated. PSIS resampling is used to select
#'  the draws for [`CmdStanVB`], and [`CmdStanLaplace`] fit objects.
#'  * A type inheriting from `posterior::draws`. If the draws object has fewer
#'  draws than the number of requested chains/paths, the draws are reused in
#'  their existing order until each chain/path has an initialization. If there
#'  are more draws than requested chains/paths, draws are selected uniformly
#'  without replacement. If the draws object's parameters are only a subset of
#'  the model parameters then the other parameters will be drawn by Stan's
#'  default initialization. The draws object must have at least some parameters
#'  that are the same name and dimensions as the current Stan model.
#'
#' @param output_dir (string) A path to a directory where CmdStan should write
#'   its output CSV files. For MCMC and standalone generated quantities there
#'   will be one file per logical chain; for other methods there will be a
#'   single file. For interactive use this can typically
#'   be left at `NULL` (temporary directory) since CmdStanR makes the CmdStan
#'   output (posterior draws and diagnostics) available in \R via methods of the
#'   fitted model objects. This can be set for an entire \R session using
#'   `options(cmdstanr_output_dir)`. The behavior of `output_dir` is as follows:
#'   * If `NULL` (the default), then the CSV files are written to a temporary
#'   directory and only saved permanently if the user calls one of the `$save_*`
#'   methods of the fitted model object (e.g.,
#'   [`$save_output_files()`][fit-method-save_output_files]). These temporary
#'   files are removed when the fitted model object is [garbage
#'   collected][base::gc] (manually or automatically).
#'   * If a path, then the files are created in `output_dir` with names
#'   corresponding to the defaults used by `$save_output_files()`.
#'
#' @param output_basename (string) A string to use as a prefix for the names of
#'   the output CSV files of CmdStan. If `NULL` (the default), the basename of
#'   the output CSV files is composed of the model name, timestamp, and a
#'   six-character random hexadecimal suffix.
#'
#' @param sig_figs (positive integer) The number of significant figures (up to a
#'   maximum of 18) to use when storing the output values. If `NULL` (the
#'   default), the default from the installed CmdStan version is used. Use
#'   [`$cmdstan_defaults()`][model-method-cmdstan_defaults] to check that
#'   default. Increasing this value will result in larger output CSV files and
#'   thus an increased usage of disk space.
#'
#' @param opencl_ids (integer vector of length 2) The platform and device IDs of
#'   the OpenCL device to use for fitting. The model must be compiled with
#'   `cpp_options = list(stan_opencl = TRUE)` for this argument to have an
#'   effect.
#'
#' @param show_messages (logical) When `TRUE` (the default), prints all output
#'   during the execution process, such as iteration numbers and elapsed times.
#'   If the output is silenced then the [`$output()`][fit-method-output] method
#'   of the resulting fit object can be used to display the silenced messages.
#'
#' @param show_exceptions (logical) When `TRUE` (the default), prints all
#'   informational messages, for example rejection of the current proposal.
#'   Disable if you wish to silence these messages, but this is not usually
#'   recommended unless you are very confident that the model is correct up to
#'   numerical error. If the messages are silenced then the
#'   [`$output()`][fit-method-output] method of the resulting fit object can be
#'   used to display the silenced messages.
#'
#' @param save_cmdstan_config (logical) When `TRUE`, call CmdStan with argument
#'   `"output save_config=1"` to save a JSON file which contains the argument
#'   tree and extra information (equivalent to the output CSV file header). The
#'   default is `FALSE` but can be set to `TRUE` for an entire \R session by
#'   `options(cmdstanr_save_config = TRUE)`.
#'
