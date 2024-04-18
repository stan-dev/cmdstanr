#' @param data (multiple options) The data to use for the variables specified in
#'   the data block of the Stan program. One of the following:
#'  * A named list of \R objects with the names corresponding to variables
#'  declared in the data block of the Stan program. Internally this list is then
#'  written to JSON for CmdStan using [write_stan_json()]. See
#'  [write_stan_json()] for details on the conversions performed on \R objects
#'  before they are passed to Stan.
#'  * A path to a data file compatible with CmdStan (JSON or \R dump). See the
#'  appendices in the CmdStan guide for details on using these formats.
#'  * `NULL` or an empty list if the Stan program has no data block.
#'
#' @param seed (positive integer(s)) A seed for the (P)RNG to pass to CmdStan.
#'   In the case of multi-chain sampling the single `seed` will automatically be
#'   augmented by the the run (chain) ID so that each chain uses a different
#'   seed. The exception is the transformed data block, which defaults to
#'   using same seed for all chains so that the same data is generated for all
#'   chains if RNG functions are used. The only time `seed` should be specified
#'   as a vector (one element per chain) is if RNG functions are used in
#'   transformed data and the goal is to generate *different* data for each
#'   chain.
#'
#' @param refresh (non-negative integer) The number of iterations between
#'   printed screen updates. If `refresh = 0`, only error messages will be
#'   printed.
#'
#' @param init (multiple options) The initialization method to use for the
#'   variables declared in the parameters block of the Stan program. One of
#'   the following:
#'  * A real number `x>0`. This initializes _all_ parameters randomly between
#'  `[-x,x]` on the _unconstrained_ parameter space.;
#'  * The number `0`. This initializes _all_ parameters to `0`;
#'  * A character vector of paths (one per chain) to JSON or Rdump files
#'  containing initial values for all or some parameters. See
#'  [write_stan_json()] to write \R objects to JSON files compatible with
#'  CmdStan.
#'  * A list of lists containing initial values for all or some parameters. For
#'  MCMC the list should contain a sublist for each chain. For other model
#'  fitting methods there should be just one sublist. The sublists should have
#'  named elements corresponding to the parameters for which you are specifying
#'  initial values. See **Examples**.
#'  * A function that returns a single list with names corresponding to the
#'  parameters for which you are specifying initial values. The function can
#'  take no arguments or a single argument `chain_id`. For MCMC, if the function
#'  has argument `chain_id` it will be supplied with the chain id (from 1 to
#'  number of chains) when called to generate the initial values. See
#'  **Examples**.
#'  * A [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanPathfinder`],
#'  or [`CmdStanLaplace`] fit object.
#'  If the fit object's parameters are only a subset of the model
#'  parameters then the other parameters will be drawn by Stan's default
#'  initialization. The fit object must have at least some parameters that are the
#'  same name and dimensions as the current Stan model. For the `sample` and
#'  `pathfinder` method, if the fit object has fewer draws than the requested
#'  number of chains/paths then the inits will be drawn using sampling with
#'  replacement. Otherwise sampling without replacement will be used.
#'  When a [`CmdStanPathfinder`] fit object is used as the init, if
#'. `psis_resample` was set to `FALSE` and `calculate_lp` was
#'  set to `TRUE` (default), then resampling without replacement with Pareto
#'  smoothed weights will be used. If `psis_resample` was set to `TRUE` or
#'  `calculate_lp` was set to `FALSE` then sampling without replacement with
#'  uniform weights will be used to select the draws.
#'  PSIS resampling is used to select the draws for  [`CmdStanVB`],
#'  and [`CmdStanLaplace`] fit objects.
#'
#'  * A type inheriting from `posterior::draws`. If the draws object has less
#'  samples than the number of requested chains/paths then the inits will be
#'  drawn using sampling with replacement. Otherwise sampling without
#'  replacement will be used. If the draws object's parameters are only a subset
#'  of the model parameters then the other parameters will be drawn by Stan's
#'  default initialization. The fit object must have at least some parameters
#'  that are the same name and dimensions as the current Stan model.
#'
#' @param save_latent_dynamics (logical) Should auxiliary diagnostic information
#'   about the latent dynamics be written to temporary diagnostic CSV files?
#'   This argument replaces CmdStan's `diagnostic_file` argument and the content
#'   written to CSV is controlled by the user's CmdStan installation and not
#'   CmdStanR (for some algorithms no content may be written). The default
#'   is `FALSE`, which is appropriate for almost every use case. To save the
#'   temporary files created when `save_latent_dynamics=TRUE` see the
#'   [`$save_latent_dynamics_files()`][fit-method-save_latent_dynamics_files]
#'   method.
#'
#' @param output_dir (string) A path to a directory where CmdStan should write
#'   its output CSV files. For MCMC there will be one file per chain; for other
#'   methods there will be a single file. For interactive use this can typically
#'   be left at `NULL` (temporary directory) since CmdStanR makes the CmdStan
#'   output (posterior draws and diagnostics) available in \R via methods of the
#'   fitted model objects. The behavior of `output_dir` is as follows:
#'   * If `NULL` (the default), then the CSV files are written to a temporary
#'   directory and only saved permanently if the user calls one of the `$save_*`
#'   methods of the fitted model object (e.g.,
#'   [`$save_output_files()`][fit-method-save_output_files]). These temporary
#'   files are removed when the fitted model object is
#'   [garbage collected][base::gc] (manually or automatically).
#'   * If a path, then the files are created in `output_dir` with names
#'   corresponding to the defaults used by `$save_output_files()`.
#'
#' @param output_basename (string) A string to use as a prefix for the names of
#'   the output CSV files of CmdStan. If `NULL` (the default), the basename of
#'   the output CSV files will be comprised from the model name, timestamp, and
#'   5 random characters.
#'
#' @param sig_figs (positive integer) The number of significant figures used
#'   when storing the output values. By default, CmdStan represent the output
#'   values with 6 significant figures. The upper limit for `sig_figs` is 18.
#'   Increasing this value will result in larger output CSV files and thus an
#'   increased usage of disk space.
#'
#' @param opencl_ids (integer vector of length 2) The platform and
#'   device IDs of the OpenCL device to use for fitting. The model must
#'   be compiled with `cpp_options = list(stan_opencl = TRUE)` for this
#'   argument to have an effect.
#'
#' @param show_messages (logical) When `TRUE` (the default), prints all
#'   output during the execution process, such as iteration numbers and elapsed times.
#'   If the output is silenced then the [`$output()`][fit-method-output] method of
#'   the resulting fit object can be used to display the silenced messages.
#'
#' @param show_exceptions (logical) When `TRUE` (the default), prints all
#'   informational messages, for example rejection of the current proposal.
#'   Disable if you wish to silence these messages, but this is not usually
#'   recommended unless you are very confident that the model is correct up to
#'   numerical error. If the messages are silenced then the
#'   [`$output()`][fit-method-output] method of the resulting fit object can be
#'   used to display the silenced messages.
#'
