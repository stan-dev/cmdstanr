#' @param data (multiple options) The data to use for the variables specified in
#'   the `data` block of the Stan program. One of the following:
#'  * A named list of \R objects (like for RStan). Internally this list is then
#'  written to JSON for CmdStan using [write_stan_json()].
#'  * A path to a data file compatible with CmdStan (JSON or \R dump). See the
#'  appendices in the CmdStan manual for details on using these formats.
#'  * `NULL` or an empty list if the Stan program has no `data` block.
#'
#' @param seed (positive integer) A seed for the (P)RNG to pass to CmdStan.
#'
#' @param refresh (non-negative integer) The number of iterations between
#'   printed screen updates. If `refresh = 0`, only error messages will be
#'   printed.
#'
#' @param init (multiple options) The initialization method to use for the
#'   variables declared in the `parameters` block of the Stan program:
#'  * A real number `x>0`. This initializes _all_ parameters randomly between
#'  `[-x,x]` (on the _unconstrained_ parameter space);
#'  * The number `0`. This initializes _all_ parameters to `0`;
#'  * A character vector of paths (one per chain) to JSON or Rdump files
#'  containing initial values for all or some parameters. See
#'  [write_stan_json()] to write \R objects to JSON files compatible with
#'  CmdStan.
#'  * A list of lists containing initial values for all or some parameters. For
#'  MCMC the list should contain a sublist for each chain. For optimization and
#'  variational inference there should be just one sublist. The sublists should
#'  have named elements corresponding to the parameters for which you are
#'  specifying initial values. See **Examples**.
#'  * A function that returns a single list with names corresponding to the
#'  parameters for which you are specifying initial values. The function can
#'  take no arguments or a single argument `chain_id`. For MCMC, if the function
#'  has argument `chain_id` it will be supplied with the chain id (from 1 to
#'  number of chains) when called to generate the initial values. See
#'  **Examples**.
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
#'   its output CSV files. For interactive use this can typically be left at
#'   `NULL` (temporary directory) since CmdStanR makes the CmdStan output
#'   (posterior draws and diagnostics) available in \R via methods of the fitted
#'   model objects. The behavior of `output_dir` is as follows:
#'   * If `NULL` (the default), then the CSV files are written to a temporary
#'   directory and only saved permanently if the user calls one of the `$save_*`
#'   methods of the fitted model object (e.g.,
#'   [`$save_output_files()`][fit-method-save_output_files]). These temporary
#'   files are removed when the fitted model object is
#'   [garbage collected][base::gc] (manually or automatically).
#'   * If a path, then the files are created in `output_dir` with names
#'   corresponding to the defaults used by `$save_output_files()`.
#'
#' @param output_basename (string) A string to use as a prefix for the
#'   names of the output CSV files of CmdStan.
#'   * If `NULL` (the default), the basename of the output CSV files
#'   will be comprised from the model name, timestamp and 5 random characters.
#' 
#' @param sig_figs (positive integer) The number of significant figures used
#'   when storing the output values. By default, CmdStan represent the output
#'   values with 6 significant figures. The upper limit for `sig_figs` is 18.
#'   Increasing this value will result in larger output CSV files and thus an
#'   increased usage of disk space.
#'
