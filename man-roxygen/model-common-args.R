#' @section Arguments shared by all fitting methods: The following arguments can
#'   be specified for any of the fitting methods (`sample`, `optimize`,
#'   `variational`). Arguments left at `NULL` default to the default used by the
#'   installed version of CmdStan.
#'   * `data`: (multiple options) The data to use. One of the following:
#'     - A named list of \R objects (like for RStan). Internally this list is
#'       then written to JSON for CmdStan using [write_stan_json()].
#'     - A path to a data file compatible with CmdStan (JSON or \R dump). See
#'       the appendices in the CmdStan manual for details on using these formats.
#'   * `seed`: (positive integer) A seed for the (P)RNG to pass to CmdStan.
#'   * `refresh`: (non-negative integer) The number of iterations between
#'   printed screen updates. If `refresh = 0`, only error messages will be printed.
#'   * `init`: (multiple options) The initialization method for the parameters block:
#'     - A real number `x>0` initializes randomly between `[-x,x]` (on the
#'       *unconstrained* parameter space);
#'     - `0` initializes to `0`;
#'     - A character vector of paths (one per chain) to JSON or Rdump files. See
#'       [write_stan_json()] to write \R objects to JSON files compatible with
#'       CmdStan.
#'     - A list of lists. For MCMC the list should contain a sublist for each
#'       chain. For optimization and variational inference there should be just one
#'       sublist. The sublists should have named elements corresponding to the
#'       parameters for which you are specifying initial values. See **Examples**.
#'     - A function that returns a single list with names corresponding to the
#'       parameters for which you are specifying initial values. The function
#'       can take no arguments or a single argument `chain_id`. For MCMC, if the
#'       function has argument `chain_id` it will be supplied with the chain id
#'       (from 1 to number of chains) when called to generate the initial
#'       values. See **Examples**.
#'   * `save_latent_dynamics`: (logical) Should auxiliary diagnostic information
#'   about the latent dynamics be written to temporary diagnostic CSV files?
#'   This argument replaces CmdStan's `diagnostic_file` argument and the content
#'   written to CSV is controlled by the user's CmdStan installation and not
#'   CmdStanR (and for some algorithms no content may be written). The default
#'   is `save_latent_dynamics=FALSE`, which is appropriate for almost every use case
#'   (all diagnostics recommended for users to check are _always_ saved, e.g.,
#'   divergences for HMC). To save the temporary files created when
#'   `save_latent_dynamics=TRUE` see the
#'   [`$save_latent_dynamics_files()`][fit-method-save_latent_dynamics_files] method.
#'   * `output_dir`: (string) A path to a directory where CmdStan should write
#'   its output CSV files. For interactive use this can typically be left at
#'   `NULL` (temporary directory) since CmdStanR makes the CmdStan output (e.g.,
#'   posterior draws and diagnostics) available in \R via methods of the fitted
#'   model objects. The behavior of `output_dir` is as follows:
#'     - If `NULL` (the default), then the CSV files are written to a temporary
#'       directory and only saved permanently if the user calls one of the
#'       `$save_*` methods of the fitted model object (e.g.,
#'       [`$save_output_files()`][fit-method-save_output_files]). These temporary
#'       files are removed when the fitted model object is garbage collected.
#'     - If a path, then the files are created in `output_dir` with names
#'       corresponding the defaults used by `$save_output_files()` (and similar
#'       methods like `$save_latent_dynamics_files()`).
#'   * `sig_figs`: (positive integer) The number of significant figures used
#'   for the output values. By default, CmdStan represent the output values with
#'   6 significant figures. The upper limit for `sig_figs` is 18. Increasing
#'   this value can cause an increased usage of disk space due to larger
#'   output CSV files.
#'
