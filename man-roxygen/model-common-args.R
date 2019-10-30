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
#'     screen updates.
#'   * `init`: (multiple options) The initialization method:
#'     - A real number `x>0` initializes randomly between `[-x,x]` (on the
#'       *unconstrained* parameter space);
#'     - `0` initializes to `0`;
#'     - A character vector of data file paths (one per chain) to
#'       initialization files.
#'   * `save_diagnostics`: (logical) Should auxiliary diagnostic information
#'   (beyond standard diagnostics) be written to temporary diagnostic CSV files?
#'   This argument replaces CmdStan's `diagnostic_file` argument and the content
#'   written to CSV is controlled by the user's CmdStan installation and not
#'   CmdStanR (and for some algorithms no content may be written). The default
#'   is `save_diagnostics=FALSE`, which is appropriate for almost every use case
#'   (all diagnostics recommended for users to check are _always_ saved, e.g.,
#'   divergences for HMC). To save the temporary files created when
#'   `save_diagnostics=TRUE` see the
#'   [`$save_diagnostic_files()`][fit-method-save_diagnostic_files] method.
#'
