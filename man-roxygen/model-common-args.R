#' @section Arguments shared by all fitting methods: The following arguments can
#'   be specified for any of the fitting methods (`sample`, `optimize`,
#'   `variational`). Arguments left at `NULL` default to the default used by the
#'   installed version of CmdStan.
#'   * `data` (multiple options): The data to use:
#'      - A named list of \R objects like for RStan;
#'      - A path to a data file compatible with CmdStan (\R dump or JSON). See
#'        the appendices in the CmdStan manual for details on using these
#'        formats.
#'   * `seed`: (positive integer) A seed for the (P)RNG to pass to CmdStan.
#'   * `refresh`: (non-negative integer) The number of iterations between
#'     screen updates.
#'   * `init`: (multiple options) The initialization method:
#'      - A real number `x>0` initializes randomly between `[-x,x]` (on the
#'      *unconstrained* parameter space);
#'      - `0` initializes to `0`;
#'      - A character vector of data file paths (one per chain) to
#'        initialization files.
