#' @section Arguments shared by all fitting methods: The following arguments can
#'   be specified for any of the fitting methods (`sample`, `optimize`,
#'   `variational`):
#'   * `data` (multiple options): The data to use:
#'      - A named list of \R objects like for RStan;
#'      - A path to a data file compatible with CmdStan.
#'   * `seed`: (positive integer) A seed for the (P)RNG to pass to CmdStan.
#'   * `refresh`: (non-negative integer) The number of iterations between
#'     screen updates.
#'   * `init`: (multiple options) The initialization method:
#'      - A real number `x>0` initializes randomly between `[-x,x]` (on the
#'      *unconstrained* parameter space);
#'      - `0` initializes to `0`;
#'      - A character vector of data file paths (one per chain) to
#'        initialization files.
