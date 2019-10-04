#' Get or set the file path to the CmdStan installation
#'
#' `cmdstan_path()` returns the full path to the CmdStan installation. This can
#' be set using the `set_cmdstan_path()` function. When the package is loaded,
#' if the [environment variable][Sys.setenv()] `CMDSTAN` exists its value will
#' be used as the path to CmdStan unless a different path is set using
#' `set_cmdstan_path()`.
#'
#' @export
#' @return The full file path to the CmdStan installation.
#'
cmdstan_path <- function() {
  path <- .cmdstanr$PATH
  if (substr(path, nchar(path), nchar(path)) == "/") {
    # remove training "/" (is this necessary?)
    path <- substr(path, 1, nchar(path) - 1)
  }
  path
}

#' @rdname cmdstan_path
#' @export
#' @param full_path The full file path to the CmdStan installation as a string.
set_cmdstan_path <- function(path) {
  if (!dir.exists(path)) {
    stop("Directory does not exist.", call. = FALSE)
  }
  .cmdstanr$PATH <- path
  invisible(path)
}

# instantiate --------------------------------------------------
.cmdstanr <- new.env(parent = emptyenv())
.cmdstanr$PATH <- Sys.getenv("CMDSTAN")
.cmdstanr$TEMP_DIR <- tempdir()


# internal ----------------------------------------------------------------
cmdstan_tempdir <- function() {
  .cmdstanr$TEMP_DIR
}

