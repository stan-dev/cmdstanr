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
  if (is.null(path)) {
    stop("CmdStan path has not been set yet. See ?set_cmdstan_path.",
         call. = FALSE)
  }
  if (substr(path, nchar(path), nchar(path)) == "/") {
    # remove training "/" (is this necessary?)
    path <- substr(path, 1, nchar(path) - 1)
  }

  repair_path(path)
}

#' @rdname cmdstan_path
#' @export
#' @param path The full file path to the CmdStan installation as a string.
set_cmdstan_path <- function(path) {
  if (dir.exists(path)) {
    .cmdstanr$PATH <- absolute_path(path)
    message("CmdStan path set to: ", path)
  } else {
    warning("Path not set. Can't find directory: ", path, call. = FALSE)
  }
  invisible(path)
}


# internal ----------------------------------------------------------------

# initialize env and env vars
.cmdstanr <- new.env(parent = emptyenv())
.cmdstanr$PATH <- NULL
.cmdstanr$TMP_DIR <- NULL

cmdstan_tempdir <- function() {
  .cmdstanr$TEMP_DIR
}

unset_cmdstan_path <- function() {
  .cmdstanr$PATH <- NULL
}

# called in .onLoad() in zzz.R:
cmdstanr_initialize <- function() {
  path <- Sys.getenv("CMDSTAN")
  if (isTRUE(nzchar(path))) {
    if (dir.exists(path)) {
      .cmdstanr$PATH <- absolute_path(path)
    } else {
      warning("Can't find directory specified by environment variable",
              " 'CMDSTAN'. Path not set.", call. = FALSE)
      .cmdstanr$PATH <- NULL
    }
  }

  .cmdstanr$TEMP_DIR <- tempdir(check = TRUE)
}
