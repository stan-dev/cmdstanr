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
  path <- repair_path(path)

  if (is.null(.cmdstanr$VERSION)) {
    .cmdstanr$VERSION <- read_cmdstan_version(path)
  }

  path
}

#' @rdname cmdstan_path
#' @export
#' @param path The full file path to the CmdStan installation as a string.
set_cmdstan_path <- function(path) {
  if (dir.exists(path)) {
    path <- absolute_path(path)
    .cmdstanr$PATH <- path
    .cmdstanr$VERSION <- read_cmdstan_version(path)
    message("CmdStan path set to: ", path)
  } else {
    warning("Path not set. Can't find directory: ", path, call. = FALSE)
  }
  invisible(path)
}


# internal ----------------------------------------------------------------

# initialize internal environment to store path to cmdstan, cmdstan version
# number, and path to temp dir
.cmdstanr <- new.env(parent = emptyenv())
.cmdstanr$PATH <- NULL
.cmdstanr$VERSION <- NULL
.cmdstanr$TMP_DIR <- NULL

# path to temp directory
cmdstan_tempdir <- function() {
  .cmdstanr$TEMP_DIR
}

# cmdstan version number
cmdstan_version <- function() {
  .cmdstanr$VERSION
}

# unset the path (only used in tests)
unset_cmdstan_path <- function() {
  .cmdstanr$PATH <- NULL
  .cmdstanr$VERSION <- NULL
}

# called in .onLoad() in zzz.R:
cmdstanr_initialize <- function() {
  path <- Sys.getenv("CMDSTAN")
  if (isTRUE(nzchar(path))) {
    if (dir.exists(path)) {
      path <- absolute_path(path)
      .cmdstanr$PATH <- path
    } else {
      warning("Can't find directory specified by environment variable",
              " 'CMDSTAN'. Path not set.", call. = FALSE)
      .cmdstanr$PATH <- NULL
    }
  }

  .cmdstanr$TEMP_DIR <- tempdir(check = TRUE)
}


#' Find the version of cmdstan from makefile
#' @noRd
#' @param path Path to installation.
#' @return Version number as a string.
read_cmdstan_version <- function(path) {
  makefile_path <- file.path(path, "makefile")
  if (!file.exists(makefile_path)) {
    warning(
      "Can't find CmdStan makefile to detect version number. ",
      "Path may not point to valid installation.",
      call. = FALSE
    )
    return(NULL)
  }
  makefile <- readLines(makefile_path)
  version_line <- grep("^CMDSTAN_VERSION", makefile, value = TRUE)
  sub("CMDSTAN_VERSION := ", "", version_line)
}
