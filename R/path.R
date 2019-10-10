#' Get or set the file path to the CmdStan installation
#'
#' `cmdstan_path()` returns the full path to the CmdStan installation. The path
#' can be set using the `set_cmdstan_path()` function. See **Details**.
#'
#' @export
#' @return The file path to the CmdStan installation.
#'
#' @details
#' Before the package can be used it needs to know where the CmdStan
#' installation is located. When the package is loaded it tries to help automate
#' this to avoid having to manually set the path every session:
#'
#' * If the [environment variable][Sys.setenv()] `"CMDSTAN"` exists at load time
#'   then its value will be automatically set as the default path to CmdStan for
#'   the \R session.
#' * If no environment variable is found when loaded but the directory
#'   `".cmdstanr/cmdstan"` exists in the user's home directory
#'   (`Sys.getenv("HOME")`, *not* the current working directory) then it will
#'   be set as the path to CmdStan for the \R session. This is the same as the
#'   default directory that [install_cmdstan()] would use to install the latest
#'   version of CmdStan.
#'
#' It is always possible to change the path after loading the package using
#' `set_cmdstan_path(path)`.
#'
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
#' @param path The full file path to the CmdStan installation as a string. If
#'   `NULL` (the default) then the path is set to the default path used by
#'   [install_cmdstan()] if it exists.
set_cmdstan_path <- function(path = NULL) {
  if (is.null(path)) {
    path <- cmdstan_default_path()
  }
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

# default path to symlink to latest cmdstan if installed via
# install_cmdstan() with default settings
cmdstan_default_path <- function() {
  # ".cmdstanr/cmdstan" is a symlink to latest version
  file.path(Sys.getenv("HOME"), ".cmdstanr/cmdstan")
}

# unset the path (only used in tests)
unset_cmdstan_path <- function() {
  .cmdstanr$PATH <- NULL
  .cmdstanr$VERSION <- NULL
}

# called in .onLoad() in zzz.R:
cmdstanr_initialize <- function() {
  # First check for environment variable CMDSTAN, but if not found
  # then see if default
  path <- Sys.getenv("CMDSTAN")
  if (isTRUE(nzchar(path))) { # CMDSTAN environment variable found
    if (dir.exists(path)) {
      path <- absolute_path(path)
      suppressMessages(set_cmdstan_path(path))
    } else {
      warning("Can't find directory specified by environment variable",
              " 'CMDSTAN'. Path not set.", call. = FALSE)
      .cmdstanr$PATH <- NULL
    }

  } else { # environment variable not found
    path <- cmdstan_default_path()
    if (dir.exists(path)) {
      suppressMessages(set_cmdstan_path(path))
    }
  }

  .cmdstanr$TEMP_DIR <- tempdir(check = TRUE)
  invisible(TRUE)
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
