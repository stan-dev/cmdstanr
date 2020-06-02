#' Get or set the file path to the CmdStan installation
#'
#' @description Use the `set_cmdstan_path()` function to tell CmdStanR where the
#'   CmdStan installation in located. Once the path has been set,
#'   `cmdstan_path()` will return the full path to the CmdStan installation and
#'   `cmdstan_version()` will return the CmdStan version number. See **Details**
#'   for how to avoid manually setting the path in each \R session.
#'
#' @export
#'
#' @param path The full file path to the CmdStan installation as a string. If
#'   `NULL` (the default) then the path is set to the default path used by
#'   [install_cmdstan()] if it exists.
#' @return A string. Either the file path to the CmdStan installation or the
#'   CmdStan version number.
#'
#' @details
#' Before the package can be used it needs to know where the CmdStan
#' installation is located. When the package is loaded it tries to help automate
#' this to avoid having to manually set the path every session:
#'
#' * If the [environment variable][Sys.setenv()] `"CMDSTAN"` exists at load time
#' then its value will be automatically set as the default path to CmdStan for
#' the \R session.
#' * If no environment variable is found when loaded but any directory in the form
#' `".cmdstanr/cmdstan-[version]"`, for example `".cmdstanr/cmdstan-2.23.0"`,
#' exists in the user's home directory (`Sys.getenv("HOME")`,
#' *not* the current working directory) then the path to the cmdstan with the largest
#' version number will be set as the path to CmdStan for the \R session.
#' This is the same as the default directory that [install_cmdstan()] would use to
#' install the latest version of CmdStan.
#'
#' It is always possible to change the path after loading the package using
#' `set_cmdstan_path(path)`.
#'
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

#' @rdname set_cmdstan_path
#' @export
cmdstan_path <- function() {
  path <- .cmdstanr$PATH %||% stop_no_path()
  path <- repair_path(path)

  if (is.null(.cmdstanr$VERSION)) {
    .cmdstanr$VERSION <- read_cmdstan_version(path)
  }

  path
}

#' @rdname set_cmdstan_path
#' @export
cmdstan_version <- function() {
  .cmdstanr$VERSION %||% stop_no_path()
}


# internal ----------------------------------------------------------------

# initialize internal environment to store path to cmdstan, cmdstan version
# number, and path to temp dir
.cmdstanr <- new.env(parent = emptyenv())
.cmdstanr$PATH <- NULL
.cmdstanr$VERSION <- NULL
.cmdstanr$TEMP_DIR <- NULL

# path to temp directory
cmdstan_tempdir <- function() {
  .cmdstanr$TEMP_DIR
}

# error message to throw if no path has been set
stop_no_path <- function() {
  stop("CmdStan path has not been set yet. See ?set_cmdstan_path.",
       call. = FALSE)
}

#' cmdstan_default_install_path
#'
#' Path to where  [install_cmdstan()] with default settings installs CmdStan.
#'
#' @keywords internal
#' @return The installation path.
#' @export
cmdstan_default_install_path <- function() {
  file.path(Sys.getenv("HOME"), ".cmdstanr")
}

#' cmdstan_default_path
#'
#' Returns the path to the installation of cmdstan with the most recent release version.
#'
#' @keywords internal
#' @return Path to the cmdstan installation with the most recent release version, NULL if no
#' installation found.
#' @export
cmdstan_default_path <- function() {
  installs_path <- file.path(Sys.getenv("HOME"), ".cmdstanr")
  if(dir.exists(installs_path)) {
    cmdstan_installs <- list.dirs(path = installs_path, recursive = FALSE, full.names = FALSE)
    is_rc <- sapply(cmdstan_installs, is_release_candidate)
    cmdstan_installs <- cmdstan_installs[!is_rc]
    # if installed in folder cmdstan, with no version
    # move to cmdstan-version folder
    if ("cmdstan" %in% cmdstan_installs) {
      ver <- read_cmdstan_version(file.path(installs_path, "cmdstan"))
      old_path <- file.path(installs_path, "cmdstan")
      new_path <- file.path(installs_path, paste0("cmdstan-",ver))
      file.rename(old_path, new_path)
      cmdstan_installs <- list.dirs(path = installs_path, recursive = FALSE, full.names = FALSE)
    }
    if(length(cmdstan_installs) > 0) {
      return(file.path(installs_path,sort(cmdstan_installs, decreasing = TRUE)[1]))
    }
  }  
  return(NULL)
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
    if (!is.null(path)) {
      suppressMessages(set_cmdstan_path(path))
    }
  }

  if (getRversion() < '3.5.0') {
    .cmdstanr$TEMP_DIR <- tempdir()
  } else {
    .cmdstanr$TEMP_DIR <- tempdir(check = TRUE)
  }
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
  version_line <- grep("^CMDSTAN_VERSION :=", makefile, value = TRUE)
  sub("CMDSTAN_VERSION := ", "", version_line)
}

#' Returns whether the supplied installation is a release candidate
#' @noRd
#' @param path Path to installation.
#' @return TRUE if the installation in the supplied path is a release candidate
is_release_candidate <- function(path) {
  if(endsWith(path,"/")) {
    path <- substr(path,1,nchar(path)-1)
  }
  if(length(grep(pattern="-rc[0-9]*$", x=path)) > 0){
    TRUE
  } else{
    FALSE
  }
}