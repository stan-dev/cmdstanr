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
#' @param path (string) The full file path to the CmdStan installation. If
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
#' the \R session. If the environment variable `"CMDSTAN"` is set, but a valid
#' CmdStan is not found in the supplied path, the path is treated as a top
#' folder that contains CmdStan installations. In that case, the CmdStan
#' installation with the largest version number will be set as the path to
#' CmdStan for the \R session.
#' * If no environment variable is found when loaded but any directory in the
#' form `".cmdstan/cmdstan-[version]"` (e.g., `".cmdstan/cmdstan-2.23.0"`),
#' exists in the user's home directory (`Sys.getenv("HOME")`, *not* the current
#' working directory) then the path to the cmdstan with the largest version
#' number will be set as the path to CmdStan for the \R session. This is the
#' same as the default directory that [install_cmdstan()] would use to install
#' the latest version of CmdStan.
#'
#' It is always possible to change the path after loading the package using
#' `set_cmdstan_path(path)`.
#'
set_cmdstan_path <- function(path = NULL) {
  if (is.null(path)) {
    path <- cmdstan_default_path() %||% cmdstan_default_path(old = TRUE)
  }
  if (dir.exists(path)) {
    path <- absolute_path(path)
    .cmdstanr$PATH <- path
    .cmdstanr$VERSION <- read_cmdstan_version(path)
    .cmdstanr$WSL <- grepl("//wsl$", path, fixed = TRUE)
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
#' @param error_on_NA (logical) Should an error be thrown if CmdStan is not
#'   found. The default is `TRUE`. If `FALSE`, `cmdstan_version()` returns
#'   `NULL`.
#' @return CmdStan version string if available. If CmdStan is not found and
#'   `error_on_NA` is `FALSE`, `cmdstan_version()` returns `NULL`.
cmdstan_version <- function(error_on_NA = TRUE) {
  version <- .cmdstanr$VERSION
  if (is.null(version) && error_on_NA) {
    stop_no_path()
  }
  version
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
#' @param old Should the old default path (.cmdstanr) be used instead of the new
#'   one (.cmdstan)? Defaults to `FALSE` and may be removed in a future release.
#' @param wsl Return the directory for WSL installations?
#' @return The installation path.
#' @export
cmdstan_default_install_path <- function(old = FALSE, wsl = FALSE) {
  if (wsl) {
    file.path(paste0(wsl_dir_prefix(wsl = TRUE), wsl_home_dir()), ".cmdstan")
  } else {
    if (old) {
      file.path(.home_path(), ".cmdstanr")
    } else {
      file.path(.home_path(), ".cmdstan")
    }
  }
}

#' cmdstan_default_path
#'
#' Returns the path to the installation of CmdStan with the most recent release
#' version.
#'
#' For Windows systems with WSL CmdStan installs, if there are side-by-side WSL
#' and native installs with the same version then the WSL is preferred.
#' Otherwise, the most recent release is chosen, regardless of whether it is
#' native or WSL.
#'
#' @export
#' @keywords internal
#' @param old See [cmdstan_default_install_path()].
#' @param dir Path to a custom install folder with CmdStan installations.
#' @return Path to the CmdStan installation with the most recent release
#'   version, or `NULL` if no installation found.
#'
cmdstan_default_path <- function(old = FALSE, dir = NULL) {
  if (!is.null(dir)) {
    installs_path <- dir
  } else {
    installs_path <- cmdstan_default_install_path(old)
  }
  wsl_installed <- wsl_installed()
  if (!isTRUE(wsl_installed)) {
    wsl_installs_path <- NULL
    wsl_path_exists <- FALSE
  } else {
    wsl_installs_path <- cmdstan_default_install_path(old, wsl = TRUE)
    wsl_path_linux <- gsub(wsl_dir_prefix(wsl = TRUE), "", wsl_installs_path,
                          fixed=TRUE)
    wsl_path_exists <- isTRUE(.wsl_check_exists(wsl_path_linux))
  }
  if (dir.exists(installs_path) || wsl_path_exists) {
    latest_cmdstan <- ifelse(dir.exists(installs_path),
                             .latest_cmdstan_installed(installs_path), "")
    latest_wsl_cmdstan <- ifelse(wsl_path_exists,
                                 .latest_cmdstan_installed(wsl_installs_path), "")
    if (latest_wsl_cmdstan >= latest_cmdstan) {
      return(file.path(wsl_installs_path, latest_wsl_cmdstan))
    } else {
      return(file.path(installs_path, latest_cmdstan))
    }
  }
  NULL
}

.latest_cmdstan_installed <- function(installs_path) {
  cmdstan_installs <- list.dirs(path = installs_path, recursive = FALSE, full.names = FALSE)
  # if installed in cmdstan folder with no version move to cmdstan-version folder
  if ("cmdstan" %in% cmdstan_installs) {
    ver <- read_cmdstan_version(file.path(installs_path, "cmdstan"))
    old_path <- file.path(installs_path, "cmdstan")
    new_path <- file.path(installs_path, paste0("cmdstan-", ver))
    file.rename(old_path, new_path)
    cmdstan_installs <- list.dirs(path = installs_path, recursive = FALSE, full.names = FALSE)
  }
  latest_cmdstan <- ""
  if (length(cmdstan_installs) > 0) {
    cmdstan_installs <- grep("^cmdstan-", cmdstan_installs, value = TRUE)
    latest_cmdstan <- sort(cmdstan_installs, decreasing = TRUE)[1]
    if (is_release_candidate(latest_cmdstan)) {
      non_rc_path <- strsplit(latest_cmdstan, "-rc")[[1]][1]
      if (dir.exists(file.path(installs_path, non_rc_path))) {
        latest_cmdstan <- non_rc_path
      }
    }
  }
  latest_cmdstan
}


#' Find the version of CmdStan from makefile
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
  if (length(version_line) == 0) {
    stop("CmdStan makefile is missing a version number.", call. = FALSE)
  }
  sub("CMDSTAN_VERSION := ", "", version_line)
}

#' Returns whether the supplied installation is a release candidate
#' @noRd
#' @param path Path to installation.
#' @return TRUE if the installation in the supplied path is a release candidate
is_release_candidate <- function(path) {
  if (endsWith(path, "/")) {
    path <- substr(path, 1, nchar(path) - 1)
  }
  grepl(pattern = "-rc[0-9]*$", x = path)
}

# unset the path (only used in tests)
unset_cmdstan_path <- function() {
  .cmdstanr$PATH <- NULL
  .cmdstanr$VERSION <- NULL
}

# fake a cmdstan version (only used in tests)
fake_cmdstan_version <- function(version, mod) {
  .cmdstanr$VERSION <- version
  if(!is.null(mod)) {
    if (!is.null(mod$.__enclos_env__$private$exe_info_)) {
      mod$.__enclos_env__$private$exe_info_$stan_version <- version
    }
    if (!is.null(mod$.__enclos_env__$private$cmdstan_version_)) {
      mod$.__enclos_env__$private$cmdstan_version_ <- version
    }
  }
}
reset_cmdstan_version <- function() {
  .cmdstanr$VERSION <- read_cmdstan_version(cmdstan_path())
}

.home_path <- function() {
  home <- Sys.getenv("HOME")
  if (os_is_windows()) {
    userprofile <- Sys.getenv("USERPROFILE")
    h_drivepath <- file.path(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH"))
    win_home <- ifelse(userprofile == "", h_drivepath, userprofile)
    if (win_home != "") {
      home <- win_home
    }
  }
  home
}
