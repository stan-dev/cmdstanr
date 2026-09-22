#' Get or set the file path to the CmdStan installation
#'
#' @description Use the `set_cmdstan_path()` function to tell CmdStanR where the
#'   CmdStan installation is located. Once the path has been set,
#'   `cmdstan_path()` will return the full path to the CmdStan installation and
#'   `cmdstan_version()` will return the CmdStan version number. See **Details**
#'   for how to avoid manually setting the path in each \R session.
#'
#' @export
#'
#' @param path (string) The full file path to the CmdStan installation. If
#'   `NULL` (the default) then the path is set using the `"CMDSTAN"`
#'   environment variable when available, otherwise the default path used by
#'   [install_cmdstan()] if it exists.
#'
#' @return
#' `set_cmdstan_path()` invisibly returns the supplied or automatically resolved
#' path. If `path = NULL` and no installation is found, it invisibly returns
#' `NULL`.
#'
#' `cmdstan_path()` returns the current CmdStan path as a string or it errors if
#' no path has been set.
#'
#' `cmdstan_version()` returns the CmdStan version as a string. If CmdStan is not
#' found, it errors when `error_on_NA = TRUE` and returns `NULL` when
#' `error_on_NA = FALSE`.
#'
#' @details
#' Before the package can be used it needs to know where the CmdStan
#' installation is located. When the package is loaded it tries to help automate
#' this to avoid having to manually set the path every session:
#'
#' * If the [environment variable][Sys.setenv()] `"CMDSTAN"` points directly to
#' a valid CmdStan installation at load time, that path is used for the \R
#' session. If it instead points to an existing parent directory containing
#' versioned CmdStan installations, the installation with the largest version
#' number is used.
#' * If no environment variable is found when loaded but any directory in the
#' form `".cmdstan/cmdstan-[version]"` (e.g., `".cmdstan/cmdstan-2.35.0"`),
#' exists in the user's home directory (*not* the current working directory),
#' then the path to the CmdStan installation with the largest version number is
#' used for the \R session. On Windows the home directory is determined from
#' `USERPROFILE`, falling back to `HOMEDRIVE` and `HOMEPATH`. On other platforms
#' it is determined from `HOME`. This is the same default directory that
#' [install_cmdstan()] uses.
#'
#' It is always possible to change the path after loading the package using
#' `set_cmdstan_path(path)`.
#'
#' @seealso [install_cmdstan()], [cmdstan_default_install_path()], and
#'   [cmdstan_default_path()]
#'
set_cmdstan_path <- function(path = NULL) {
  if (is.null(path)) {
    env_path <- resolve_cmdstan_path_from_env()
    if (isTRUE(is.na(env_path))) {
      unset_cmdstan_path()
      return(invisible(NULL))
    }
    if (!is.null(env_path)) {
      path <- env_path
    } else {
      path <- cmdstan_default_path()
      if (is.null(path)) {
        return(invisible(NULL))
      }
    }
  }
  if (dir.exists(path)) {
    path <- absolute_path(path)
    version <- read_cmdstan_version(path)
    if (!is.null(version) && !is_supported_cmdstan_version(version)) {
      warning(
        "CmdStan path not set. CmdStan v", version, " is no longer supported. ",
        "CmdStanR now requires CmdStan v", cmdstan_min_version(), " or newer.",
        call. = FALSE
      )
      unset_cmdstan_path()
      return(invisible(path))
    }
    .cmdstanr$PATH <- path
    .cmdstanr$VERSION <- version
    .cmdstanr$WSL <- grepl("//wsl$", path, fixed = TRUE)
    message("CmdStan path set to: ", path)
  } else {
    warning("CmdStan path not set. Can't find directory: ", path, call. = FALSE)
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

#' The selected installation, checked right before a program runs out of it
#'
#' `cmdstan_path()` returns the path cached when it was set, so an
#' installation deleted since then would otherwise surface as a failure to
#' start make or stanc.
#'
#' @noRd
checked_cmdstan_path <- function() {
  path <- cmdstan_path()
  if (!dir.exists(path)) {
    stop(
      "The CmdStan installation at '", path, "' no longer exists. ",
      "Use set_cmdstan_path() to select another installation or ",
      "install_cmdstan() to reinstall it.",
      call. = FALSE
    )
  }
  path
}

#' The selected installation's version as its makefile says now, since the
#' version `cmdstan_version()` caches goes stale when a checkout is rebuilt
#' in place. A missing installation keeps the cached version.
#' @noRd
current_cmdstan_version <- function() {
  path <- cmdstan_path()
  if (!dir.exists(path)) {
    return(cmdstan_version())
  }
  read_cmdstan_version(path)
}

#' @rdname set_cmdstan_path
#' @export
#' @param error_on_NA (logical) Should an error be thrown if CmdStan is not
#'   found. The default is `TRUE`. If `FALSE`, `cmdstan_version()` returns
#'   `NULL`.
cmdstan_version <- function(error_on_NA = TRUE) {
  version <- .cmdstanr$VERSION
  if (is.null(version) && error_on_NA) {
    stop_no_path()
  }
  version
}


# internal ----------------------------------------------------------------

# initialize internal environment to store path to cmdstan, cmdstan version
# number, path to temp dir, etc.
.cmdstanr <- new.env(parent = emptyenv())
.cmdstanr$PATH <- NULL
.cmdstanr$VERSION <- NULL
.cmdstanr$TEMP_DIR <- NULL
.cmdstanr$WSL <- FALSE
.cmdstanr$TOOLCHAIN_PATH <- NULL

# Create a private null pointer for detecting when the model pointer used by
# compiled model methods loses its native address during serialization. The
# round trip avoids using methods::new("externalptr")'s shared prototype.
new_null_external_pointer <- function() {
  unserialize(serialize(methods::new("externalptr"), NULL))
}
.cmdstanr$NULL_EXTERNAL_POINTER <- new_null_external_pointer()


unset_cmdstan_path <- function() {
  .cmdstanr$PATH <- NULL
  .cmdstanr$VERSION <- NULL
  .cmdstanr$WSL <- FALSE
}

cmdstan_tempdir <- function() {
  .cmdstanr$TEMP_DIR
}

stop_no_path <- function() {
  stop("CmdStan path has not been set yet. See ?set_cmdstan_path.",
       call. = FALSE)
}

cmdstan_min_version <- function() {
  "2.35.0"
}

# Normalize versions for comparison. This is intentionally looser than
# cmdstan_version_from_path(): it accepts version strings, paths, and install
# dir names, and strips release-candidate suffixes.
cmdstan_version_for_comparison <- function(version) {
  version <- as.character(version)
  version <- sub("[/\\\\]+$", "", version)
  version <- basename(version)
  version <- sub("^cmdstan-", "", version)
  sub("-rc[0-9]+$", "", version)
}

# Scalar comparison of versions numbers. Returns -1, 0, or 1.
# Empty strings are used when no native or WSL install was found during path discovery.
cmdstan_version_compare <- function(version, other) {
  if (length(version) != 1 || is.na(version) || !nzchar(version)) {
    return(-1L)
  }
  if (length(other) != 1 || is.na(other) || !nzchar(other)) {
    return(1L)
  }
  utils::compareVersion(
    cmdstan_version_for_comparison(version),
    cmdstan_version_for_comparison(other)
  )
}

is_supported_cmdstan_version <- function(version) {
  cmp <- tryCatch(
    suppressWarnings(cmdstan_version_compare(version, cmdstan_min_version())),
    error = function(e) NA_integer_
  )
  isTRUE(cmp >= 0)
}

resolve_cmdstan_path_from_env <- function() {
  path <- Sys.getenv("CMDSTAN")
  if (!nzchar(path)) {
    return(NULL)
  }
  if (!dir.exists(path)) {
    warning(
      "CmdStan path not set. Can't find directory specified by environment ",
      "variable 'CMDSTAN'.",
      call. = FALSE
    )
    return(NA_character_)
  }
  path <- absolute_path(path)
  version <- suppressWarnings(read_cmdstan_version(path))
  if (!is.null(version)) {
    return(path)
  }
  path <- cmdstan_default_path(dir = path)
  if (is.null(path)) {
    warning(
      "CmdStan path not set. No CmdStan installation found in the path ",
      "specified by the environment variable 'CMDSTAN'.",
      call. = FALSE
    )
    return(NA_character_)
  }
  path
}

#' Path to where  `install_cmdstan()` with default settings installs CmdStan.
#'
#' @keywords internal
#' @param wsl Return the directory for WSL installations?
#' @return The installation path.
#' @export
#' @seealso [install_cmdstan()], [set_cmdstan_path()], and
#'   [cmdstan_default_path()]
cmdstan_default_install_path <- function(wsl = FALSE) {
  if (wsl) {
    file.path(paste0(wsl_dir_prefix(wsl = TRUE), wsl_home_dir()), ".cmdstan")
  } else {
    file.path(home_path(), ".cmdstan")
  }
}

home_path <- function() {
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

#' Path to the installation of CmdStan with the most recent release version
#'
#' For Windows systems with WSL CmdStan installs, if there are side-by-side WSL
#' and native installs with the same version then the WSL is preferred.
#' Otherwise, the most recent release is chosen, regardless of whether it is
#' native or WSL.
#'
#' @export
#' @keywords internal
#' @param dir Path to a custom install folder with CmdStan installations.
#' @return Path to the CmdStan installation with the most recent release
#'   version, or `NULL` if no installation found.
#'
#' @seealso [install_cmdstan()], [set_cmdstan_path()], and
#'   [cmdstan_default_install_path()]
#'
cmdstan_default_path <- function(dir = NULL) {
  if (!is.null(dir)) {
    installs_path <- dir
    wsl_installs_path <- NULL
    wsl_path_exists <- FALSE
  } else {
    installs_path <- cmdstan_default_install_path()
    wsl_installed <- wsl_installed()
    if (!isTRUE(wsl_installed)) {
      wsl_installs_path <- NULL
      wsl_path_exists <- FALSE
    } else {
      wsl_installs_path <- cmdstan_default_install_path(wsl = TRUE)
      wsl_path_linux <- gsub(wsl_dir_prefix(wsl = TRUE), "", wsl_installs_path,
                            fixed=TRUE)
      wsl_path_exists <- isTRUE(.wsl_check_exists(wsl_path_linux))
    }
  }
  if (dir.exists(installs_path) || wsl_path_exists) {
    latest_cmdstan <- ifelse(dir.exists(installs_path),
                             latest_cmdstan_installed(installs_path), "")
    latest_wsl_cmdstan <- ifelse(wsl_path_exists,
                                 latest_cmdstan_installed(wsl_installs_path), "")
    if (!nzchar(latest_cmdstan) && !nzchar(latest_wsl_cmdstan)) {
      return(NULL)
    }
    if (cmdstan_version_compare(latest_wsl_cmdstan, latest_cmdstan) >= 0) {
      return(file.path(wsl_installs_path, latest_wsl_cmdstan))
    } else {
      return(file.path(installs_path, latest_cmdstan))
    }
  }
  NULL
}

# Return the newest CmdStan install directory name under an install root
latest_cmdstan_installed <- function(installs_path) {
  cmdstan_installs <- list.dirs(path = installs_path, recursive = FALSE, full.names = FALSE)
  latest_cmdstan <- ""
  if (length(cmdstan_installs) > 0) {
    cmdstan_installs <- grep(
      "^cmdstan-[0-9]+\\.[0-9]+\\.[0-9]+(-rc[0-9]+)?$",
      cmdstan_installs,
      value = TRUE
    )
    if (length(cmdstan_installs) == 0) {
      return(latest_cmdstan)
    }
    # list.dirs() returns dir names, so sort by parsed versions instead of
    # lexicographic names
    latest_cmdstan <- cmdstan_installs[order(
      numeric_version(cmdstan_version_for_comparison(cmdstan_installs)),
      cmdstan_installs,
      decreasing = TRUE
    )[1]]
    if (is_release_candidate(latest_cmdstan)) {
      non_rc_path <- strsplit(latest_cmdstan, "-rc")[[1]][1]
      if (dir.exists(file.path(installs_path, non_rc_path))) {
        latest_cmdstan <- non_rc_path
      }
    }
  }
  latest_cmdstan
}

# Detect WSL UNC paths, which may need path-based version fallback below
is_wsl_unc_path <- function(path) {
  is.character(path) &&
    length(path) == 1 &&
    !is.na(path) &&
    startsWith(repair_path(path), "//wsl$/")
}

# Strict extractor for CmdStan install paths. Unlike
# cmdstan_version_for_comparison(), it returns NULL for non-CmdStan paths and
# preserves release-candidate suffixes.
cmdstan_version_from_path <- function(path) {
  path <- repair_path(path)
  match <- regmatches(
    path,
    regexpr("cmdstan-[0-9]+\\.[0-9]+\\.[0-9]+(?:-rc[0-9]+)?$", path)
  )
  if (!length(match) || is.na(match) || !nzchar(match)) {
    return(NULL)
  }
  sub("^cmdstan-", "", match)
}


#' Find the version of CmdStan from makefile
#' @noRd
#' @param path Path to installation.
#' @return Version number as a string.
read_cmdstan_version <- function(path) {
  makefile_path <- file.path(path, "makefile")
  if (is_wsl_unc_path(path)) {
    makefile <- tryCatch(
      suppressWarnings(readLines(makefile_path, warn = FALSE)),
      error = function(e) NULL
    )
  } else if (file.exists(makefile_path)) {
    makefile <- readLines(makefile_path)
  } else {
    makefile <- NULL
  }
  if (is.null(makefile)) {
    if (is_wsl_unc_path(path)) {
      version_from_path <- cmdstan_version_from_path(path)
      if (!is.null(version_from_path)) {
        return(version_from_path)
      }
    }
    warning(
      "Can't find CmdStan makefile to detect version number. ",
      "Path may not point to valid installation.",
      call. = FALSE
    )
    return(NULL)
  }
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

tbb_path <- function(dir = NULL) {
  path_to_TBB <- NULL
  if (os_is_windows()) {
    if (is.null(dir)) {
      dir <- cmdstan_path()
    }
    path_to_TBB <- file.path(dir, "stan", "lib", "stan_math", "lib", "tbb")
  }
  path_to_TBB
}

#' The TBB directory to put on PATH when launching a model executable
#'
#' On Windows there's no rpath, so the executable looks for tbb.dll on PATH and
#' we need to put a TBB directory there. We use the one from the build record,
#' i.e., the TBB the model was actually built against, even if the user has
#' selected a different CmdStan installation since then. If that directory no
#' longer exists we don't add anything, in which case the executable either uses
#' whatever TBB is already on PATH or fails to load if there isn't one. If
#' there's no usable build record we fall back to the selected installation's
#' TBB, which is what we always did before.
#'
#' @param tbb_dir The record's `tbb_dir`, or `NULL` when there is no usable
#'   record.
#' @return The directory, or `NULL` when nothing should go on PATH.
#' @noRd
tbb_launch_path <- function(tbb_dir) {
  if (!os_is_windows()) {
    return(NULL)
  }
  if (is.null(tbb_dir)) {
    return(tbb_path())
  }
  if (dir.exists(tbb_dir)) tbb_dir else NULL
}
