#' Install CmdStan or clean and rebuild an existing installation
#'
#' @description The `install_cmdstan()` function attempts to download and
#'   install the latest release of [CmdStan](https://github.com/stan-dev/cmdstan/releases/latest).
#'   Installing a previous release or a new release candidate is also possible
#'   by specifying the `version` or `release_url` argument.
#'   See the first few sections of the CmdStan
#'   [installation guide](https://mc-stan.org/docs/cmdstan-guide/cmdstan-installation.html)
#'   for details on the C++ toolchain required for installing CmdStan.
#'
#'   The `rebuild_cmdstan()` function cleans and rebuilds the CmdStan
#'   installation. Use this function in case of any issues when compiling models.
#'
#'   The `cmdstan_make_local()` function is used to read/write makefile flags
#'   and variables from/to the `make/local` file of a CmdStan installation.
#'   Writing to the `make/local` file can be used to permanently add makefile
#'   flags/variables to an installation. For example adding specific compiler
#'   switches, changing the C++ compiler, etc. A change to the `make/local` file
#'   should typically be followed by calling `rebuild_cmdstan()`.
#'
#'   The `check_cmdstan_toolchain()` function attempts to check for the required
#'   C++ toolchain. It is called internally by `install_cmdstan()` but can also
#'   be called directly by the user. On Windows only, calling the function with
#'   the `fix = TRUE` argument will attempt to install the necessary toolchain
#'   components if they are not found. For Windows users with RTools and CmdStan
#'   versions >= 2.35 no additional toolchain configuration is required.
#'
#'   NOTE: When installing CmdStan on Windows with RTools and CmdStan versions
#'   prior to 2.35.0, the above additional toolchain configuration
#'   is still required. To enable this configuration, set the environment variable
#'   `CMDSTANR_USE_MSYS_TOOLCHAIN` to 'true' and call
#'   `check_cmdstan_toolchain(fix = TRUE)`.
#'
#' @export
#' @param dir (string) The path to the directory in which to install CmdStan.
#'   The default is to install it in a directory called `.cmdstan` within the
#'   user's home directory (i.e, `file.path(Sys.getenv("HOME"), ".cmdstan")`).
#' @param cores (integer) The number of CPU cores to use to parallelize building
#'   CmdStan and speed up installation. If `cores` is not specified then the
#'   default is to look for the option `"mc.cores"`, which can be set for an
#'   entire \R session by `options(mc.cores=value)`. If the `"mc.cores"` option
#'   has not been set then the default is `2`.
#' @param quiet (logical) For `install_cmdstan()`, should the verbose output
#'   from the system processes be suppressed when building the CmdStan binaries?
#'   The default is `FALSE`. For `check_cmdstan_toolchain()`, should the
#'   function suppress printing informational messages? The default is `FALSE`.
#'   If `TRUE` only errors will be printed.
#' @param overwrite (logical) Should CmdStan still be downloaded and installed
#'   even if an installation of the same version is found in `dir`? The default
#'   is `FALSE`, in which case an informative error is thrown instead of
#'   overwriting the user's installation.
#' @param timeout (positive real) Timeout (in seconds) for the build stage of
#'   the installation.
#' @param version (string) The CmdStan release version to install. The default
#'   is `NULL`, which downloads the latest stable release from
#'   <https://github.com/stan-dev/cmdstan/releases>.
#' @param release_url (string) The URL for the specific CmdStan release or
#'   release candidate to install. See <https://github.com/stan-dev/cmdstan/releases>.
#'   The URL should point to the tarball (`.tar.gz.` file) itself, e.g.,
#'   `release_url="https://github.com/stan-dev/cmdstan/releases/download/v2.25.0/cmdstan-2.25.0.tar.gz"`.
#'   If both `version` and `release_url` are specified then `version` will be used.
#' @param release_file (string) A file path to a CmdStan release tar.gz file
#'   downloaded from the releases page: <https://github.com/stan-dev/cmdstan/releases>.
#'   For example: `release_file=""./cmdstan-2.33.1.tar.gz"`. If `release_file` is
#'   specified then both `release_url` and `version` will be ignored.
#' @param cpp_options (list) Any makefile flags/variables to be written to
#'   the `make/local` file. For example, `list("CXX" = "clang++")` will force
#'   the use of clang for compilation.
#' @param check_toolchain (logical) Should `install_cmdstan()` attempt to check
#'   that the required toolchain is installed and properly configured. The
#'   default is `TRUE`.
#' @param wsl (logical) Should CmdStan be installed and run through the Windows
#'  Subsystem for Linux (WSL). The default is `FALSE`.
#'
#' @examples
#' \dontrun{
#' check_cmdstan_toolchain()
#'
#' # install_cmdstan(cores = 4)
#'
#' cpp_options <- list(
#'   "CXX" = "clang++",
#'   "CXXFLAGS+= -march=native",
#'   PRECOMPILED_HEADERS = TRUE
#' )
#' # cmdstan_make_local(cpp_options = cpp_options)
#' # rebuild_cmdstan()
#' }
#'
install_cmdstan <- function(dir = NULL,
                            cores = getOption("mc.cores", 2),
                            quiet = FALSE,
                            overwrite = FALSE,
                            timeout = 1200,
                            version = NULL,
                            release_url = NULL,
                            release_file = NULL,
                            cpp_options = list(),
                            check_toolchain = TRUE,
                            wsl = FALSE) {
  # Use environment variable to record WSL usage throughout install,
  # post-installation will simply check for 'wsl-' prefix in cmdstan path
  if (isTRUE(wsl)) {
    if (!os_is_windows()) {
      warning("wsl=TRUE is only available on Windows, and will be ignored!",
              call. = FALSE)
      wsl <- FALSE
    } else {
      .cmdstanr$WSL <- TRUE
    }
  } else {
    .cmdstanr$WSL <- FALSE
  }
  if (os_is_windows() && !os_is_wsl() && isTRUE(version < "2.35.0")) {
    # RTools can be used unmodified with CmdStan 2.35+
    # For new installs of older versions, users need to install mingw32-make and MSYS gcc
    if (Sys.getenv("CMDSTANR_USE_MSYS_TOOLCHAIN") == "") {
      stop("CmdStan versions prior to 2.35.0 require additional toolchain configuration on Windows.\n",
            "Please set the environment variable CMDSTANR_USE_MSYS_TOOLCHAIN to 'true' and \n",
            "call `check_cmdstan_toolchain(fix = TRUE)` before installing CmdStan.", call. = FALSE)
    }
  }
  if (check_toolchain) {
    check_cmdstan_toolchain(fix = FALSE, quiet = quiet)
  }
  make_local_msg <- NULL
  if (!is.null(cmdstan_version(error_on_NA = FALSE))) {
    current_make_local_contents <- cmdstan_make_local()
    if (length(current_make_local_contents) > 0) {
      old_cmdstan_path <- cmdstan_path()
      make_local_msg <- paste0("cmdstan_make_local(cpp_options = cmdstan_make_local(dir = \"", cmdstan_path(), "\"))")
    }
  }
  if (is.null(dir)) {
    dir <- cmdstan_default_install_path(wsl = wsl)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  } else {
    dir <- repair_path(dir)
    assert_dir_exists(dir, access = "rwx")
  }
  if (!is.null(release_file)) {
    if (!is.null(release_url) || !is.null(version)) {
      warning("release_file and release_url/version shouldn't both be specified!",
              "\nrelease_url/version will be ignored.", call. = FALSE)
    }

    release_url <- release_file
  }
  if (!is.null(version) && is.null(release_file)) {
    if (!is.null(release_url)) {
      warning("version and release_url shouldn't both be specified!",
              "\nrelease_url will be ignored.", call. = FALSE)
    }

    release_url <- paste0("https://github.com/stan-dev/cmdstan/releases/download/v",
                          version, "/cmdstan-", version, cmdstan_arch_suffix(version), ".tar.gz")
  }
  if (!is.null(release_url)) {
    if (!endsWith(release_url, ".tar.gz")) {
      stop(release_url, " is not a .tar.gz archive!",
           "cmdstanr supports installing from .tar.gz archives only.", call. = FALSE)
    }
    message("* Installing CmdStan from ", release_url)
    download_url <- release_url
    split_url <- strsplit(release_url, "/")
    tar_name <- utils::tail(split_url[[1]], n = 1)
    tar_name <- gsub("-linux-(.*).tar.gz", ".tar.gz", tar_name)
    cmdstan_ver <- substr(tar_name, 0, nchar(tar_name) - 7)
    tar_gz_file <- paste0(cmdstan_ver, ".tar.gz")
    dir_cmdstan <- file.path(dir, cmdstan_ver)
    dest_file <- file.path(dir, tar_gz_file)
  } else {
    ver <- latest_released_version(quiet = quiet)
    message("* Latest CmdStan release is v", ver)
    cmdstan_ver <- paste0("cmdstan-", ver)
    tar_gz_file <- paste0(cmdstan_ver, cmdstan_arch_suffix(ver), ".tar.gz")
    dir_cmdstan <- file.path(dir, cmdstan_ver)
    message("* Installing CmdStan v", ver, " in ", dir_cmdstan)
    message("* Downloading ", tar_gz_file, " from GitHub...")
    download_url <- github_download_url(ver)
    dest_file <- file.path(dir, tar_gz_file)
  }
  if (!check_install_dir(dir_cmdstan, overwrite)) {
    return(invisible(NULL))
  }
  if (is.null(release_file)) {
    tar_downloaded <- download_with_retries(download_url, dest_file, quiet = quiet)
    if (inherits(tar_downloaded, "try-error")) {
      error_msg <- paste("Download of CmdStan failed with error:",
                          attr(tar_downloaded, "condition")$message)
      if (!is.null(version)) {
        error_msg <- paste0(error_msg, "\nPlease check if the supplied version number is valid.")
      } else if (!is.null(release_url)) {
        error_msg <- paste0(error_msg, "\nPlease check if the supplied release URL is valid.")
      }
      stop(error_msg, call. = FALSE)
    }
    message("* Download complete")
  } else {
    file.copy(release_file, dest_file)
  }
  message("* Unpacking archive...")
  if (wsl) {
    # Significantly faster to use WSL to untar the downloaded archive, as there are
    # similar IO issues accessing the WSL filesystem from windows
    wsl_tar_gz_file <- gsub(paste0("//wsl$/", wsl_distro_name()), "",
                            dest_file, fixed = TRUE)
    wsl_tar_gz_file <- wsl_safe_path(wsl_tar_gz_file)
    untar_rc <- processx::run(
      command = "wsl",
      args = c("tar", "-xf", wsl_tar_gz_file, "-C",
               gsub(tar_gz_file, "", wsl_tar_gz_file))
    )
    remove_rc <- processx::run(
      command = "wsl",
      args = c("rm", wsl_tar_gz_file)
    )
  } else {
    untar_rc <- utils::untar(
      dest_file,
      exdir = dir
    )
    if (untar_rc != 0) {
      stop("Problem extracting tarball. Exited with return code: ", untar_rc, call. = FALSE)
    }
    file.remove(dest_file)
  }

  cmdstan_make_local(dir = dir_cmdstan, cpp_options = cpp_options, append = TRUE)
  # Setting up native M1 compilation of CmdStan and its downstream libraries
  if (is_rosetta2()) {
    cmdstan_make_local(
      dir = dir_cmdstan,
      cpp_options = list(
        CXX = "arch -arch arm64e clang++"
      ),
      append = TRUE
    )
  }

  # Building fails on Apple silicon with < v2.31 due to a makefiles setting
  # for stanc3, so manually implement the patch if needed from:
  # https://github.com/stan-dev/cmdstan/pull/1127
  stanc_makefile <- readLines(file.path(dir_cmdstan, "make", "stanc"))
  stanc_makefile <- gsub("\\bxattr -d com.apple.quarantine bin/stanc",
                          "-xattr -d com.apple.quarantine bin/stanc",
                          stanc_makefile)
  writeLines(stanc_makefile, con = file.path(dir_cmdstan, "make", "stanc"))

  if (is_ucrt_toolchain() && !wsl) {
    cmdstan_make_local(
      dir = dir_cmdstan,
      cpp_options = list(
        "CXXFLAGS += -Wno-nonnull -D_UCRT",
        "TBB_CXXFLAGS= -D_UCRT"
      ),
      append = TRUE
    )
  }

  # Suppress noisy warnings from Boost
  cmdstan_make_local(
    dir = dir_cmdstan,
    cpp_options = list(
      "CXXFLAGS += -Wno-deprecated-declarations"
    ),
    append = TRUE
  )

  message("* Building CmdStan binaries...")
  build_log <- build_cmdstan(dir_cmdstan, cores, quiet, timeout)
  if (!build_status_ok(build_log, quiet = quiet)) {
    return(invisible(build_log))
  }

  example_log <- build_example(dir_cmdstan, cores, quiet, timeout)
  if (!build_status_ok(example_log, quiet = quiet)) {
    return(invisible(example_log))
  }

  message("* Finished installing CmdStan to ", dir_cmdstan, "\n")
  set_cmdstan_path(dir_cmdstan)
  if (!is.null(make_local_msg) && old_cmdstan_path != cmdstan_path()) {
    message(
      "\nThe previous installation of CmdStan had a non-empty make/local file.\n",
      "If you wish to copy the file to the new installation, run the following commands:\n",
      "\n",
      make_local_msg,
      "\nrebuild_cmdstan(cores = ...)"
    )
  }
  if (isTRUE(wsl)) {
    Sys.unsetenv("CMDSTANR_USE_WSL")
  }
}


#' @rdname install_cmdstan
#' @export
rebuild_cmdstan <- function(dir = cmdstan_path(),
                            cores = getOption("mc.cores", 2),
                            quiet = FALSE,
                            timeout = 600) {
  clean_cmdstan(dir, cores, quiet)
  build_cmdstan(dir, cores, quiet, timeout)
  invisible(NULL)
}

#' @rdname install_cmdstan
#' @export
#' @param append (logical) For `cmdstan_make_local()`, should the listed
#'   makefile flags be appended to the end of the existing `make/local` file?
#'   The default is `TRUE`. If `FALSE` the file is overwritten.
#' @return For `cmdstan_make_local()`, if `cpp_options=NULL` then the existing
#'   contents of `make/local` are returned without writing anything, otherwise
#'   the updated contents are returned.
#'
cmdstan_make_local <- function(dir = cmdstan_path(),
                               cpp_options = NULL,
                               append = TRUE) {
  make_local_path <- file.path(dir, "make", "local")
  if (!is.null(cpp_options)) {
    built_flags <- c()
    for (i in seq_len(length(cpp_options))) {
      option_name <- names(cpp_options)[i]
      if (isTRUE(as.logical(cpp_options[[i]]))) {
        built_flags <- c(built_flags, paste0(toupper(option_name), "=true"))
      } else if (isFALSE(as.logical(cpp_options[[i]]))) {
        built_flags <- c(built_flags, paste0(toupper(option_name), "=false"))
      } else {
        if (is.null(option_name) || !nzchar(option_name)) {
          built_flags <- c(built_flags, paste0(cpp_options[[i]]))
        } else {
          built_flags <- c(built_flags, paste0(toupper(option_name), "=", cpp_options[[i]]))
        }
      }
    }
    write(built_flags, file = make_local_path, append = append)
  }
  if (file.exists(make_local_path)) {
    return(trimws(strsplit(trimws(readChar(make_local_path, file.info(make_local_path)$size)), "\n")[[1]]))
  } else {
    return(NULL)
  }
}

#' @rdname install_cmdstan
#' @export
#' @param fix For `check_cmdstan_toolchain()`, should CmdStanR attempt to fix
#'   any detected toolchain problems? Currently this option is only available on
#'   Windows. The default is `FALSE`, in which case problems are only reported
#'   along with suggested fixes.
#'
check_cmdstan_toolchain <- function(fix = FALSE, quiet = FALSE) {
  if (os_is_windows()) {
    if (os_is_wsl()) {
      check_wsl_toolchain()
    } else if (R.version$major >= "4") {
      check_rtools4x_windows_toolchain(fix = fix, quiet = quiet)
    } else {
      check_rtools35_windows_toolchain(fix = fix, quiet = quiet)
    }
  } else {
    check_unix_make()
    check_unix_cpp_compiler()
  }
  if (!checkmate::test_directory(dirname(tempdir()), access = "w")) {
    stop("No write permissions to the temporary folder! Please change the permissions or location of the temporary folder.", call. = FALSE)
  }
  if (!quiet) {
    message("The C++ toolchain required for CmdStan is setup properly!")
  }
  invisible(NULL)
}


# internal ----------------------------------------------------------------

check_install_dir <- function(dir_cmdstan, overwrite = FALSE) {
  if (dir.exists(dir_cmdstan)) {
    if (!overwrite) {
      warning(
        "An installation already exists at ", dir_cmdstan, ". ",
        "Please remove or rename the installation folder or set overwrite=TRUE.",
        call. = FALSE
      )
      return(FALSE)
    } else {
      message("* Removing the existing installation of CmdStan...")
      unlink(dir_cmdstan, recursive = TRUE, force = TRUE)
    }
  }
  TRUE
}

github_auth_token <- function() {
  github_pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(github_pat)) {
    auth_token <- c(Authorization = paste0("token ", github_pat))
  } else {
    auth_token <- NULL
  }
  auth_token
}

# construct url for download from cmdstan version number
github_download_url <- function(version_number) {

  base_url <- "https://github.com/stan-dev/cmdstan/releases/download/"
  paste0(base_url, "v", version_number,
         "/cmdstan-", version_number, cmdstan_arch_suffix(), ".tar.gz")
}

# get version number of latest release
latest_released_version <- function(quiet=TRUE, ...) {
  dest_file <- tempfile(pattern = "releases-", fileext = ".json")
  download_url <- "https://api.github.com/repos/stan-dev/cmdstan/releases/latest"
  release_list_downloaded <- download_with_retries(download_url, dest_file, quiet = quiet, ...)
  if (inherits(release_list_downloaded, "try-error")) {
    stop("GitHub download of release list failed with error: ",
        attr(release_list_downloaded, "condition")$message,
        call. = FALSE)
  }
  release <- jsonlite::read_json(dest_file)
  sub("v", "", release$tag_name)
}

try_download <- function(download_url, destination_file,
                          quiet = TRUE) {
  download_status <- try(
    suppressWarnings(
      utils::download.file(url = download_url,
                           destfile = destination_file,
                           quiet = quiet,
                           headers = github_auth_token())
    ),
    silent = TRUE
  )
  download_status
}

# download with retries and pauses
download_with_retries <- function(download_url,
                                  destination_file,
                                  retries = 5,
                                  pause_sec = 5,
                                  quiet = TRUE) {
    download_rc <- try_download(download_url, destination_file,
                                quiet = quiet)
    num_retries <- 0
    while (num_retries < retries && inherits(download_rc, "try-error")) {
      Sys.sleep(pause_sec)
      num_retries <- num_retries + 1
      download_rc <- try_download(download_url, destination_file, quiet = quiet)
    }
    download_rc
}

build_cmdstan <- function(dir,
                          cores = getOption("mc.cores", 2),
                          quiet = FALSE,
                          timeout) {
  translation_args <- NULL
  if (is_rosetta2()) {
    run_cmd <- "/usr/bin/arch"
    translation_args <- c("-arch", "arm64e", "make")
  } else {
    run_cmd <- make_cmd()
  }
  withr::with_envvar(
    c("HOME" = short_path(Sys.getenv("HOME"))),
    withr::with_path(
      c(
        toolchain_PATH_env_var(),
        tbb_path(dir = dir)
      ),
      wsl_compatible_run(
        command = run_cmd,
        args = c(translation_args, paste0("-j", cores), "build"),
        wd = dir,
        echo_cmd = is_verbose_mode(),
        echo = !quiet || is_verbose_mode(),
        spinner = quiet,
        error_on_status = FALSE,
        stderr_callback = function(x, p) { if (quiet) message(x) },
        timeout = timeout
      )
    )
  )
}

clean_cmdstan <- function(dir = cmdstan_path(),
                          cores = getOption("mc.cores", 2),
                          quiet = FALSE) {
  withr::with_envvar(
    c("HOME" = short_path(Sys.getenv("HOME"))),
    withr::with_path(
      c(
        toolchain_PATH_env_var(),
        tbb_path(dir = dir)
      ),
      wsl_compatible_run(
        command = make_cmd(),
        args = "clean-all",
        wd = dir,
        echo_cmd = is_verbose_mode(),
        echo = !quiet || is_verbose_mode(),
        spinner = quiet,
        error_on_status = FALSE,
        stderr_callback = function(x, p) { if (quiet) message(x) }
      )
    )
  )
}

build_example <- function(dir, cores, quiet, timeout) {
  withr::with_envvar(
    c("HOME" = short_path(Sys.getenv("HOME"))),
    withr::with_path(
      c(
        toolchain_PATH_env_var(),
        tbb_path(dir = dir)
      ),
      wsl_compatible_run(
        command = make_cmd(),
        args = c(paste0("-j", cores),
                  cmdstan_ext(file.path("examples", "bernoulli", "bernoulli"))),
        wd = dir,
        echo_cmd = is_verbose_mode(),
        echo = !quiet || is_verbose_mode(),
        spinner = quiet,
        error_on_status = FALSE,
        stderr_callback = function(x, p) { if (quiet) message(x) },
        timeout = timeout
      )
    )
  )
}

build_status_ok <- function(process_log, quiet = FALSE) {
  if (process_log$timeout) {
    if (quiet) {
      end_warning <-
        " and running again with 'quiet=FALSE' to see full installation output."
    } else {
      end_warning <- "."
    }
    warning(
      "The build process timed out. ",
      "Try increasing the value of the 'timeout' argument",
      end_warning,
      call. = FALSE
    )
    return(FALSE)
  }

  if (is.na(process_log$status) || process_log$status != 0) {
    if (quiet) {
      end_warning <-
        " and/or try again with 'quiet=FALSE' to see full installation output."
    } else {
      end_warning <- "."
    }
    cat("\n")
    warning(
      "There was a problem during installation. See the error message(s) above",
      end_warning,
      call. = FALSE
    )
    return(FALSE)
  }

  TRUE
}

install_toolchain <- function(quiet = FALSE) {
  rtools_usr_bin <- file.path(rtools4x_home_path(), "usr", "bin")
  rtools_version <- paste0("Rtools", rtools4x_version())
  if (is_ucrt_toolchain()) {
    install_pkgs <- c("mingw-w64-ucrt-x86_64-make", "mingw-w64-ucrt-x86_64-gcc")
    if (!quiet) message(paste0("Installing mingw32-make and g++ with ", rtools_version))
  } else {
    install_pkgs <- "mingw-w64-x86_64-make"
    if (!quiet) message(paste0("Installing mingw32-make with ", rtools_version))
  }
  if (!checkmate::test_directory(rtools_usr_bin, access = "w")) {
    warning("No write permissions in the RTools folder. This might prevent installing the toolchain.",
            " Consider changing permissions or reinstalling RTools in a different folder.", call. = FALSE)
  }
  withr::with_path(
    c(
      toolchain_PATH_env_var()
    ),
    processx::run(
      "pacman",
      args = c("-Sy", install_pkgs, "--noconfirm"),
      wd = rtools_usr_bin,
      error_on_status = TRUE,
      echo_cmd = is_verbose_mode(),
      echo = is_verbose_mode()
    )
  )
  invisible(NULL)
}

check_wsl_toolchain <- function() {
  if (!wsl_installed()) {
    stop("\n", "A WSL distribution is not installed or is not accessible.",
         "\n", "Please see the Microsoft documentation for guidance on installing WSL: ",
         "\n", "https://docs.microsoft.com/en-us/windows/wsl/install",
         call. = FALSE)
  }

  make_not_present <- processx::run(command = "wsl",
                                    args = c("which", "make"),
                                    error_on_status = FALSE)

  gpp_not_present <- processx::run(command = "wsl",
                                    args = c("which", "g++"),
                                    error_on_status = FALSE)

  clangpp_not_present <- processx::run(command = "wsl",
                                   args = c("which", "clang++"),
                                   windows_verbatim_args = TRUE,
                                   error_on_status = FALSE)

  if (make_not_present$status || (gpp_not_present$status
        && clangpp_not_present$status)) {
    stop("\n", "Your distribution is missing the needed utilities for compiling C++.",
         "\n", "Please launch your WSL and install them using the appropriate command:",
         "\n", "Debian/Ubuntu: sudo apt-get install build-essential",
         "\n", "Fedora: sudo dnf group install \"C Development Tools and Libraries\"",
         "\n", "Arch: pacman -Sy base-devel",
         call. = FALSE)
  }
}

check_rtools4x_windows_toolchain <- function(fix = FALSE, quiet = FALSE) {
  rtools_path <- rtools_home_path()
  rtools_version <- paste0("Rtools", rtools4x_version())
  toolchain_path <- rtools4x_toolchain_path()
  # If RTOOLS4X_HOME is not set (the env. variable gets set on install)
  # we assume that RTools 40 is not installed.
  if (!nzchar(rtools_path)) {
    stop(
      "\n", rtools_version, " was not found but is required to run CmdStan with R version ",
      R.version$major, ".", R.version$minor, ".",
      "\nPlease install ", rtools_version, " and run cmdstanr::check_cmdstan_toolchain().",
      call. = FALSE
    )
  }
  # If RTools is installed in a path with spaces or brackets
  # we error as this path is not valid
  if (grepl("\\(|)| ", rtools_path)) {
    stop(
      "\n", rtools_version, " is installed in a path with spaces or brackets, which is not supported.",
      "\nPlease reinstall ", rtools_version, " to a valid path, restart R, and then run cmdstanr::check_cmdstan_toolchain().",
      call. = FALSE
    )
  }
  # No additional utilities/toolchains are needed with RTools4
  if (Sys.getenv("CMDSTANR_USE_MSYS_TOOLCHAIN") == "") {
    return(invisible(NULL))
  }
  if (!is_toolchain_installed(app = "g++", path = toolchain_path) ||
      !is_toolchain_installed(app = "mingw32-make", path = toolchain_path)) {
    if (!fix) {
      stop(
        "\n", rtools_version, " installation found but the toolchain was not installed.",
        "\nRun cmdstanr::check_cmdstan_toolchain(fix = TRUE) to fix the issue.",
        call. = FALSE
      )
    } else {
      install_toolchain(quiet = quiet)
      if (!is_toolchain_installed(app = "g++", path = toolchain_path) ||
          !is_toolchain_installed(app = "mingw32-make", path = toolchain_path)) {
        stop(
          "\nInstallation of the toolchain failed. Try reinstalling RTools and trying again.",
          "\nIf the issue persists, open a bug report at https://github.com/stan-dev/cmdstanr.",
          call. = FALSE
        )
      }
      return(invisible(NULL))
    }
  }
}

check_rtools35_windows_toolchain <- function(fix = FALSE,
                                             quiet = FALSE,
                                             paths = NULL) {
  if (is.null(paths)) {
    paths <- c(file.path("C:/", "Rtools"), file.path("C:/", "Rtools35"))
  }
  mingw32_make_path <- dirname(Sys.which("mingw32-make"))
  gpp_path <- dirname(Sys.which("g++"))
  # If mingw32-make and g++ are not found, we check typical RTools 3.5 folders.
  # If found, we fix PATH, otherwise we recommend the user to install RTools 3.5.
  if (!nzchar(mingw32_make_path) || !nzchar(gpp_path)) {
    rtools_path <- Sys.getenv("RTOOLS35_HOME")
    if (!nzchar(rtools_path)) {
      found_rtools <- FALSE
      for (p in paths) {
        if (dir.exists(p)) {
          if (found_rtools) {
            stop(
              "\nMultiple RTools 3.5 installations found. Please select the installation to use by running",
              "\n\nwrite(\'RTOOLS35_HOME=rtools35/install/path/\', file = \"~/.Renviron\", append = TRUE)",
              "\n\nThen restart R and run 'cmdstanr::check_cmdstan_toolchain(fix = TRUE)'.",
              call. = FALSE
            )
          } else {
            rtools_path <- p
            found_rtools <- TRUE
          }
        }
      }
    }
    if (nzchar(rtools_path)) {
      if (!fix) {
        stop(
          "\nRTools installation found but PATH was not properly set.",
          "\nRun check_cmdstan_toolchain(fix = TRUE) to fix the issue.",
          call. = FALSE
        )
      }
      if (!quiet) {
        message("Writing RTools path to ~/.Renviron ...")
      }
      if (!nzchar(Sys.getenv("RTOOLS35_HOME"))) {
        write(paste0("RTOOLS35_HOME=", rtools_path), file = "~/.Renviron", append = TRUE)
        Sys.setenv(RTOOLS35_HOME = rtools_path)
      }
      write('PATH="${RTOOLS35_HOME}\\bin;${RTOOLS35_HOME}\\mingw_64\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
      Sys.setenv(PATH = paste0(Sys.getenv("RTOOLS35_HOME"), "\\mingw_64\\bin;", Sys.getenv("PATH")))
      check_rtools35_windows_toolchain(fix = FALSE, quiet = quiet)
      return(invisible(NULL))
    } else {
      stop(
        "\nA toolchain was not found. Please install RTools 3.5 and run",
        "\n\nwrite(\'RTOOLS35_HOME=rtools35/install/path/\', file = \"~/.Renviron\", append = TRUE)",
        "\nreplacing 'rtools35/install/path/' with the actual install path of RTools 3.5.",
        "\n\nThen restart R and run 'cmdstanr::check_cmdstan_toolchain(fix = TRUE)'.",
        call. = FALSE
      )
    }
  }
}

check_unix_make <- function() {
  # On Unix systems we check for make and a suitable compiler
  make_path <- dirname(Sys.which("make"))
  if (!nzchar(make_path)) {
    if (os_is_macos()) {
      stop(
        "The 'make' tool was not found. ",
        "Please install the command line tools for Mac with 'xcode-select --install' ",
        "or install Xcode from the app store. ",
        "Then restart R and run cmdstanr::check_cmdstan_toolchain().",
        call. = FALSE
      )
    } else {
      stop(
        "The 'make' tool was not found. ",
        "Please install 'make', restart R, and then run cmdstanr::check_cmdstan_toolchain().",
        call. = FALSE
      )
    }

  }
}

check_unix_cpp_compiler <- function() {
  gpp_path <- dirname(Sys.which("g++"))
  clang_path <- dirname(Sys.which("clang++"))
  if (!nzchar(gpp_path) && !nzchar(clang_path)) {
    if (os_is_macos()) {
      stop(
        "A suitable C++ compiler was not found. ",
        "Please install the command line tools for Mac with 'xcode-select --install' ",
        "or install Xcode from the app store. ",
        "Then restart R and run cmdstanr::check_cmdstan_toolchain().",
        call. = FALSE
      )
    } else {
      stop(
        "A C++ compiler was not found. ",
        "Please install the 'clang++' or 'g++' compiler, restart R, ",
        "and run cmdstanr::check_cmdstan_toolchain().",
        call. = FALSE
      )
    }
  }
}

cmdstan_arch_suffix <- function(version = NULL) {
  os_needs_arch <- os_is_linux() || os_is_wsl()
  if ((!is.null(version) && version < "2.26") || !os_needs_arch) {
    # pre-CmdStan 2.26, only the x86 tarball was provided
    return(NULL)
  }

  arch <- NULL
  if (os_is_wsl()) {
    arch <- wsl_compatible_run(command = "uname", args = "-m")$stdout
    arch <- gsub("\n", "", arch, fixed = TRUE)
  } else {
    arch <- R.version$arch
  }

  if (any(grepl(arch, c("x86_64", "i686")))) {
    return(NULL)
  }

  arch <- gsub("aarch64", "arm64", arch)
  arch <- gsub("armv7l", "armel", arch)
  available_archs <- c("arm64", "armel", "armhf", "mips64el", "ppc64el", "s390x")
  selected_arch <- grep(arch, available_archs, value = TRUE)

  if (length(selected_arch) == 0) {
    stop("Your CPU architecture: ", arch, " is not compatible!", "\n",
          "Supported architectures are: ", paste0(available_archs, collapse = ", "),
          call. = FALSE)
  }

  paste0("-linux-", selected_arch)
}

is_toolchain_installed <- function(app, path) {
  res <- tryCatch({
      withr::with_path(
        c(
          toolchain_PATH_env_var()
        ),
        processx::run(
          app,
          args = c("--version"),
          wd = path,
          error_on_status = FALSE,
          echo_cmd = is_verbose_mode(),
          echo = is_verbose_mode()
        )
      )
      app_path <- withr::with_path(
        c(
          toolchain_PATH_env_var()
        ),
        repair_path(dirname(Sys.which(app)))
      )
      if (normalizePath(app_path) != normalizePath(rtools4x_toolchain_path())) {
        return(FALSE)
      }
      return(TRUE)
    },
    error = function(cond) {
      return(FALSE)
    }
  )
  res
}

toolchain_PATH_env_var <- function() {
  path <- NULL
  if (R.version$major == "4") {
    rtools_home <- rtools4x_home_path()
    path <- paste0(
      repair_path(file.path(rtools_home, "usr", "bin")), ";",
      rtools4x_toolchain_path()
    )
  }
  path
}

rtools4x_toolchain_path <- function() {
  if (arch_is_aarch64()) {
    toolchain <- "aarch64-w64-mingw32.static.posix"
  } else {
    if (Sys.getenv("CMDSTANR_USE_MSYS_TOOLCHAIN") != "" ||
        isTRUE(cmdstan_version(error_on_NA=FALSE) < "2.35.0")) {
      toolchain <- ifelse(is_ucrt_toolchain(), "ucrt64", "mingw64")
    } else {
      toolchain <- "x86_64-w64-mingw32.static.posix"
    }
  }
  repair_path(file.path(rtools4x_home_path(), toolchain, "bin"))
}

rtools4x_version <- function() {
  rtools_ver <- NULL

  if (R.version$minor < "2.0") {
    rtools_ver <- "40"
  } else if (R.version$minor < "3.0") {
    rtools_ver <- "42"
  } else if (R.version$minor < "4.0") {
    rtools_ver <- "43"
  } else if (R.version$minor < "5.0") {
    rtools_ver <- "44"
  } else {
    rtools_ver <- "45"
  }
  rtools_ver
}

rtools4x_home_path <- function() {
  rtools_ver <- rtools4x_version()
  if (arch_is_aarch64()) {
    rtools_ver <- paste0(rtools_ver, "_AARCH64")
  }
  path <- Sys.getenv(paste0("RTOOLS", rtools_ver, "_HOME"))

  if (!nzchar(path)) {
    default_path <- repair_path(file.path(paste0("C:/rtools", rtools_ver)))
    if (arch_is_aarch64()) {
      default_path <- paste0(default_path, "-aarch64")
    }
    if (dir.exists(default_path)) {
      path <- default_path
    }
  }

  path
}

rtools_home_path <- function() {
  path <- NULL
  if (R.version$major == "3") {
    path <- Sys.getenv("RTOOLS_HOME")
  }
  if (R.version$major == "4") {
    path <- rtools4x_home_path()
  }
  path
}
