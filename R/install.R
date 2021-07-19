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
#'   be called directly by the user.
#'
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
#' @param cpp_options (list) Any makefile flags/variables to be written to
#'   the `make/local` file. For example, `list("CXX" = "clang++")` will force
#'   the use of clang for compilation.
#' @param check_toolchain (logical) Should `install_cmdstan()` attempt to check
#'   that the required toolchain is installed and properly configured. The
#'   default is `TRUE`.
#'
#' @examples
#' \dontrun{
#' check_cmdstan_toolchain()
#'
#' # install_cmdstan(cores = 4)
#'
#' cpp_options <- list(
#'   "CXX" = "clang++",
#'   "CXXFLAGS+= -march-native",
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
                            cpp_options = list(),
                            check_toolchain = TRUE) {
  if (check_toolchain) {
    check_cmdstan_toolchain(fix = FALSE, quiet = quiet)
  }
  if (is.null(dir)) {
    dir <- cmdstan_default_install_path()
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  } else {
    dir <- repair_path(dir)
    checkmate::assert_directory_exists(dir, access = "rwx")
  }
  if (!is.null(version)) {
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
    cmdstan_ver <- substr(tar_name, 0, nchar(tar_name) - 7)
    tar_gz_file <- paste0(cmdstan_ver, ".tar.gz")
    dir_cmdstan <- file.path(dir, cmdstan_ver)
    dest_file <- file.path(dir, tar_gz_file)
  } else {
    ver <- latest_released_version()
    message("* Latest CmdStan release is v", ver)
    cmdstan_ver <- paste0("cmdstan-", ver, cmdstan_arch_suffix(ver))
    tar_gz_file <- paste0(cmdstan_ver, ".tar.gz")
    dir_cmdstan <- file.path(dir, cmdstan_ver)
    message("* Installing CmdStan v", ver, " in ", dir_cmdstan)
    message("* Downloading ", tar_gz_file, " from GitHub...")
    download_url <- github_download_url(ver)
    dest_file <- file.path(dir, tar_gz_file)
  }
  if (!check_install_dir(dir_cmdstan, overwrite)) {
    return(invisible(NULL))
  }
  tar_downloaded <- download_with_retries(download_url, dest_file)
  if (!tar_downloaded) {
    if (!is.null(version)) {
      stop("Download of CmdStan failed. Please check if the supplied version number is valid.", call. = FALSE)
    }
    if (!is.null(release_url)) {
      stop("Download of CmdStan failed. Please check if the supplied release URL is valid.", call. = FALSE)
    }
    stop("Download of CmdStan failed. Please try again.", call. = FALSE)
  }
  message("* Download complete")

  message("* Unpacking archive...")
  untar_rc <- utils::untar(
    dest_file,
    exdir = dir_cmdstan,
    extras = "--strip-components 1"
  )
  if (untar_rc != 0) {
    stop("Problem extracting tarball. Exited with return code: ", untar_rc, call. = FALSE)
  }
  file.remove(dest_file)
  cmdstan_make_local(dir = dir_cmdstan, cpp_options = cpp_options, append = TRUE)
  version <- read_cmdstan_version(dir_cmdstan)
  if (os_is_windows()) {
    if (version >= "2.24" && R.version$major >= "4" && !("PRECOMPILED_HEADERS" %in% toupper(names(cpp_options)))) {
      # cmdstan 2.24 can use precompiled headers with RTools 4.0 to speedup compiling
      cmdstan_make_local(
        dir = dir_cmdstan,
        cpp_options = list(
          PRECOMPILED_HEADERS = TRUE
        ),
        append = TRUE
      )
    }
  }
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
        built_flags <- c(built_flags, paste0(option_name, "=true"))
      } else if (isFALSE(as.logical(cpp_options[[i]]))) {
        built_flags <- c(built_flags, paste0(option_name, "=false"))
      } else {
        if (is.null(option_name) || !nzchar(option_name)) {
          built_flags <- c(built_flags, paste0(cpp_options[[i]]))
        } else {
          built_flags <- c(built_flags, paste0(option_name, "=", cpp_options[[i]]))
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
    if (R.version$major >= "4") {
      check_rtools40_windows_toolchain(fix = fix, quiet = quiet)
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
latest_released_version <- function() {
  dest_file <- tempfile(pattern = "releases-", fileext = ".json")
  download_url <- "https://api.github.com/repos/stan-dev/cmdstan/releases/latest"
  release_list_downloaded <- download_with_retries(download_url, dest_file)
  if (!release_list_downloaded) {
    stop("GitHub download of release list failed.", call. = FALSE)
  }
  release <- jsonlite::read_json(dest_file)
  sub("v", "", release$tag_name)
}

# download with retries and pauses
download_with_retries <- function(download_url,
                                  destination_file,
                                  retries = 5,
                                  pause_sec = 5,
                                  quiet = TRUE) {

    download_rc <- 1
    while (retries > 0 && download_rc != 0) {
      try(
        suppressWarnings(
          download_rc <- utils::download.file(url = download_url,
                                            destfile = destination_file,
                                            quiet = quiet,
                                            headers = github_auth_token())
        ),
        silent = TRUE
      )
      if (download_rc != 0) {
        Sys.sleep(pause_sec)
      }
      retries <- retries - 1
    }
    if (download_rc == 0) {
      TRUE
    } else {
      FALSE
    }
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
  processx::run(
    run_cmd,
    args = c(translation_args, paste0("-j", cores), "build"),
    wd = dir,
    echo_cmd = is_verbose_mode(),
    echo = !quiet || is_verbose_mode(),
    spinner = quiet,
    error_on_status = FALSE,
    stderr_callback = function(x, p) { if (quiet) message(x) },
    timeout = timeout
  )
}

clean_cmdstan <- function(dir = cmdstan_path(),
                          cores = getOption("mc.cores", 2),
                          quiet = FALSE) {
  processx::run(
    make_cmd(),
    args = c("clean-all"),
    wd = dir,
    echo_cmd = is_verbose_mode(),
    echo = !quiet || is_verbose_mode(),
    spinner = quiet,
    error_on_status = FALSE,
    stderr_callback = function(x, p) { if (quiet) message(x) }
  )
}

build_example <- function(dir, cores, quiet, timeout) {
  processx::run(
    make_cmd(),
    args = c(paste0("-j", cores), cmdstan_ext(file.path("examples", "bernoulli", "bernoulli"))),
    wd = dir,
    echo_cmd = is_verbose_mode(),
    echo = !quiet || is_verbose_mode(),
    spinner = quiet,
    error_on_status = FALSE,
    stderr_callback = function(x, p) { if (quiet) message(x) },
    timeout = timeout
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

install_mingw32_make <- function(quiet = FALSE) {
  rtools_usr_bin <- file.path(Sys.getenv("RTOOLS40_HOME"), "usr", "bin")
  if (!checkmate::test_directory(rtools_usr_bin, access = "w")) {
    warning("No write permissions in the RTools folder. This might prevent installing mingw32-make.",
            " Consider changing permissions or reinstalling RTools in a different folder.", call. = FALSE)
  }
  if (!quiet) message("Installing mingw32-make and writing RTools path to ~/.Renviron ...")
  processx::run(
    "pacman",
    args = c("-Syu", "mingw-w64-x86_64-make", "--noconfirm"),
    wd = rtools_usr_bin,
    error_on_status = TRUE,
    echo_cmd = is_verbose_mode(),
    echo = is_verbose_mode()
  )
  write('PATH="${RTOOLS40_HOME}\\usr\\bin;${RTOOLS40_HOME}\\mingw64\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
  Sys.setenv(PATH = paste0(Sys.getenv("RTOOLS40_HOME"), "\\usr\\bin;", Sys.getenv("RTOOLS40_HOME"), "\\mingw64\\bin;", Sys.getenv("PATH")))
  invisible(NULL)
}

check_rtools40_windows_toolchain <- function(fix = FALSE, quiet = FALSE) {
  rtools_path <- Sys.getenv("RTOOLS40_HOME")
  # If RTOOLS40_HOME is not set (the env. variable gets set on install)
  # we assume that RTools 40 is not installed.
  if (!nzchar(rtools_path)) {
    stop(
      "\nRTools 4.0 was not found but is required to run CmdStan with R version 4.x.",
      "\nPlease install RTools 4.0 and run check_cmdstan_toolchain().",
      call. = FALSE
    )
  }
  # If RTools is installed in a path with spaces or brackets
  # we error as this path is not valid
  if (grepl("\\(|)| ", rtools_path)) {
    stop(
      "\nRTools 4.0 is installed in a path with spaces or brackets, which is not supported.",
      "\nPlease reinstall RTools 4.0 to a valid path, restart R, and then run check_cmdstan_toolchain().",
      call. = FALSE
    )
  }
  toolchain_path <- repair_path(file.path(rtools_path, "mingw64", "bin"))
  mingw32_make_path <- dirname(Sys.which("mingw32-make"))
  gpp_path <- dirname(Sys.which("g++"))
  if (!nzchar(mingw32_make_path) || !nzchar(gpp_path)) {
    if (!fix) {
      stop(
        "\nRTools installation found but PATH was not properly set.",
        "\nRun check_cmdstan_toolchain(fix = TRUE) to fix the issue.",
        call. = FALSE
      )
    } else {
      install_mingw32_make(quiet = quiet)
      check_rtools40_windows_toolchain(fix = FALSE, quiet = quiet)
      return(invisible(NULL))
    }
  }
  # Check if the mingw32-make and g++ get picked up by default are the RTools-supplied ones
  if (toolchain_path != mingw32_make_path || gpp_path != toolchain_path) {
    if (!fix) {
      stop(
        "\nOther C++ toolchains installed on your system conflict with RTools.",
        "\nPlease run check_cmdstan_toolchain(fix = TRUE) to fix the issue.",
        call. = FALSE
      )
    } else {
      install_mingw32_make(quiet = quiet)
      check_rtools40_windows_toolchain(fix = FALSE, quiet = quiet)
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
        "Then restart R and run check_cmdstan_toolchain().",
        call. = FALSE
      )
    } else {
      stop(
        "The 'make' tool was not found. ",
        "Please install 'make', restart R, and then run check_cmdstan_toolchain().",
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
        "Then restart R and run check_cmdstan_toolchain().",
        call. = FALSE
      )
    } else {
      stop(
        "A C++ compiler was not found. ",
        "Please install the 'clang++' or 'g++' compiler, restart R, ",
        "and run check_cmdstan_toolchain().",
        call. = FALSE
      )
    }
  }
}

cmdstan_arch_suffix <- function(version = NULL) {
  arch <- NULL
  if (grepl("linux", R.version$os) && grepl("aarch64", R.version$arch)) {
    arch <- "-linux-arm64"
  }  
  if (!is.null(version) && version < "2.26") {
    # pre-CmdStan 2.26, only the x85 tarball was provided
    arch <- NULL
  }
  arch
}