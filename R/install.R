#' Install CmdStan or clean and rebuild an existing installation
#'
#' @description The `install_cmdstan()` function attempts to download and
#'   install the latest release of
#'   [CmdStan](https://github.com/stan-dev/cmdstan/releases/latest). Installing
#'   a previous release or a new release candidate is also possible by
#'   specifying the `release_url` argument. Currently the necessary C++ tool
#'   chain is assumed to be available, but in the future CmdStanR may help
#'   install the requirements. See the first few sections of the
#'   CmdStan [installation guide](https://mc-stan.org/docs/2_24/cmdstan-guide/cmdstan-installation.html)
#'   for details on the required toolchain.
#'
#'   The `rebuild_cmdstan()` function cleans and rebuilds the cmdstan
#'   installation. Use this function in case of any issues when compiling
#'   models.
#'
#'   The `cmdstan_make_local()` function is used to read/write makefile flags
#'   and variables from/to the `make/local` file of a CmdStan installation.
#'   Writing to the `make/local` file can be used to permanently add makefile
#'   flags/variables to an installation. For example adding specific compiler
#'   switches, changing the C++ compiler, etc. A change to the `make/local` file
#'   should typically be followed by calling `rebuild_cmdstan()`.
#'
#' @export
#' @param dir Path to the directory in which to install CmdStan. The default is
#'   to install it in a directory called `.cmdstanr` within the user's home
#'   directory (i.e, `file.path(Sys.getenv("HOME"), ".cmdstanr")`).
#' @param cores The number of CPU cores to use to parallelize building CmdStan
#'   and speed up installation. If `cores` is not specified then the default is
#'   to look for the option `"mc.cores"`, which can be set for an entire \R
#'   session by `options(mc.cores=value)`. If the `"mc.cores"` option has not
#'   been set then the default is `2`.
#' @param quiet Should the verbose output from the system processes be
#'   suppressed when building the CmdStan binaries? The default is `FALSE`.
#' @param overwrite When an existing installation is found in `dir`, should
#'   CmdStan still be downloaded and reinstalled? The default is `FALSE`, in
#'   which case an informative error is thrown instead of overwriting the user's
#'   installation.
#' @param timeout Timeout (in seconds) for the CmdStan build stage of the
#'   installation process.
#' @param release_url Specifies the URL to a specific Cmdstan release to be
#'   installed. By default set to `NULL`, which downloads the latest stable
#'   release from [GitHub](https://github.com/stan-dev/cmdstan/releases).
#' @param cpp_options A list specifying any makefile flags/variables to be
#'   written to the `make/local` file. For example, `list("CXX" = "clang++")`
#'   will force the use of clang for compilation.
#'
#' @examples
#' # install_cmdstan(cores = 4)
#'
install_cmdstan <- function(dir = NULL,
                            cores = getOption("mc.cores", 2),
                            quiet = FALSE,
                            overwrite = FALSE,
                            timeout = 1200,
                            release_url = NULL,
                            cpp_options = list(),
                            check_toolchain = TRUE) {
  if (check_toolchain) check_cmdstan_toolchain(fix = FALSE, quiet = quiet)
  if (is.null(dir)) {
    dir <- cmdstan_default_install_path()
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  } else {
    dir <- repair_path(dir)
    checkmate::assert_directory_exists(dir, access = "rwx")    
  }

  if (!is.null(release_url)) {
    if (!endsWith(release_url, ".tar.gz")) {
      stop(release_url, " is not a .tar.gz archive!",
           "cmdstanr supports installing from .tar.gz archives only.")
    }
    message("* Installing Cmdstan from ", release_url)
    download_url <- release_url
    split_url <- strsplit(release_url, "/")
    tar_name <- utils::tail(split_url[[1]], n=1)
    cmdstan_ver <- substr(tar_name, 0, nchar(tar_name)-7)
    tar_gz_file <- paste0(cmdstan_ver, ".tar.gz")
    dir_cmdstan <- file.path(dir, cmdstan_ver)
    dest_file <- file.path(dir, tar_gz_file)
  } else {
    ver <- latest_released_version()
    message("* Latest CmdStan release is v", ver)
    cmdstan_ver <- paste0("cmdstan-", ver)
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
    stop("GitHub download of Cmdstan failed.", call. = FALSE)
  }
  message("* Download complete")

  message("* Unpacking archive...")
  untar_rc <- utils::untar(
    dest_file,
    exdir = dir_cmdstan,
    extras = "--strip-components 1"
  )
  if (untar_rc != 0) {
    stop("Problem extracting tarball. Exited with return code: ", untar_rc,
          call. = FALSE)
  }
  file.remove(dest_file)
  cmdstan_make_local(dir = dir_cmdstan, cpp_options = cpp_options, append = TRUE)
  version <- read_cmdstan_version(dir_cmdstan)
  if (os_is_windows()) {
    if (version >= "2.24" && R.version$major >= "4") {
      # cmdstan 2.24 can use precompiled headers with RTools 4.0 to speedup compiling
      cmdstan_make_local(
        dir = dir_cmdstan,
        cpp_options = list(
          PRECOMPILED_HEADERS = TRUE
        ),
        append = TRUE
      )
    }
    if (version < "2.24") {
      # cmdstan 2.23 and earlier prints a lot of warnings with RTools 4.0 on Windows
      # this disables them
      cmdstan_make_local(
        dir = dir_cmdstan,
        cpp_options = list(
          "ifeq (gcc,$(CXX_TYPE))",
          "CXXFLAGS_WARNINGS+= -Wno-int-in-bool-context -Wno-attributes",
          "endif"
        ),
        append = TRUE
      )
    }
    if (version > "2.22" && version < "2.24") {
      # cmdstan 2.23 unnecessarily required chmod after moving the windows-stanc
      # this moves the exe file so the make command that requires chmod is not used
      windows_stanc <- file.path(dir_cmdstan, "bin", "windows-stanc")
      bin_stanc_exe <- file.path(dir_cmdstan, "bin", "stanc.exe")
      if (file.exists(windows_stanc)) {
        file.copy(windows_stanc, bin_stanc_exe)
      }
    }
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
#' @param append For `cmdstan_make_local()`, should the listed makefile flags be
#'   appended to the end of the existing `make/local` file? The default is `TRUE`.
#'   If `FALSE` the file is overwritten.
#' @return For `cmdstan_make_local()`, if `cpp_options=NULL` then the existing
#'   contents of `make/local` are returned without writing anything, otherwise
#'   the updated contents are returned.
#' @examples
#' cpp_options <- list(
#'   "CXX" = "clang++",
#'   "CXXFLAGS+= -march-native",
#'   PRECOMPILED_HEADERS = TRUE
#' )
#' # cmdstan_make_local(cpp_options = cpp_options)
#' # rebuild_cmdstan()
#'
cmdstan_make_local <- function(dir = cmdstan_path(),
                               cpp_options = NULL,
                               append = TRUE) {
  make_local_path <- file.path(dir, "make", "local")
  if (!is.null(cpp_options)) {
    built_flags = c()
    for (i in seq_len(length(cpp_options))) {
      option_name <- names(cpp_options)[i]
      if (isTRUE(as.logical(cpp_options[[i]]))) {
        built_flags = c(built_flags, paste0(option_name, "=true"))
      } else if (isFALSE(as.logical(cpp_options[[i]]))) {
        built_flags = c(built_flags, paste0(option_name, "=false"))
      } else {
        if (is.null(option_name) || !nzchar(option_name)) {
          built_flags = c(built_flags, paste0(cpp_options[[i]]))
        } else {
          built_flags = c(built_flags, paste0(option_name, "=", cpp_options[[i]]))
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

# construct url for download from cmdstan version number
github_download_url <- function(version_number) {
  base_url <- "https://github.com/stan-dev/cmdstan/releases/download/"
  paste0(base_url, "v", version_number,
         "/cmdstan-", version_number, ".tar.gz")
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
      download_rc <- utils::download.file(url = download_url,
                                          destfile = destination_file,
                                          quiet = quiet)
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
  processx::run(
    make_cmd(),
    args = c(paste0("-j", cores), "build"),
    wd = dir,
    echo_cmd = FALSE,
    echo = !quiet,
    spinner = quiet,
    error_on_status = FALSE,
    stderr_line_callback = function(x,p) { if (quiet) message(x) },
    timeout = timeout
  )
}

# Removes files that are used to simplify switching to using threading,
# opencl or mpi.
clean_compile_helper_files <- function() {
  # remove main_.*.o files and model_header_.*.hpp.gch files
  files_to_remove <- c(
    list.files(
      path = file.path(cmdstan_path(), "src", "cmdstan"),
      pattern = "main.*\\.o$",
      full.names = TRUE
    ),
    list.files(
      path = file.path(cmdstan_path(), "src", "cmdstan"),
      pattern = "main.*\\.d$",
      full.names = TRUE
    ),
    list.files(
      path = file.path(cmdstan_path(), "stan", "src", "stan", "model"),
      pattern = "model_header.*\\.hpp.gch$",
      full.names = TRUE
    ),
    list.files(
      path = file.path(cmdstan_path(), "stan", "src", "stan", "model"),
      pattern = "model_header.*\\.d$",
      full.names = TRUE
    )
  )
  if (!is.null(files_to_remove)) {
    file.remove(files_to_remove)
  }
}

clean_cmdstan <- function(dir = cmdstan_path(),
                          cores = getOption("mc.cores", 2),
                          quiet = FALSE) {
  processx::run(
    make_cmd(),
    args = c("clean-all"),
    wd = dir,
    echo_cmd = FALSE,
    echo = !quiet,
    spinner = quiet,
    error_on_status = FALSE,
    stderr_line_callback = function(x,p) { if (quiet) message(x) }
  )
  clean_compile_helper_files()
}

build_example <- function(dir, cores, quiet, timeout) {
  processx::run(
    make_cmd(),
    args = c(paste0("-j", cores), cmdstan_ext("examples/bernoulli/bernoulli")),
    wd = dir,
    echo_cmd = FALSE,
    echo = !quiet,
    spinner = quiet,
    error_on_status = FALSE,
    stderr_line_callback = function(x,p) { if (quiet) message(x) },
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
install_mingw32_make <- function() {
  processx::run(
    "pacman",
    args = c("-Syu", "mingw-w64-x86_64-make","--noconfirm"),
    wd = file.path(Sys.getenv("RTOOLS40_HOME"), "usr", "bin"),
    error_on_status = TRUE
  )
  invisible(NULL)
}

#' @rdname install_cmdstan
#' @export
#' @param fix For `check_cmdstan_toolchain()`, should the function install
#'   attempt to fix toolchain issues. This function currently only attempts
#'   fixing Windows toolchain issues. The default is `FALSE`,
#'   If `FALSE`, the function will only report issues.
#' @param quiet For `check_cmdstan_toolchain()`, should the function supress 
#'   printing informational messages? The default is `FALSE`.
#'   If `TRUE` the function will only print errors.
#' @examples
#' # check_cmdstan_toolchain(fix = FALSE, quiet = FALSE)
#'
check_cmdstan_toolchain <- function(fix = FALSE, quiet = FALSE) {
  if(os_is_windows()) {
    if (R.version$major >= "4") {
      rtools_path <- Sys.getenv("RTOOLS40_HOME")
      # If RTOOLS40_HOME is not set (the env. variable gets set on install)
      # we assume that RTools 40 is not installed.
      if (!nzchar(rtools_path)) {
        error_message <- c(
          "",
          "RTools 4.0 is required to run cmdstanr with R version 4.x, but was not found.",
          "Install RTools 4.0 and run 'cmdstanr::install_cmdstan_toolchain()'"
        )
        stop(paste0(error_message, collapse = "\n"), call. = FALSE)
      }
      # If RTools is installed in a path with spaces or brackets
      # we error as this path is not valid
      if (regexpr("\\(|)| ", rtools_path) > 0) {
        error_message <- c(
          "",
          "RTools 4.0 is installed in a path with spaces or brackets, which is not supported.",
          "Reinstall RTools 4.0 to a valid path, restart R and run 'cmdstanr::install_cmdstan_toolchain()'"
        )
        stop(paste0(error_message, collapse = "\n"), call. = FALSE)
      }
      if (fix) {
        install_mingw32_make()
      }
      toolchain_path <- repair_path(file.path(rtools_path, "mingw64", "bin"))
      mingw32_make_path <- dirname(Sys.which("mingw32-make"))
      gpp_path <- dirname(Sys.which("g++"))
      
      if (!nzchar(mingw32_make_path) || !nzchar(gpp_path)) {
        if (!fix) {
          error_message <- c(
            "",
            "RTools installation found but PATH was not properly set.",
            "Run 'cmdstanr::install_cmdstan_toolchain(check_only = FALSE)' to fix the issue."
          )
          stop(paste0(error_message, collapse = "\n"), call. = FALSE)
        } else {
          if (!quiet) message("Writing RTools path to ~/.Renviron ...")
          write('PATH="${RTOOLS40_HOME}\\usr\\bin;${RTOOLS40_HOME}\\mingw64\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
          stop("Restart R and run install_cmdstan_toolchain(check_only = TRUE) to confirm the installation was successful.", call. = FALSE)
        }
      }
      # Check if the mingw32-make and g++ get picked up by default are the RTools-supplied ones
      if (toolchain_path != mingw32_make_path || gpp_path != toolchain_path) {
        if (!fix) {
          error_message <- c(
            "",
            "Other C++ toolchains installed on your system conflict with RTools.",
            "Run 'cmdstanr::install_cmdstan_toolchain(check_only = FALSE)' to fix the issue."
          )
          stop(paste0(error_message, collapse = "\n"), call. = FALSE)
        } else {
          if (!quiet) message("Writing RTools path to ~/.Renviron ...")
          write('PATH="${RTOOLS40_HOME}\\usr\\bin;${RTOOLS40_HOME}\\mingw64\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
          stop("Restart R and run install_cmdstan_toolchain(check_only = TRUE) to confirm the installation was successful.", call. = FALSE)
        }
      }
    } else {
      mingw32_make_path <- dirname(Sys.which("mingw32-make"))
      gpp_path <- dirname(Sys.which("g++"))
      # If mingw32-make and g++ are not found, we check typical RTools 3.5 folders.
      # If found, we fix PATH, otherwise we recommend the user to install RTools 3.5.
      if (!nzchar(mingw32_make_path) || !nzchar(gpp_path)) {
        rtools_path <- Sys.getenv("RTOOLS35_HOME")
        if (!nzchar(rtools_path)) {
          typical_paths <- c(
            file.path("C:/", "Rtools"),
            file.path("C:/", "Rtools35"),
            file.path("C:/", "RTools"),
            file.path("C:/", "RTools35"),
            file.path("C:/", "rtools"),
            file.path("C:/", "rtools35"),
            file.path("c:/", "Rtools"),
            file.path("c:/", "Rtools35"),
            file.path("c:/", "RTools"),
            file.path("c:/", "RTools35"),
            file.path("c:/", "rtools"),
            file.path("c:/", "rtools35")
          )
          for (p in typical_paths) {
            if (dir.exists(p)) {
              rtools_path <- p
            }
          }
        }
        if (nzchar(rtools_path)) {
          if (!fix) {
            error_message <- c(
              "",
              "A RTools installation was found, but the PATH is not properly set.",
              "Run 'cmdstanr::install_cmdstan_toolchain(check_only = FALSE)' to fix the issue."
            )
            stop(paste0(error_message, collapse = "\n"), call. = FALSE)
          }
          if (!quiet) message("Writing RTools path to ~/.Renviron ...")
          if (nzchar(Sys.getenv("RTOOLS35_HOME"))) {
            write(paste0('RTOOLS35_HOME=', rtools_path), file = "~/.Renviron", append = TRUE)
          }
          write('PATH="${RTOOLS35_HOME}\\bin;${RTOOLS35_HOME}\\mingw_64\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
          stop("Restart R and run install_cmdstan_toolchain(check_only = TRUE) to confirm the installation was successful.", call. = FALSE)
        } else {
          error_message <- c(
            "",
            "A toolchain was not found. Please install RTools 3.5 and run",
            "",
            "write(\'RTOOLS35_HOME=rtools35/install/path/\', file = \"~/.Renviron\", append = TRUE)",
            "replacing 'rtools35/install/path/' with the actual install path of RTools 3.5.",
            "",
            "Then restart R and run 'cmdstanr::install_cmdstan_toolchain(check_only = FALSE)'."
          )
          stop(paste0(error_message, collapse = "\n"), call. = FALSE)
        }
      }
    }
  } else {
    # On Unix systems we check for make and a suitable compiler
    make_path <- dirname(Sys.which("make"))
    if (!nzchar(make_path)) {
      if (os_is_macos()) {
        stop("The 'make' tool was not found. Please install 'xcode' toolchains with 'xcode-select --install' or the app store and restart the R session.", call. = FALSE)
      } else {
        stop("The 'make' tool was not found. Please install 'make' and restart the R session.", call. = FALSE)
      }
      
    }
    gpp_path <- dirname(Sys.which("g++"))
    clang_path <- dirname(Sys.which("clang++"))
    if (!nzchar(gpp_path) && !nzchar(clang_path)) {
      if (os_is_macos()) {
        stop("A suitable C++ compiler was not found. Please install 'xcode' toolchains with 'xcode-select --install' or the app store and restart the R session.", call. = FALSE)
      } else {
        stop("A C++ compiler was not found. Please install the 'clang++' or 'g++' compiler and restart the R session.", call. = FALSE)
      }
    }
    if (nzchar(gpp_path)) {
      processx::
    }
  }
  if (!quiet) message("The CmdStan toolchain is setup properly!")
  invisible(NULL)
}
