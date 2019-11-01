#' Install the latest release of CmdStan
#'
#' \if{html}{\figure{logo.png}{options: width="25px" alt="https://mc-stan.org/about/logo/"}}
#' The `install_cmdstan()` function runs a script
#' (see `inst/make_cmdstan.sh` on [GitHub](https://github.com/stan-dev/cmdstanr))
#' that by default attempts to download and install the latest release of
#' [CmdStan](https://github.com/stan-dev/cmdstan/releases/latest). Currently the
#' necessary C++ tool chain is assumed to be available (see Appendix B of the
#' CmdStan [guide](https://github.com/stan-dev/cmdstan/releases/latest)),
#' but in the future CmdStanR may help install the requirements.
#'
#' @export
#' @param dir Path to the directory in which to install CmdStan. The default is
#'   to install it in a directory called `.cmdstanr` within the user's home
#'   directory (i.e, `file.path(Sys.getenv("HOME"), ".cmdstanr")`).
#' @param cores The number of CPU cores to use to parallelize building CmdStan
#'   and speed up installation. The default is `cores=2`, although we recommend
#'   using more cores if available.
#' @param quiet Should the verbose output from the system process be suppressed
#'   during installation? The default is `FALSE`.
#' @param overwrite When an existing installation is found in `dir`, should
#'   CmdStan still be downloaded and reinstalled? The default is `FALSE`, in
#'   which case an informative error is thrown instead of overwriting the user's
#'   installation.
#' @param repo_clone If `FALSE` (the default), the latest CmdStan release is
#'   downloaded and installed from tarball. If `TRUE` will install a git clone
#'   of CmdStan from `repo_url` and check out the branch `repo_branch`.
#' @param repo_url If `repo_clone` is `TRUE`, the URL of the git repository to
#'   clone. The default URL points to the
#'   [`stan-dev/cmdstan`](https://github.com/stan-dev/cmdstan)
#'   repository on GitHub.
#' @param repo_branch If `repo_clone` is `TRUE`, the name of the branch to
#'   checkout in the cloned repository.
#'
#' @return [Invisibly][base::invisible], the list returned by [processx::run()],
#'   which contains information about the system process that was run. See the
#'   **Value** section at [processx::run()] for details.
#'
#' @seealso [cmdstan_git_checkout_branch()]
#'
install_cmdstan <- function(dir = NULL,
                            cores = 2,
                            quiet = FALSE,
                            overwrite = FALSE,
                            repo_clone = FALSE,
                            repo_url = "https://github.com/stan-dev/cmdstan.git",
                            repo_branch = "develop") {
  make_cmdstan <- system.file("make_cmdstan.sh", package = "cmdstanr")
  if (!is.null(dir)) {
    checkmate::assert_directory_exists(dir)
    make_cmdstan <- c(make_cmdstan, paste0("-d ", dir))
  }
  make_cmdstan <- c(make_cmdstan, paste0("-j", cores))
  if (os_is_windows()) {
    make_cmdstan <- c(make_cmdstan, "-w")
  }
  if (overwrite) {
    make_cmdstan <- c(make_cmdstan, "-o")
  }
  if (repo_clone) {
    make_cmdstan <- c(make_cmdstan, "-r", "-u", repo_url, "-b", repo_branch)
  }

  install_log <- processx::run(
    command = "bash",
    args = make_cmdstan,
    echo_cmd = FALSE,
    echo = !quiet,
    spinner = quiet,
    error_on_status = FALSE
  )

  if (is.na(install_log$status) || install_log$status != 0) {
    if (!quiet) {
      suggestion <- "See the error message(s) above."
    } else {
      suggestion <- "Please try again with 'quiet=FALSE' to get the full error output."
    }
    cat("\n")
    warning("There was a problem during installation. ", suggestion,
            call. = FALSE)
    return(invisible(install_log))
  }

  if (!is.null(dir)) {
    install_path <- file.path(dir, "cmdstan")
  } else {
    install_path <- cmdstan_default_path()
  }

  set_cmdstan_path(install_path)
  invisible(install_log)
}

#' Check out a branch from a git repository and rebuild CmdStan
#'
#' This only works if the installed CmdStan is from a clone of the git
#' repository. See [install_cmdstan] for more details.
#'
#' @export
#' @inheritParams install_cmdstan
#' @param clean_and_rebuild If `TRUE` (the default), will run `make clean-all`
#'   and `make build` after checking out the new branch.
#' @inherit install_cmdstan return
#'
#' @seealso [install_cmdstan()]
#'
cmdstan_git_checkout_branch <- function(repo_branch,
                                        clean_and_rebuild = TRUE,
                                        cores = 2,
                                        quiet = FALSE) {
  make_cmdstan <- system.file("make_cmdstan.sh", package = "cmdstanr")
  make_cmdstan <- c(make_cmdstan, paste0("-d ", cmdstan_path()))
  make_cmdstan <- c(make_cmdstan, paste0("-j", cores))
  make_cmdstan <- c(make_cmdstan, "-c", "-b", repo_branch)
  if (clean_and_rebuild) {
    make_cmdstan <- c(make_cmdstan, "-p")
  }
  install_log <- processx::run(
    command = "bash",
    args = make_cmdstan,
    echo = !quiet,
    echo_cmd = !quiet,
    spinner = quiet
  )
  invisible(install_log)
}
