#' Install the latest release of CmdStan
#'
#' \if{html}{\figure{logo.png}{options: width="25px" alt="https://mc-stan.org/about/logo/"}}
#' The `install_cmdstan()` function runs a script
#' (see `inst/make_cmdstan.sh` on [GitHub](https://github.com/stan-dev/cmdstanr))
#' that attempts to download and install the latest release of
#' [CmdStan](https://github.com/stan-dev/cmdstan/releases/latest). Currently the
#' necessary C++ tool chain is assumed to be available (see Appendix B of the
#' CmdStan [guide](https://github.com/stan-dev/cmdstan/releases/latest)),
#' but in the future CmdStanR may help install the requirements.
#'
#' @export
#' @param dir Path to the directory in which to install CmdStan. The default is
#'   to install it in a folder `".cmdstanr"` in the user's home directory
#'   (`Sys.getenv("HOME")`).
#' @param cores The number of CPU cores to use to parallelize building CmdStan
#'   and speed up installation. The default is `cores=2`, although we recommend
#'   using more cores if available.
#' @param quiet Defaults to `FALSE`, but if `TRUE` will suppress the verbose
#'   output during the installation process.
#' @param overwrite Defaults to `FALSE`, but if `TRUE` will download and
#'   reinstall CmdStan even if an existing installation was found.
#' @param repo_clone Defaults to `FALSE`, but if `TRUE` will install a
#'   git clone of cmdstan and checkout the specified branch.
#' @param repo_url The URL of the git repository to clone. The default URL
#'   is the stan-dev cmdstan github repository
#' @param repo_branch The branch to checkout in the cloned repository
#'
#' @return [Invisibly][base::invisible], the list returned by [processx::run()],
#'   which contains information about the system process that was run. See the
#'   **Value** section at [processx::run()] for details.
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
    echo = !quiet,
    echo_cmd = !quiet,
    spinner = quiet
  )

  if (!is.null(dir)) {
    install_path <- file.path(dir, "cmdstan")
  } else {
    install_path <- cmdstan_default_path()
  }

  message(
    "\nUse set_cmdstan_path('", install_path, "') ",
    "to point CmdStanR to the location of the new installation."
  )
  invisible(install_log)
}

#' Checks out a branch in the cmdstan folder if the installed cmdstan is a clone of a git
#' repository
#'
#' @export
#' @param branch_name Name of the git repository branch to checkout
#' @param clean_and_rebuild If `TRUE` will run make clean-all and make build after
#'   cheking out the new branch.
#' @param cores The number of CPU cores to use to parallelize building CmdStan.
#'   The default is `cores=2`, although we recommend
#'   using more cores if available.
#' @param quiet Defaults to `FALSE`, but if `TRUE` will suppress the verbose
#'   output during the checkout process.
#'
cmdstan_git_checkout_branch <- function(branch_name,
                                        clean_and_rebuild = TRUE,
                                        cores = 2,
                                        quiet = FALSE) {
  make_cmdstan <- system.file("make_cmdstan.sh", package = "cmdstanr")
  make_cmdstan <- c(make_cmdstan, paste0("-d ", cmdstan_path()))
  make_cmdstan <- c(make_cmdstan, paste0("-j", cores))
  make_cmdstan <- c(make_cmdstan, "-c", "-b", branch_name)
  install_log <- processx::run(
    command = "bash",
    args = make_cmdstan,
    echo = !quiet,
    echo_cmd = !quiet,
    spinner = quiet
  )
}
