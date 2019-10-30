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
#'   and speed up installation.
#' @param quiet Defaults to `FALSE`, but if `TRUE` will suppress the verbose
#'   output during the installation process.
#' @param force_reinstall Defaults to `FALSE`, but if `TRUE` will reinstall cmdstan
#'   even if the version to be installed is the same as the one currently installed.
#' @param backup_existing Defaults to `FALSE`, but if `TRUE` will backup the currently
#'   installed cmdstan to the folder cmdstan_backup at the location of the current install.
#' @param github_repo_clone Defaults to `FALSE`, but if `TRUE` will install the Github
#'   clone of cmdstan and checkout the develop branch
#'
install_cmdstan <- function(dir = NULL,
                            cores = 2,
                            quiet = FALSE,
                            force_reinstall = FALSE,
                            backup_existing = FALSE,
                            github_repo_clone = FALSE) {
  make_cmdstan <- system.file("make_cmdstan.sh", package = "cmdstanr")
  if (!is.null(dir)) {
    checkmate::assert_directory_exists(dir)
    make_cmdstan <- c(make_cmdstan, paste0("-d ", dir))
  }
  make_cmdstan <- c(make_cmdstan, paste0("-j", cores))
  if (os_is_windows()) {
    make_cmdstan <- c(make_cmdstan, "-w")
  }
  if (backup_existing) {
    make_cmdstan <- c(make_cmdstan, "-b")
  }
  if (github_repo_clone) {
    make_cmdstan <- c(make_cmdstan, "-c")
  }
  if (force_reinstall) {
    make_cmdstan <- c(make_cmdstan, "-f")
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
