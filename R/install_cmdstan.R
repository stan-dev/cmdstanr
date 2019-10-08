#' Install the latest release of CmdStan
#'
#' Runs the script `make_cmdstan.sh` (see `inst/make_cmdstan.sh` on
#' [GitHub](https://github.com/stan-dev/cmdstanr)). Currently assumes that the
#' necessary C++ tool chain is available, but in the future CmdStanR may help
#' install the requirements.
#'
#' @export
#' @param dir Path to the directory in which to install CmdStan. The default is
#'   to install it in a folder `.cmdstanr` in the user's home directory
#'   (`Sys.getenv("HOME")`).
#'
install_cmdstan <- function(dir = NULL) {
  make_cmdstan <- system.file("make_cmdstan.sh", package = "cmdstanr")
  if (!is.null(dir)) {
    checkmate::assert_directory_exists(dir)
    make_cmdstan <- c(make_cmdstan, paste0("-d ", dir))
  }
  processx::run("bash", make_cmdstan, echo = TRUE)
}
