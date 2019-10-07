#' Install the latest release of CmdStan
#'
#' @export
#' @param dir Path to the directory in which to install CmdStan. The default is
#'   to install it in a folder `.cmdstanr` in the user's home directory
#'   (`Sys.getenv("HOME")`).
#'
install_cmdstan <- function(dir = NULL) {
  cmd <- "bash"
  args <- system.file("make_cmdstan.sh", package = "cmdstanr")
  if (!is.null(dir)) {
    checkmate::assert_directory_exists(dir)
    args <- c(args, paste0("-d ", dir))
  }
  processx::run(cmd, args, echo = TRUE)
}
