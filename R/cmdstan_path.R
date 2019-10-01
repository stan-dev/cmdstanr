#' Get or set the file path to the CmdStan installation
#'
#' `cmdstan_path()` returns the full path to the CmdStan installation. This can
#' be set using the `set_cmdstan_path()` function. When the package is loaded,
#' if the [environment variable][Sys.setenv()] `CMDSTAN` exists its value will
#' be used as the path to CmdStan unless a different path is set using
#' `set_cmdstan_path()`.
#'
#' @export
#' @return The full file path to the CmdStan installation.
#'
cmdstan_path <- function() {
  .cmdstanr$PATH
}

#' @rdname cmdstan_path
#' @export
#' @param full_path The full file path to the CmdStan installation as a string.
set_cmdstan_path <- function(path) {
  if (!dir.exists(path)) {
    stop("Directory does not exist.", call. = FALSE)
  }
  .cmdstanr$PATH <- path
  invisible(path)
}

# instantiate --------------------------------------------------
.cmdstanr <- new.env(parent = emptyenv())
.cmdstanr$PATH <- Sys.getenv("CMDSTAN")
.cmdstanr$EXTENSION <- if (.Platform$OS.type == "windows") ".exe" else ""


# internal ----------------------------------------------------------------
cmdstan_ext <- function(path = NULL) {
  if (is.null(path)) {
    return(.cmdstanr$EXTENSION)
  }
  paste0(path, .cmdstanr$EXTENSION)
}

add_cmdstan_path <- function(relative_path) {
  if (!nzchar(cmdstan_path())) {
    stop("Please set the path to CmdStan. See ?set_cmdstan_path.",
         call. = FALSE)
  }
  file.path(cmdstan_path(), relative_path)
}

strip_cmdstan_path <- function(full_path) {
  if (!nzchar(cmdstan_path())) {
    stop("Please set the path to CmdStan. See ?set_cmdstan_path.",
         call. = FALSE)
  }
  relative_path <- sub(cmdstan_path(), "", full_path)
  if (substr(relative_path, 1, 1) == "/") {
    relative_path <- sub("/", "", relative_path)
  }
}
