.onAttach <- function(...) {
  ver <- utils::packageVersion("cmdstanr")
  packageStartupMessage("This is cmdstanr version ", ver)
  packageStartupMessage("- Online documentation and vignettes at mc-stan.org/cmdstanr")
  if (is.null(.cmdstanr$PATH)) {
    packageStartupMessage("- Use set_cmdstan_path() to set the path to CmdStan")
    packageStartupMessage("- Use install_cmdstan() to install CmdStan")
  } else {
    packageStartupMessage("- CmdStan path set to: ", cmdstan_path(), "")
    packageStartupMessage("- Use set_cmdstan_path() to change the path")
  }
}

.onLoad <- function(...) {
  cmdstanr_initialize()
}


cmdstanr_initialize <- function() {
  # First check for environment variable CMDSTAN, but if not found
  # then see if default
  path <- Sys.getenv("CMDSTAN")
  if (isTRUE(nzchar(path))) { # CMDSTAN environment variable found
    if (dir.exists(path)) {
      path <- absolute_path(path)
      suppressMessages(set_cmdstan_path(path))
    } else {
      warning("Can't find directory specified by environment variable",
              " 'CMDSTAN'. Path not set.", call. = FALSE)
      .cmdstanr$PATH <- NULL
    }

  } else { # environment variable not found
    path <- cmdstan_default_path()
    if (!is.null(path)) {
      suppressMessages(set_cmdstan_path(path))
    }
  }

  if (getRversion() < '3.5.0') {
    .cmdstanr$TEMP_DIR <- tempdir()
  } else {
    .cmdstanr$TEMP_DIR <- tempdir(check = TRUE)
  }
  invisible(TRUE)
}
