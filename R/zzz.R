startup_messages <- function() {
  packageStartupMessage("This is cmdstanr version ", utils::packageVersion("cmdstanr"))
  packageStartupMessage("- CmdStanR documentation and vignettes: mc-stan.org/cmdstanr")
  if (is.null(.cmdstanr$PATH)) {
    packageStartupMessage("- Use set_cmdstan_path() to set the path to CmdStan")
    packageStartupMessage("- Use install_cmdstan() to install CmdStan")
  } else {
    packageStartupMessage("- CmdStan path: ", cmdstan_path())
    packageStartupMessage("- CmdStan version: ", cmdstan_version(error_on_NA = FALSE))
  }

  skip_version_check <- isTRUE(getOption(
    "CMDSTANR_NO_VER_CHECK",
    default = identical(tolower(Sys.getenv("CMDSTANR_NO_VER_CHECK")), "true")
  ))
  if (!skip_version_check) {
    latest_version <- try(suppressWarnings(latest_released_version()), silent = TRUE)
    current_version <- try(cmdstan_version(), silent = TRUE)
    if (!inherits(latest_version, "try-error")
        && !inherits(current_version, "try-error")
        && latest_version > current_version) {
      packageStartupMessage(
        "\nA newer version of CmdStan is available. See ?install_cmdstan() to install it.",
        "\nTo disable this check set option or environment variable CMDSTANR_NO_VER_CHECK=TRUE."
      )
    }
  }
}

cmdstanr_initialize <- function() {
  # First check for environment variable CMDSTAN, but if not found
  # then see if default
  path <- Sys.getenv("CMDSTAN")
  if (isTRUE(nzchar(path))) { # CMDSTAN environment variable found
    if (dir.exists(path)) {
      path <- absolute_path(path)
      suppressWarnings(suppressMessages(set_cmdstan_path(path)))
      if (is.null(cmdstan_version(error_on_NA = FALSE))) {
        path <- cmdstan_default_path(dir = path)
        if (is.null(path)) {
          warning(
            "No CmdStan installation found in the path specified ",
            "by the environment variable 'CMDSTAN'.",
            call. = FALSE
          )
          .cmdstanr$PATH <- NULL
        } else {
          set_cmdstan_path(path)
        }
      }
    } else {
      warning(
        "Can't find directory specified by environment variable 'CMDSTAN'. ",
        "Path not set.",
        call. = FALSE
      )
      .cmdstanr$PATH <- NULL
    }

  } else { # environment variable not found
    path <- cmdstan_default_path() %||% cmdstan_default_path(old = TRUE)
    if (!is.null(path)) {
      suppressMessages(set_cmdstan_path(path))
    }
  }

  if (getRversion() < "3.5.0") {
    .cmdstanr$TEMP_DIR <- tempdir()
  } else {
    .cmdstanr$TEMP_DIR <- tempdir(check = TRUE)
  }
  invisible(TRUE)
}

cmdstanr_knitr_env_function_generator <- function(env1) {
  function(env2 = NULL) {
    if (!is.null(env2)) {
      env1 <<- env2
    }
    invisible(env1)
  }
}
cmdstanr_knitr_env <- cmdstanr_knitr_env_function_generator(new.env())


.onAttach <- function(...) {
  startup_messages()
}

.onLoad <- function(...) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    cmdstanr_knitr_env(knitr::knit_global())
  }
  cmdstanr_initialize()
}
