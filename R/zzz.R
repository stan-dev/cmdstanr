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
    "cmdstanr_no_ver_check",
    default = identical(tolower(Sys.getenv("cmdstanr_no_ver_check")), "true")
  ))
  if (!skip_version_check) {
    deprecated_no_ver_check_option <- getOption("CMDSTANR_NO_VER_CHECK")
    deprecated_no_ver_check_env <- Sys.getenv(
      "CMDSTANR_NO_VER_CHECK",
      unset = NA_character_
    )
    if (!is.null(deprecated_no_ver_check_option) ||
        !is.na(deprecated_no_ver_check_env)) {
      warning(
        "The 'CMDSTANR_NO_VER_CHECK' option and environment variable are ",
        "deprecated as of CmdStanR 1.0.0 and will be removed in a future ",
        "release. Use lowercase 'cmdstanr_no_ver_check' instead.",
        call. = FALSE
      )
    }
    # fall back to the deprecated all-caps setting
    skip_version_check <- isTRUE(
      deprecated_no_ver_check_option %||%
        identical(tolower(deprecated_no_ver_check_env), "true")
    )
  }
  if (!skip_version_check) {
    latest_version <- try(suppressWarnings(latest_released_version(retries = 0)), silent = TRUE)
    current_version <- try(cmdstan_version(), silent = TRUE)
    if (!inherits(latest_version, "try-error")
        && !inherits(current_version, "try-error")
        && cmdstan_version_compare(latest_version, current_version) > 0) {
      packageStartupMessage(
        "\nA newer version of CmdStan is available. See ?install_cmdstan() to install it.",
        "\nTo disable this check set option or environment variable cmdstanr_no_ver_check=TRUE."
      )
    }
  }
}

cmdstanr_initialize <- function() {
  suppressMessages(set_cmdstan_path())
  .cmdstanr$TEMP_DIR <- tempdir(check = TRUE)
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
  # avoid conflict between parallel and processx
  Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD = 1)
  if (requireNamespace("knitr", quietly = TRUE)) {
    cmdstanr_knitr_env(knitr::knit_global())
  }
  cmdstanr_initialize()
}
