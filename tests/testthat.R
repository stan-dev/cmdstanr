library(testthat)
library(cmdstanr)

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  options(cli.hyperlink = FALSE)
  withr::local_options(list(mc.cores = 1L))
  withr::local_envvar(TESTTHAT_IS_CHECKING = "true")

  sequential_files <- "example|model-compile|model-compile-user_header|model-methods|utils"

  withr::with_envvar(
    c(TESTTHAT_PARALLEL = "false"),
    test_dir("testthat",
             package = "cmdstanr",
             reporter = check_reporter(),
             filter = sequential_files,
             load_package = "installed")
  )

  withr::with_envvar(
    c(TESTTHAT_PARALLEL = "true"),
    test_dir("testthat",
             package = "cmdstanr",
             reporter = check_reporter(),
             filter = sequential_files,
             invert = TRUE,
             load_package = "installed")
  )
}
