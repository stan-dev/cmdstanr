library(testthat)
library(cmdstanr)

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  withr::local_options(list(mc.cores = 1L))

  sequential_files <- "example|install|model-compile|model-compile-user_header|model-methods|opencl|path|threads|utils"

  withr::with_envvar(
    c(TESTTHAT_PARALLEL = "false"),
    test_dir("testthat",
             package = "cmdstanr",
             reporter = check_reporter(),
             filter = sequential_files,
             load_package = "installed")
  )

  test_dir("testthat",
           package = "cmdstanr",
           reporter = check_reporter(),
           filter = sequential_files,
           invert = TRUE,
           load_package = "installed")
}
