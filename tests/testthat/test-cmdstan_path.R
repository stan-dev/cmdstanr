
Sys.unsetenv("CMDSTAN")
.cmdstanr$PATH <- NULL

test_that("Setting path throws correct message", {
  expect_message(
    set_cmdstan_path(testthat::test_path("answers")),
    paste("CmdStan path set to:", testthat::test_path("answers"))
  )
})

test_that("Setting and getting path throws correct warning", {
  expect_warning(
    set_cmdstan_path("BAD_PATH"),
    "Can't find directory"
  )
  expect_warning(
    set_cmdstan_path("BAD_PATH"),
    "Can't find directory"
  )
})

test_that("Setting path from env var throws correct warning", {
  Sys.setenv(CMDSTAN = "BAD_PATH")
  expect_warning(
    cmdstanr_initialize(),
    "Can't find directory specified by environment variable"
  )
  Sys.unsetenv("CMDSTAN")
})

.cmdstanr$PATH <- NULL

