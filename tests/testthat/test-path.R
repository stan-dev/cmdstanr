context("CmdStan path")

Sys.unsetenv("CMDSTAN")
unset_cmdstan_path()

test_that("Setting path works and confirms with message", {
  expect_message(
    set_cmdstan_path(testthat::test_path("answers")),
    paste("CmdStan path set to:", testthat::test_path("answers"))
  )
  expect_equal(cmdstan_path(), absolute_path(testthat::test_path("answers")))
})

test_that("Setting and getting path throws correct warning/error", {
  unset_cmdstan_path()
  expect_null(.cmdstanr$PATH)
  expect_warning(
    set_cmdstan_path("BAD_PATH"),
    "Can't find directory"
  )
  expect_error(
    cmdstan_path(),
    "CmdStan path has not been set yet"
  )
  expect_null(.cmdstanr$PATH)
})

test_that("Setting path from env var throws correct warning", {
  unset_cmdstan_path()
  Sys.setenv(CMDSTAN = "BAD_PATH")
  expect_warning(
    cmdstanr_initialize(),
    "Can't find directory specified by environment variable"
  )
})

test_that("Setting path from env var is detected", {
  unset_cmdstan_path()
  Sys.setenv(CMDSTAN = testthat::test_path("answers"))
  expect_silent(cmdstanr_initialize())
  expect_equal(cmdstan_path(), absolute_path(testthat::test_path("answers")))
})

# cleanup
Sys.unsetenv("CMDSTAN")
unset_cmdstan_path()
