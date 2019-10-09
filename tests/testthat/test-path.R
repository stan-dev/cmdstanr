context("CmdStan path")

Sys.unsetenv("CMDSTAN")
unset_cmdstan_path()

CMDSTAN_PATH <- file.path(Sys.getenv("HOME"), ".cmdstanr/cmdstan-2.20.0")

test_that("Setting path works and confirms with message", {
  skip_on_cran()

  expect_message(
    set_cmdstan_path(CMDSTAN_PATH),
    paste("CmdStan path set to:", CMDSTAN_PATH)
  )
  expect_equal(cmdstan_path(), CMDSTAN_PATH)
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
  skip_on_cran()

  unset_cmdstan_path()
  Sys.setenv(CMDSTAN = CMDSTAN_PATH)
  expect_silent(cmdstanr_initialize())
  expect_equal(cmdstan_path(), CMDSTAN_PATH)
})

test_that("CmdStan version number detected first time calling cmdstan_path()", {
  skip_on_cran()

  unset_cmdstan_path()
  set_cmdstan_path(CMDSTAN_PATH)
  expect_equal(cmdstan_path(), CMDSTAN_PATH)
  expect_equal(cmdstan_version(), "2.20.0")
})

test_that("Warning message is thrown if can't detect version number", {
  path <- testthat::test_path("answers") # valid path but not cmdstan
  expect_warning(
    set_cmdstan_path(path),
    "Can't find CmdStan makefile to detect version number"
  )
})


# cleanup
Sys.unsetenv("CMDSTAN")
unset_cmdstan_path()
