context("paths")

Sys.unsetenv("CMDSTAN")

if (not_on_cran()) {
  PATH <- absolute_path(set_cmdstan_path())
  VERSION <- cmdstan_version()
} else { # CRAN
  PATH <- absolute_path(Sys.getenv("HOME")) # not actually installed, just a valid path
}

unset_cmdstan_path()

# Setting paths -----------------------------------------------------------
test_that("Setting path works and confirms with message", {
  expect_message(
    set_cmdstan_path(PATH),
    paste("CmdStan path set to:", PATH)
  )
  expect_equal(.cmdstanr$PATH, PATH)
})

test_that("Setting bad path leads to warning (can't find directory)", {
  unset_cmdstan_path()
  expect_null(.cmdstanr$PATH)
  expect_warning(
    set_cmdstan_path("BAD_PATH"),
    "Can't find directory"
  )
})

test_that("Setting bad path from env leads to warning (can't find directory)", {
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
  Sys.setenv(CMDSTAN = PATH)
  expect_silent(cmdstanr_initialize())
  expect_equal(cmdstan_path(), PATH)
  Sys.unsetenv("CMDSTAN")
})

test_that("cmdstanr_initialize() also looks for default path", {
  skip_on_cran()

  unset_cmdstan_path()
  cmdstanr_initialize()
  expect_equal(tolower(cmdstan_path()), tolower(PATH))
})


# Getting path and version ------------------------------------------------
test_that("Getting a valid path works", {
  skip_on_cran()

  unset_cmdstan_path()
  set_cmdstan_path(PATH)
  expect_equal(cmdstan_path(), PATH)
})

test_that("Getting missing path leads to error (path not set)", {
  unset_cmdstan_path()
  expect_error(
    cmdstan_path(),
    "CmdStan path has not been set yet"
  )
  expect_null(.cmdstanr$PATH)
})

test_that("CmdStan version detected when setting path", {
  skip_on_cran()

  unset_cmdstan_path()
  set_cmdstan_path(PATH)
  expect_equal(cmdstan_path(), PATH)
  expect_equal(cmdstan_version(), VERSION)
})

test_that("Warning message is thrown if can't detect version number", {
  path <- testthat::test_path("answers") # valid path but not cmdstan
  expect_warning(
    set_cmdstan_path(path),
    "Can't find CmdStan makefile to detect version number"
  )
})

test_that("cmdstan_ext() works", {
  if (os_is_windows()) {
    expect_identical(cmdstan_ext(), ".exe")
    expect_identical(cmdstan_ext("path"), "path.exe")
  } else {
    expect_identical(cmdstan_ext(), "")
    expect_identical(cmdstan_ext("path"), "path")
  }
})

test_that("repair_path() fixes slashes", {
  # all slashes should be single "/", and no trailing slash
  expect_equal(repair_path("a//b\\c/"), "a/b/c")
})

# cleanup -----------------------------------------------------------------
Sys.unsetenv("CMDSTAN")
unset_cmdstan_path()
