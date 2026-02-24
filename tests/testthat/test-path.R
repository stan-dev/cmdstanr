context("paths")

Sys.unsetenv("CMDSTAN")
PATH <- absolute_path(set_cmdstan_path())
VERSION <- cmdstan_version()
unset_cmdstan_path()


# Setting paths -----------------------------------------------------------
test_that("Setting path works and confirms with message", {
  expect_message(
    set_cmdstan_path(PATH),
    paste("CmdStan path set to:", PATH),
    fixed = TRUE
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
  unset_cmdstan_path()
  expect_true(is.null(.cmdstanr$VERSION))
  Sys.setenv(CMDSTAN = PATH)
  expect_silent(cmdstanr_initialize())
  expect_equal(cmdstan_path(), PATH)
  expect_false(is.null(.cmdstanr$VERSION))
  Sys.unsetenv("CMDSTAN")
})

test_that("Unsupported CmdStan path from env var is rejected", {
  unset_cmdstan_path()
  parent_dir <- file.path(tempdir(check = TRUE), "cmdstan-env-parent")
  old_install <- file.path(parent_dir, "cmdstan-2.34.0")
  dir.create(old_install, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(parent_dir, recursive = TRUE), add = TRUE)
  on.exit(Sys.unsetenv("CMDSTAN"), add = TRUE)
  writeLines("CMDSTAN_VERSION := 2.34.0", con = file.path(old_install, "makefile"))

  Sys.setenv(CMDSTAN = parent_dir)
  expect_warning(
    cmdstanr_initialize(),
    "cmdstanr now requires CmdStan v2.35.0 or newer",
    fixed = TRUE
  )
  expect_null(.cmdstanr$PATH)
  expect_null(.cmdstanr$VERSION)
})

test_that("cmdstanr_initialize() also looks for default path", {
  unset_cmdstan_path()
  cmdstanr_initialize()
  expect_equal(tolower(cmdstan_path()), tolower(PATH))
})


# Getting path and version ------------------------------------------------
test_that("Getting a valid path works", {
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
  unset_cmdstan_path()
  set_cmdstan_path(PATH)
  expect_equal(cmdstan_path(), PATH)
  expect_equal(cmdstan_version(), VERSION)
})

test_that("cmdstan_version() behaves correctly when version is not set", {
  version <- .cmdstanr$VERSION
  on.exit(.cmdstanr$VERSION <- version)
  .cmdstanr$VERSION <- NULL
  expect_error(cmdstan_version())
  expect_null(cmdstan_version(error_on_NA = FALSE))
})

test_that("Warning message is thrown if can't detect version number", {
  path <- testthat::test_path("answers") # valid path but not cmdstan
  expect_warning(
    set_cmdstan_path(path),
    "Can't find CmdStan makefile to detect version number"
  )
})

test_that("Setting path rejects unsupported CmdStan versions", {
  old_path <- .cmdstanr$PATH
  old_version <- .cmdstanr$VERSION
  on.exit({
    .cmdstanr$PATH <- old_path
    .cmdstanr$VERSION <- old_version
  })

  path <- file.path(tempdir(check = TRUE), "cmdstan-2.34.0")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  writeLines("CMDSTAN_VERSION := 2.34.0", con = file.path(path, "makefile"))

  expect_warning(
    set_cmdstan_path(path),
    "cmdstanr now requires CmdStan v2.35.0 or newer",
    fixed = TRUE
  )
  expect_null(.cmdstanr$PATH)
  expect_null(.cmdstanr$VERSION)
})

test_that("cmdstan_ext() works", {
  if (os_is_windows() && !os_is_wsl()) {
    expect_identical(cmdstan_ext(), ".exe")
    expect_identical(cmdstan_ext("path"), "path.exe")
  } else {
    expect_identical(cmdstan_ext(), "")
    expect_identical(cmdstan_ext("path"), "path")
  }
})

test_that("is_release_candidate() works", {
  expect_true(is_release_candidate("cmdstan-2.23.0-rc1/"))
  expect_true(is_release_candidate("cmdstan-2.23.0-rc1"))
  expect_false(is_release_candidate("cmdstan-2.23.0"))
})

# cleanup -----------------------------------------------------------------
Sys.unsetenv("CMDSTAN")
unset_cmdstan_path()
