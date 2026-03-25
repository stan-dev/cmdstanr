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
  expect_snapshot_warning(set_cmdstan_path("BAD_PATH"))
})

test_that("Setting bad path from env leads to warning (can't find directory)", {
  unset_cmdstan_path()
  .cmdstanr$WSL <- TRUE
  withr::local_envvar(c(CMDSTAN = "BAD_PATH"))
  expect_snapshot_warning(cmdstanr_initialize())
  expect_null(.cmdstanr$PATH)
  expect_null(.cmdstanr$VERSION)
  expect_false(isTRUE(.cmdstanr$WSL))
})

test_that("Setting path from env var is detected", {
  unset_cmdstan_path()
  expect_true(is.null(.cmdstanr$VERSION))
  withr::local_envvar(c(CMDSTAN = PATH))
  expect_silent(cmdstanr_initialize())
  expect_equal(cmdstan_path(), PATH)
  expect_false(is.null(.cmdstanr$VERSION))
})

test_that("Unsupported CmdStan path from env var is rejected", {
  unset_cmdstan_path()
  .cmdstanr$WSL <- TRUE
  parent_dir <- file.path(tempdir(check = TRUE), "cmdstan-env-parent")
  old_install <- file.path(parent_dir, "cmdstan-2.34.0")
  dir.create(old_install, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(parent_dir, recursive = TRUE), add = TRUE)
  writeLines("CMDSTAN_VERSION := 2.34.0", con = file.path(old_install, "makefile"))

  withr::local_envvar(c(CMDSTAN = parent_dir))
  suppressWarnings(cmdstanr_initialize())
  expect_false(identical(.cmdstanr$PATH, absolute_path(old_install)))
  expect_false(identical(.cmdstanr$VERSION, "2.34.0"))
  if (!is.null(.cmdstanr$VERSION)) {
    expect_true(is_supported_cmdstan_version(.cmdstanr$VERSION))
  }
  expect_false(isTRUE(.cmdstanr$WSL))
})

test_that("Existing CMDSTAN env path with no install resets cached state", {
  unset_cmdstan_path()
  .cmdstanr$PATH <- PATH
  .cmdstanr$VERSION <- VERSION
  .cmdstanr$WSL <- TRUE
  empty_parent <- file.path(tempdir(check = TRUE), "cmdstan-empty-parent")
  dir.create(empty_parent, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(empty_parent, recursive = TRUE), add = TRUE)

  withr::local_envvar(c(CMDSTAN = empty_parent))
  expect_snapshot_warning(cmdstanr_initialize())
  expect_null(.cmdstanr$PATH)
  expect_null(.cmdstanr$VERSION)
  expect_false(isTRUE(.cmdstanr$WSL))
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
  expect_snapshot_error(cmdstan_path())
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
  expect_snapshot_error(cmdstan_version())
  expect_null(cmdstan_version(error_on_NA = FALSE))
})

test_that("Warning message is thrown if can't detect version number", {
  path <- testthat::test_path("answers") # valid path but not cmdstan
  expect_snapshot_warning(set_cmdstan_path(path))
})

test_that("Setting path rejects unsupported CmdStan versions", {
  old_path <- .cmdstanr$PATH
  old_version <- .cmdstanr$VERSION
  old_wsl <- .cmdstanr$WSL
  on.exit({
    .cmdstanr$PATH <- old_path
    .cmdstanr$VERSION <- old_version
    .cmdstanr$WSL <- old_wsl
  })

  path <- file.path(tempdir(check = TRUE), "cmdstan-2.34.0")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  writeLines("CMDSTAN_VERSION := 2.34.0", con = file.path(path, "makefile"))

  expect_snapshot_warning(set_cmdstan_path(path))
  expect_null(.cmdstanr$PATH)
  expect_null(.cmdstanr$VERSION)
  expect_false(isTRUE(.cmdstanr$WSL))
})

test_that("unset_cmdstan_path() also resets WSL state", {
  .cmdstanr$PATH <- PATH
  .cmdstanr$VERSION <- VERSION
  .cmdstanr$WSL <- TRUE
  unset_cmdstan_path()
  expect_null(.cmdstanr$PATH)
  expect_null(.cmdstanr$VERSION)
  expect_false(isTRUE(.cmdstanr$WSL))
})

test_that("cmdstan_default_path() respects custom install directories", {
  installs <- file.path(tempdir(check = TRUE), "cmdstan-custom-installs")
  dir.create(file.path(installs, "cmdstan-2.35.0"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(installs, "cmdstan-2.36.0"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(installs, recursive = TRUE), add = TRUE)

  expect_equal(
    cmdstan_default_path(dir = installs),
    file.path(installs, "cmdstan-2.36.0")
  )
})

test_that("cmdstan_default_path() returns NULL for empty custom install directories", {
  installs <- file.path(tempdir(check = TRUE), "cmdstan-empty-installs")
  dir.create(installs, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(installs, recursive = TRUE), add = TRUE)

  expect_null(cmdstan_default_path(dir = installs))
})

test_that("CmdStan version helpers handle invalid inputs", {
  expect_identical(cmdstan_min_version(), "2.35.0")
  expect_false(is_supported_cmdstan_version(NULL))
  expect_false(is_supported_cmdstan_version("not-a-version"))
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
