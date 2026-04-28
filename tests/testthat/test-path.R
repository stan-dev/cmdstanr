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
  .cmdstanr$WSL <- TRUE
  withr::local_envvar(c(CMDSTAN = "BAD_PATH"))
  expect_warning(
    cmdstanr_initialize(),
    "Can't find directory specified by environment variable"
  )
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

test_that("set_cmdstan_path() uses CMDSTAN env var when path is omitted", {
  unset_cmdstan_path()
  withr::local_envvar(c(CMDSTAN = PATH))
  expect_message(
    set_cmdstan_path(),
    paste("CmdStan path set to:", PATH),
    fixed = TRUE
  )
  expect_equal(cmdstan_path(), PATH)
})

test_that("set_cmdstan_path() keeps cached state when no path is detected", {
  .cmdstanr$PATH <- PATH
  .cmdstanr$VERSION <- VERSION
  .cmdstanr$WSL <- TRUE
  local_mocked_bindings(
    resolve_cmdstan_path_from_env = function() NULL,
    cmdstan_default_path = function(dir = NULL) NULL
  )
  expect_silent(set_cmdstan_path())
  expect_equal(.cmdstanr$PATH, PATH)
  expect_equal(.cmdstanr$VERSION, VERSION)
  expect_identical(.cmdstanr$WSL, TRUE)
})

test_that("Unsupported CmdStan path from env var is rejected", {
  unset_cmdstan_path()
  .cmdstanr$WSL <- TRUE
  parent_dir <- withr::local_tempdir(pattern = "cmdstan-env-parent")
  old_install <- file.path(parent_dir, "cmdstan-2.34.0")
  dir.create(old_install, recursive = TRUE, showWarnings = FALSE)
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
  empty_parent <- withr::local_tempdir(pattern = "cmdstan-empty-parent")

  withr::local_envvar(c(CMDSTAN = empty_parent))
  expect_warning(
    cmdstanr_initialize(),
    "CmdStan path not set. No CmdStan installation found in the path specified by the environment variable 'CMDSTAN'.",
    fixed = TRUE
  )
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
  path <- withr::local_tempdir() # valid path but not cmdstan
  expect_warning(
    set_cmdstan_path(path),
    "Can't find CmdStan makefile to detect version number"
  )
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

  path <- file.path(withr::local_tempdir(pattern = "cmdstan-unsupported"), "cmdstan-2.34.0")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  writeLines("CMDSTAN_VERSION := 2.34.0", con = file.path(path, "makefile"))

  expect_warning(
    set_cmdstan_path(path),
    "cmdstanr now requires CmdStan v2.35.0 or newer",
    fixed = TRUE
  )
  expect_null(.cmdstanr$PATH)
  expect_null(.cmdstanr$VERSION)
  expect_false(isTRUE(.cmdstanr$WSL))
})

test_that("Explicit legacy cmdstan directory can still be set", {
  unset_cmdstan_path()
  legacy_install <- file.path(withr::local_tempdir(pattern = "cmdstan-legacy"), "cmdstan")
  dir.create(legacy_install, recursive = TRUE, showWarnings = FALSE)
  writeLines("CMDSTAN_VERSION := 2.38.0", con = file.path(legacy_install, "makefile"))

  expect_message(
    set_cmdstan_path(legacy_install),
    paste("CmdStan path set to:", absolute_path(legacy_install)),
    fixed = TRUE
  )
  expect_equal(cmdstan_path(), absolute_path(legacy_install))
  expect_equal(cmdstan_version(), "2.38.0")
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
  installs <- withr::local_tempdir(pattern = "cmdstan-custom-installs")
  dir.create(file.path(installs, "cmdstan-2.35.0"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(installs, "cmdstan-2.36.0"), recursive = TRUE, showWarnings = FALSE)

  expect_equal(
    cmdstan_default_path(dir = installs),
    file.path(installs, "cmdstan-2.36.0")
  )
})

test_that("cmdstan_default_path() returns NULL for empty custom install directories", {
  installs <- withr::local_tempdir(pattern = "cmdstan-empty-installs")

  expect_null(cmdstan_default_path(dir = installs))
})

test_that("cmdstan_default_path() ignores unversioned cmdstan directory", {
  installs <- withr::local_tempdir(pattern = "cmdstan-legacy-installs")
  dir.create(file.path(installs, "cmdstan"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(installs, "cmdstan-2.36.0"), recursive = TRUE, showWarnings = FALSE)

  expect_equal(
    cmdstan_default_path(dir = installs),
    file.path(installs, "cmdstan-2.36.0")
  )
  expect_true(dir.exists(file.path(installs, "cmdstan")))
})

test_that("cmdstan_default_path() returns NULL for legacy-only cmdstan directory", {
  installs <- withr::local_tempdir(pattern = "cmdstan-legacy-only")
  legacy_install <- file.path(installs, "cmdstan")
  dir.create(legacy_install, recursive = TRUE, showWarnings = FALSE)
  writeLines("CMDSTAN_VERSION := 2.38.0", con = file.path(legacy_install, "makefile"))

  expect_null(cmdstan_default_path(dir = installs))
  expect_true(dir.exists(legacy_install))
})

test_that("CmdStan version helpers handle invalid inputs", {
  expect_identical(cmdstan_min_version(), "2.35.0")
  expect_false(is_supported_cmdstan_version(NULL))
  expect_false(is_supported_cmdstan_version("not-a-version"))
})

test_that("CmdStan version can be recovered from WSL UNC install path", {
  wsl_path <- "//wsl$/Ubuntu-22.04/root/.cmdstan/cmdstan-2.38.0"

  expect_true(is_wsl_unc_path(wsl_path))
  expect_equal(cmdstan_version_from_path(wsl_path), "2.38.0")
  expect_equal(cmdstan_version_from_path(paste0(wsl_path, "/")), "2.38.0")
  expect_equal(suppressWarnings(read_cmdstan_version(wsl_path)), "2.38.0")
  expect_null(cmdstan_version_from_path("//wsl$/Ubuntu-22.04/root/.cmdstan/not-cmdstan"))
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
