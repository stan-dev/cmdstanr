context("install")

test_that("install_cmdstan() successfully installs cmdstan", {
  skip_if_offline()
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }
  expect_message(
    expect_output(
      install_cmdstan(dir = dir, cores = 2, quiet = FALSE, overwrite = TRUE),
      "Compiling, linking C++ code",
      fixed = TRUE
    ),
    "CmdStan path set"
  )
})

test_that("install_cmdstan() errors if installation already exists", {
  skip_if_offline()
  ver <- latest_released_version()
  if (not_on_cran()) {
    # want to test passing NULL to install_cmdstan but need a real dir to
    # check in dir.exists() below so also create dir_check
    install_dir <- cmdstan_default_install_path()
  } else {
    install_dir <- tempdir()
  }
  dir <- file.path(install_dir, paste0("cmdstan-",ver))
  fake_folder <- FALSE
  if (!dir.exists(dir)) {
    fake_folder <- TRUE
    dir.create(dir)
  }
  expect_warning(
    install_cmdstan(dir = install_dir, overwrite = FALSE),
    "An installation already exists"
  )
})

test_that("install_cmdstan() errors if it times out", {
  skip_if_offline()
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }
  ver <- latest_released_version()
  dir_exists <- dir.exists(file.path(dir, paste0("cmdstan-",ver)))
  # with quiet=TRUE
  expect_warning(
    expect_message(
      install_cmdstan(dir = dir, timeout = 1, quiet = TRUE, overwrite = dir_exists),
      if (dir_exists) "* Removing the existing installation" else "* Latest CmdStan release",
      fixed = TRUE
    ),
    "increasing the value of the 'timeout' argument and running again with 'quiet=FALSE'",
    fixed = TRUE
  )
  dir_exists <- dir.exists(file.path(dir, paste0("cmdstan-",ver)))
  # with quiet=FALSE
  expect_warning(
    expect_message(
      install_cmdstan(dir = dir, timeout = 1, quiet = FALSE, overwrite = dir_exists),
      if (dir_exists) "* Removing the existing installation" else "* Latest CmdStan release",
      fixed = TRUE
    ),
    "Try increasing the value of the 'timeout' argument.",
    fixed = TRUE
  )
})
