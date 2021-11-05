context("install")

cmdstan_test_tarball_url <- Sys.getenv("CMDSTAN_TEST_TARBALL_URL")
if (!nzchar(cmdstan_test_tarball_url)) {
  cmdstan_test_tarball_url <- NULL
}

test_that("install_cmdstan() successfully installs cmdstan", {
  skip_if_offline()
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }
  expect_message(
    expect_output(
      install_cmdstan(dir = dir, cores = 2, quiet = FALSE, overwrite = TRUE,
                      release_url = cmdstan_test_tarball_url),
      "Compiling, linking C++ code",
      fixed = TRUE
    ),
    "CmdStan path set",
    fixed = TRUE
  )
})

test_that("install_cmdstan() errors if installation already exists", {
  skip_if_offline()

  if (not_on_cran()) {
    # want to test passing NULL to install_cmdstan but need a real dir to
    # check in dir.exists() below so also create dir_check
    install_dir <- cmdstan_default_install_path()
  } else {
    install_dir <- tempdir()
  }
  dir <- file.path(install_dir, "cmdstan-2.23.0")
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  expect_warning(
    install_cmdstan(dir = install_dir, overwrite = FALSE,
                    version = "2.23.0"),
    "An installation already exists",
    fixed = TRUE
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
      install_cmdstan(dir = dir, timeout = 1, quiet = TRUE, overwrite = dir_exists,
                      release_url = cmdstan_test_tarball_url),
      if (dir_exists) "* Removing the existing installation" else "* * Installing CmdStan from https://github.com",
      fixed = TRUE
    ),
    "increasing the value of the 'timeout' argument and running again with 'quiet=FALSE'",
    fixed = TRUE
  )
  dir_exists <- dir.exists(file.path(dir, paste0("cmdstan-",ver)))
  # with quiet=FALSE
  expect_warning(
    expect_message(
      install_cmdstan(dir = dir, timeout = 1, quiet = FALSE, overwrite = dir_exists,
                      release_url = cmdstan_test_tarball_url),
      if (dir_exists) "* Removing the existing installation" else "* * Installing CmdStan from https://github.com",
      fixed = TRUE
    ),
    "Try increasing the value of the 'timeout' argument.",
    fixed = TRUE
  )
})

test_that("install_cmdstan() errors if invalid version or URL", {
  skip_if_offline()
  expect_error(
    install_cmdstan(version = "2.23.2"),
    "Download of CmdStan failed. Please check if the supplied version number is valid."
  )
  expect_error(
    install_cmdstan(release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.23.2/cmdstan-2.23.2.tar.gz"),
    "Download of CmdStan failed. Please check if the supplied release URL is valid."
  )
  expect_error(
    install_cmdstan(release_url = "https://github.com/stan-dev/cmdstan/releases/tag/v2.24.0"),
    "cmdstanr supports installing from .tar.gz archives only"
  )
})

test_that("install_cmdstan() works with version and release_url", {
  skip_if_offline()
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }

  expect_message(
    expect_output(
      install_cmdstan(dir = dir, overwrite = TRUE, cores = 4,
                      release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.24.0/cmdstan-2.24.0.tar.gz"),
      "Compiling, linking C++ code",
      fixed = TRUE
    ),
    "Finished installing CmdStan",
    fixed = TRUE
  )
  expect_warning(
    expect_message(
      expect_output(
        install_cmdstan(dir = dir, overwrite = TRUE, cores = 4,
                        version = "2.23.0",
                        # the URL is intentionally invalid to test that the version has higher priority
                        release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.23.2/cmdstan-2.23.2.tar.gz"),
        "Compiling, linking C++ code",
        fixed = TRUE
      ),
      "Finished installing CmdStan",
    fixed = TRUE
    ),
    "version and release_url shouldn't both be specified",
    fixed = TRUE
  )
  expect_true(dir.exists(file.path(dir, "cmdstan-2.23.0")))
  set_cmdstan_path(cmdstan_default_path())
})

test_that("toolchain checks on Unix work", {
  skip_if(os_is_windows())
  path_backup <- Sys.getenv("PATH")
  Sys.setenv("PATH" = "")
  if (os_is_macos()) {
    err_msg_cpp <- "A suitable C++ compiler was not found. Please install the command line tools for Mac with 'xcode-select --install' or install Xcode from the app store. Then restart R and run check_cmdstan_toolchain()."
    err_msg_make <- "The 'make' tool was not found. Please install the command line tools for Mac with 'xcode-select --install' or install Xcode from the app store. Then restart R and run check_cmdstan_toolchain()."
  } else {
    err_msg_cpp <- "A C++ compiler was not found. Please install the 'clang++' or 'g++' compiler, restart R, and run check_cmdstan_toolchain()."
    err_msg_make <- "The 'make' tool was not found. Please install 'make', restart R, and then run check_cmdstan_toolchain()."
  }
  expect_error(
    check_unix_cpp_compiler(),
    err_msg_cpp,
    fixed = TRUE
  )
  expect_error(
    check_unix_make(),
    err_msg_make,
    fixed = TRUE
  )
  Sys.setenv("PATH" = path_backup)
})

test_that("toolchain checks on Windows with RTools 3.5 work", {
  skip_if_not(os_is_windows())
  skip_if(R.Version()$major > "3")

  path_backup <- Sys.getenv("PATH")
  Sys.setenv("PATH" = "")
  tmpdir <- tempdir()
  tmp_dir1 <- file.path(tmpdir, "dir1")
  tmp_dir2 <- file.path(tmpdir, "dir2")
  if (dir.exists(tmp_dir1)) unlink(tmp_dir1)
  if (dir.exists(tmp_dir2)) unlink(tmp_dir2)
  expect_error(
    check_rtools35_windows_toolchain(paths= c(tmp_dir1, tmp_dir2)),
    "\nA toolchain was not found. Please install RTools 3.5 and run",
    fixed = TRUE
  )
  if (!dir.exists(tmp_dir1)) dir.create(tmp_dir1)
  expect_error(
    check_rtools35_windows_toolchain(paths= c(tmp_dir1, tmp_dir2)),
    "\nRTools installation found but PATH was not properly set.",
    fixed = TRUE
  )
  if (!dir.exists(tmp_dir2)) dir.create(tmp_dir2)
  expect_error(
    check_rtools35_windows_toolchain(paths= c(tmp_dir1, tmp_dir2)),
    "\nMultiple RTools 3.5 installations found. Please select the installation to use",
    fixed = TRUE
  )
  Sys.setenv("PATH" = path_backup)
})

test_that("toolchain checks without fixes on Windows with RTools 4.0 work", {
  skip_if_not(os_is_windows())
  skip_if(R.Version()$major < "4")

  rtools40_home_backup <- Sys.getenv("RTOOLS40_HOME")
  Sys.setenv("RTOOLS40_HOME" = "")
  expect_error(
    check_rtools40_windows_toolchain(),
    "\nRTools 4.0 was not found but is required to run CmdStan with R version 4.x.",
    fixed = TRUE
  )

  Sys.setenv("RTOOLS40_HOME" = "C:/with spaces/")
  expect_error(
    check_rtools40_windows_toolchain(),
    "\nRTools 4.0 is installed in a path with spaces or brackets, which is not supported.",
    fixed = TRUE
  )

  Sys.setenv("RTOOLS40_HOME" = rtools40_home_backup)
  path_backup <- Sys.getenv("PATH")
  Sys.setenv("PATH" = "")
  expect_error(
    check_rtools40_windows_toolchain(),
    "\nRTools installation found but PATH was not properly set.\nRun check_cmdstan_toolchain(fix = TRUE) to fix the issue.",
    fixed = TRUE
  )

  tmpdir <- tempdir()
  gpp_location <- file.path(rtools40_home_backup, "mingw64", "bin", "g++.exe")
  file.copy(gpp_location, file.path(tmpdir, "g++.exe"))
  Sys.setenv("PATH" = paste0(tmpdir, ";", path_backup))
  expect_error(
    check_rtools40_windows_toolchain(),
    "\nOther C++ toolchains installed on your system conflict with RTools.\nPlease run check_cmdstan_toolchain(fix = TRUE) to fix the issue.",
    fixed = TRUE
  )
  Sys.setenv("PATH" = path_backup)
})

test_that("clean and rebuild works", {
  expect_output(
    rebuild_cmdstan(),
    paste0("CmdStan v", cmdstan_version(), " built"),
    fixed = TRUE
  )
})

test_that("github_download_url constructs correct url", {
  expect_equal(
    github_download_url("FOO"),
    "https://github.com/stan-dev/cmdstan/releases/download/vFOO/cmdstan-FOO.tar.gz"
  )
})
