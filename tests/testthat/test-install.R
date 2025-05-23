context("install")

cmdstan_test_tarball_url <- Sys.getenv("CMDSTAN_TEST_TARBALL_URL")
if (!nzchar(cmdstan_test_tarball_url)) {
  cmdstan_test_tarball_url <- NULL
}

test_that("install_cmdstan() successfully installs cmdstan", {
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }
  expect_message(
    expect_output(
      install_cmdstan(dir = dir, cores = 2, quiet = FALSE, overwrite = TRUE,
                      release_url = cmdstan_test_tarball_url,
                      wsl = os_is_wsl()),
      "Compiling C++ code",
      fixed = TRUE
    ),
    "CmdStan path set",
    fixed = TRUE
  )
})

test_that("install_cmdstan() errors if installation already exists", {
  install_dir <- cmdstan_default_install_path()
  dir <- file.path(install_dir, "cmdstan-2.35.0")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  expect_warning(
    install_cmdstan(dir = install_dir, overwrite = FALSE,
                    version = "2.35.0", wsl = FALSE),
    "An installation already exists",
    fixed = TRUE
  )
})

test_that("install_cmdstan() errors if it times out", {
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
                      release_url = cmdstan_test_tarball_url, wsl = os_is_wsl()),
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
                      release_url = cmdstan_test_tarball_url,
                      wsl = os_is_wsl()),
      if (dir_exists) "* Removing the existing installation" else "* * Installing CmdStan from https://github.com",
      fixed = TRUE
    ),
    "Try increasing the value of the 'timeout' argument.",
    fixed = TRUE
  )
})

test_that("install_cmdstan() errors if invalid version or URL", {
  expect_error(
    install_cmdstan(version = "2.35.5", wsl = os_is_wsl()),
    "Download of CmdStan failed with error: cannot open URL 'https://github.com/stan-dev/cmdstan/releases/download/v2.35.5/cmdstan-2.35.5.tar.gz'\nPlease check if the supplied version number is valid."
  )
  expect_error(
    install_cmdstan(release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.35.5/cmdstan-2.35.5.tar.gz",
                    wsl = os_is_wsl()),
    "Download of CmdStan failed with error: cannot open URL 'https://github.com/stan-dev/cmdstan/releases/download/v2.35.5/cmdstan-2.35.5.tar.gz'\nPlease check if the supplied release URL is valid."
  )
  expect_error(
    install_cmdstan(release_url = "https://github.com/stan-dev/cmdstan/releases/tag/v2.24.0", wsl = os_is_wsl()),
    "cmdstanr supports installing from .tar.gz archives only"
  )
})

test_that("install_cmdstan() works with version and release_url", {
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }

  expect_message(
    expect_output(
      install_cmdstan(dir = dir, overwrite = TRUE, cores = 4,
                      release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.35.0/cmdstan-2.35.0.tar.gz",
                      wsl = os_is_wsl()),
      "Compiling C++ code",
      fixed = TRUE
    ),
    "Finished installing CmdStan",
    fixed = TRUE
  )
  expect_warning(
    expect_message(
      expect_output(
        install_cmdstan(dir = dir, overwrite = TRUE, cores = 4,
                        version = "2.35.0",
                        # the URL is intentionally invalid to test that the version has higher priority
                        release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.27.3/cmdstan-2.27.3.tar.gz",
                        wsl = os_is_wsl()),
        "Compiling C++ code",
        fixed = TRUE
      ),
      "Finished installing CmdStan",
    fixed = TRUE
    ),
    "version and release_url shouldn't both be specified",
    fixed = TRUE
  )
  expect_true(dir.exists(file.path(dir, "cmdstan-2.35.0")))
  set_cmdstan_path(cmdstan_default_path())
})

test_that("toolchain checks on Unix work", {
  skip_if(os_is_windows())
  path_backup <- Sys.getenv("PATH")
  Sys.setenv("PATH" = "")
  if (os_is_macos()) {
    err_msg_cpp <- "A suitable C++ compiler was not found. Please install the command line tools for Mac with 'xcode-select --install' or install Xcode from the app store. Then restart R and run cmdstanr::check_cmdstan_toolchain()."
    err_msg_make <- "The 'make' tool was not found. Please install the command line tools for Mac with 'xcode-select --install' or install Xcode from the app store. Then restart R and run cmdstanr::check_cmdstan_toolchain()."
  } else {
    err_msg_cpp <- "A C++ compiler was not found. Please install the 'clang++' or 'g++' compiler, restart R, and run cmdstanr::check_cmdstan_toolchain()."
    err_msg_make <- "The 'make' tool was not found. Please install 'make', restart R, and then run cmdstanr::check_cmdstan_toolchain()."
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
  skip_if(os_is_wsl())
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

test_that("Downloads respect quiet argument", {
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }
  version <- latest_released_version()

  ver_msg <- "trying URL 'https://api.github.com/repos/stan-dev/cmdstan/releases/latest'"
  download_msg <- paste0("trying URL 'https://github.com/stan-dev/cmdstan/releases/download/v",
                         version, "/cmdstan-", version, ".tar.gz'")

  # expect_message has trouble capturing the messages from download.file
  # so handle manually
  install_normal <- suppressWarnings(
    capture.output(install_cmdstan(dir = dir, overwrite = TRUE, quiet = FALSE),
                   type = "message")
  )
  install_quiet <- suppressWarnings(
    capture.output(install_cmdstan(dir = dir, overwrite = TRUE, quiet = TRUE),
                   type = "message")
  )

  expect_true(any(grepl(ver_msg, install_normal, fixed = TRUE)))
  expect_true(any(grepl(download_msg, install_normal, fixed = TRUE)))

  expect_false(any(grepl(ver_msg, install_quiet, fixed = TRUE)))
  expect_false(any(grepl(download_msg, install_quiet, fixed = TRUE)))
})

test_that("Download failures return error message", {
  # GHA fails on Windows old-rel here, but cannot replicate locally
  skip_if(os_is_windows() && getRversion() < '4.2')

  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }

  expect_error({
    # Use an invalid proxy address to force a download failure
    withr::with_envvar(
      c("http_proxy"="invalid","https_proxy"="invalid"),
      install_cmdstan(dir = dir, overwrite = TRUE)
    )},
    "GitHub download of release list failed with error: cannot open URL 'https://api.github.com/repos/stan-dev/cmdstan/releases/latest'")
})

test_that("Install from release file works", {
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }

  destfile = file.path(dir, "cmdstan-2.35.0.tar.gz")

  download_with_retries(
    "https://github.com/stan-dev/cmdstan/releases/download/v2.35.0/cmdstan-2.35.0.tar.gz",
    destfile)

  expect_message(
    expect_output(
      install_cmdstan(dir = dir, cores = 2, quiet = FALSE, overwrite = TRUE,
                      release_file = destfile,
                      wsl = os_is_wsl()),
      "Compiling C++ code",
      fixed = TRUE
    ),
    "CmdStan path set",
    fixed = TRUE
  )
})
