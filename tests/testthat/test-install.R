# avoid parallel on Mac due to strange intermittent TBB errors on Github Actions
CORES <- if (os_is_macos()) 1 else 2

cmdstan_test_tarball_url <- Sys.getenv("CMDSTAN_TEST_TARBALL_URL")
if (!nzchar(cmdstan_test_tarball_url)) {
  cmdstan_test_tarball_url <- NULL
}

test_that("install_cmdstan() successfully installs cmdstan", {
  dir <- tempdir(check = TRUE)
  expect_message(
    expect_output(
      install_cmdstan(dir = dir, cores = CORES, quiet = FALSE, overwrite = TRUE,
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
  skip_if(!is.null(cmdstan_test_tarball_url))

  dir <- tempdir(check = TRUE)
  ver <- latest_released_version()
  dir_exists <- dir.exists(file.path(dir, paste0("cmdstan-",ver)))
  # with quiet=TRUE
  expect_warning(
    expect_message(
      install_cmdstan(dir = dir, timeout = 1, quiet = TRUE, overwrite = dir_exists,
                      cores = CORES, wsl = os_is_wsl()),
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
                      cores = CORES, wsl = os_is_wsl()),
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
  # this test is irrelevant if tests are using a release candidate tarball URL so skip
  skip_if(!is.null(cmdstan_test_tarball_url))

  dir <- tempdir(check = TRUE)

  expect_message(
    expect_output(
      install_cmdstan(dir = dir, overwrite = TRUE, cores = CORES,
                      release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.37.0/cmdstan-2.37.0.tar.gz",
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
        install_cmdstan(dir = dir, overwrite = TRUE, cores = CORES,
                        version = "2.37.0",
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
  expect_true(dir.exists(file.path(dir, "cmdstan-2.37.0")))
  set_cmdstan_path()
})

test_that("toolchain checks on Unix work", {
  skip_if(os_is_windows())
  withr::local_envvar(c("PATH" = ""))
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
})

test_that("clean and rebuild works", {
  set_cmdstan_path()
  expect_output(
    rebuild_cmdstan(cores = CORES),
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

test_that("extract_cmdstan_version_from_archive_name parses realistic inputs", {
  expect_equal(
    extract_cmdstan_version_from_archive_name(
      "https://github.com/stan-dev/cmdstan/releases/download/v2.37.0/cmdstan-2.37.0.tar.gz"
    ),
    "2.37.0"
  )
  expect_equal(
    extract_cmdstan_version_from_archive_name(
      "https://github.com/stan-dev/cmdstan/releases/download/v2.37.0/cmdstan-2.37.0-linux-arm64.tar.gz"
    ),
    "2.37.0"
  )
  expect_equal(
    extract_cmdstan_version_from_archive_name(
      "https://github.com/stan-dev/cmdstan/releases/download/v2.35.0-rc1/cmdstan-2.35.0-rc1.tar.gz?download=1"
    ),
    "2.35.0-rc1"
  )
  expect_equal(
    extract_cmdstan_version_from_archive_name(
      file.path(tempdir(check = TRUE), "cmdstan-2.35.1-linux-s390x.tar.gz")
    ),
    "2.35.1"
  )
  expect_null(
    extract_cmdstan_version_from_archive_name(
      "https://github.com/stan-dev/cmdstan/releases/tag/v2.37.0"
    )
  )
})

test_that("Downloads respect quiet argument", {
  dir <- tempdir(check = TRUE)
  version <- latest_released_version()

  ver_msg <- "trying URL 'https://api.github.com/repos/stan-dev/cmdstan/releases/latest'"
  download_msg <- paste0("trying URL 'https://github.com/stan-dev/cmdstan/releases/download/v",
                         version, "/cmdstan-", version, ".tar.gz'")

  # expect_message has trouble capturing the messages from download.file
  # so handle manually
  install_normal <- suppressWarnings(
    capture.output(install_cmdstan(dir = dir, overwrite = TRUE, quiet = FALSE, cores = CORES),
                   type = "message")
  )
  install_quiet <- suppressWarnings(
    capture.output(install_cmdstan(dir = dir, overwrite = TRUE, quiet = TRUE, cores = CORES),
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

  dir <- tempdir(check = TRUE)

  expect_error({
    # Use an invalid proxy address to force a download failure
    withr::with_envvar(
      c("http_proxy"="invalid","https_proxy"="invalid"),
      install_cmdstan(dir = dir, overwrite = TRUE)
    )},
    "GitHub download of release list failed with error: cannot open URL 'https://api.github.com/repos/stan-dev/cmdstan/releases/latest'")
})

test_that("Install from release file works", {
  dir <- tempdir(check = TRUE)

  destfile <- file.path(dir, "cmdstan-2.37.0.tar.gz")

  download_with_retries(
    "https://github.com/stan-dev/cmdstan/releases/download/v2.37.0/cmdstan-2.37.0.tar.gz",
    destfile)

  expect_message(
    expect_output(
      install_cmdstan(dir = dir, cores = CORES, quiet = FALSE, overwrite = TRUE,
                      release_file = destfile,
                      wsl = os_is_wsl()),
      "Compiling C++ code",
      fixed = TRUE
    ),
    "CmdStan path set",
    fixed = TRUE
  )
})

test_that("install_cmdstan() errors for unsupported CmdStan versions", {
  expect_error(
    install_cmdstan(version = "2.34.0", check_toolchain = FALSE, wsl = os_is_wsl()),
    "Requested CmdStan version (2.34.0) is unsupported.",
    fixed = TRUE
  )
  expect_error(
    install_cmdstan(
      release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.34.0/cmdstan-2.34.0.tar.gz",
      check_toolchain = FALSE,
      wsl = os_is_wsl()
    ),
    "Requested CmdStan release_url/release_file (2.34.0) is unsupported.",
    fixed = TRUE
  )
  expect_error(
    install_cmdstan(
      release_file = file.path(tempdir(check = TRUE), "cmdstan-2.34.0.tar.gz"),
      check_toolchain = FALSE,
      wsl = os_is_wsl()
    ),
    "Requested CmdStan release_url/release_file (2.34.0) is unsupported.",
    fixed = TRUE
  )
})

test_that("unsupported release-candidate versions are rejected by the floor check", {
  expect_false(is_supported_cmdstan_version("2.34.0-rc1"))
  expect_true(is_supported_cmdstan_version("2.35.0-rc1"))
  expect_error(
    install_cmdstan(version = "2.34.0-rc1", check_toolchain = FALSE, wsl = os_is_wsl()),
    "Requested CmdStan version (2.34.0-rc1) is unsupported.",
    fixed = TRUE
  )
})

test_that("deprecated CMDSTANR_USE_MSYS_TOOLCHAIN is ignored with warning", {
  old_flag <- .cmdstanr$WARNED_IGNORED_MSYS_TOOLCHAIN
  on.exit(.cmdstanr$WARNED_IGNORED_MSYS_TOOLCHAIN <- old_flag)

  .cmdstanr$WARNED_IGNORED_MSYS_TOOLCHAIN <- FALSE
  withr::with_envvar(c(CMDSTANR_USE_MSYS_TOOLCHAIN = "true"), {
    expect_warning(
      make_cmd(),
      "CMDSTANR_USE_MSYS_TOOLCHAIN",
      fixed = TRUE
    )
    expect_silent(make_cmd())
  })
})

test_that("check_cmdstan_toolchain(fix = TRUE) is deprecated", {
  expect_snapshot(
    check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
  )
})

# Windows toolchain discovery tests ----------------------------------------

test_that("toolchain_PATH_env_var() returns NULL on non-Windows", {
  old_cache <- .cmdstanr$TOOLCHAIN_PATH
  on.exit(.cmdstanr$TOOLCHAIN_PATH <- old_cache)

  .cmdstanr$TOOLCHAIN_PATH <- NULL
  local_mocked_bindings(os_is_windows = function() FALSE)
  expect_null(toolchain_PATH_env_var())
})

test_that("toolchain_PATH_env_var() caches result after first call", {
  skip_if(!os_is_windows())

  old_cache <- .cmdstanr$TOOLCHAIN_PATH
  on.exit(.cmdstanr$TOOLCHAIN_PATH <- old_cache)

  .cmdstanr$TOOLCHAIN_PATH <- NULL

  # First call should populate the cache
  first_result <- toolchain_PATH_env_var()
  expect_identical(.cmdstanr$TOOLCHAIN_PATH, first_result)

  # Second call should return cached value without re-running lookup
  second_result <- toolchain_PATH_env_var()
  expect_identical(second_result, first_result)
})

test_that("toolchain_PATH_env_var() uses RTOOLS40_HOME for R < 4.2", {
  skip_if(!os_is_windows())

  old_cache <- .cmdstanr$TOOLCHAIN_PATH
  on.exit(.cmdstanr$TOOLCHAIN_PATH <- old_cache)

  fake_home <- utils::shortPathName(withr::local_tempdir(pattern = "rtools40-home-"))
  fake_cpp_dir <- file.path(fake_home, "mingw64", "bin")
  fake_bin_dir <- file.path(fake_home, "usr", "bin")

  # Create the expected directory structure for R 4.0/4.1
  dir.create(fake_cpp_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fake_bin_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fake_cpp_dir, "c++.exe"))
  file.create(file.path(fake_bin_dir, "make.exe"))

  .cmdstanr$TOOLCHAIN_PATH <- NULL
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("4.1.0"),
      repair_path = function(path) path
    )
    withr::local_envvar(c(RTOOLS40_HOME = fake_home, R_ARCH = "/x64"))
    result <- toolchain_PATH_env_var()
    expect_false(is.null(result))
    expect_identical(
      result,
      paste0(repair_path(utils::shortPathName(c(fake_bin_dir, fake_cpp_dir))), collapse = ";")
    )
  })

  fake_cpp_dir <- file.path(fake_home, "mingw32", "bin")
  dir.create(fake_cpp_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fake_cpp_dir, "c++.exe"))

  .cmdstanr$TOOLCHAIN_PATH <- NULL
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("4.1.0"),
      repair_path = function(path) path
    )
    withr::local_envvar(c(RTOOLS40_HOME = fake_home, R_ARCH = "/i386"))
    result <- toolchain_PATH_env_var()
    expect_false(is.null(result))
    expect_identical(
      result,
      paste0(repair_path(utils::shortPathName(c(fake_bin_dir, fake_cpp_dir))), collapse = ";")
    )
  })
})

test_that("toolchain_PATH_env_var() falls back to Sys.which() when Rcmd fails", {
  skip_if(!os_is_windows())
  old_cache <- .cmdstanr$TOOLCHAIN_PATH

  on.exit(.cmdstanr$TOOLCHAIN_PATH <- old_cache)

  fake_bin <- withr::local_tempdir(pattern = "rtools-fallback-")
  file.create(file.path(fake_bin, "make.exe"))
  file.create(file.path(fake_bin, "c++.exe"))

  .cmdstanr$TOOLCHAIN_PATH <- NULL
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("4.2.0"),
      repair_path = function(path) path
    )
    # Mock .cmdstanr_rcmd to simulate a failure
    local_mocked_bindings(
      .cmdstanr_rcmd = function(..., stdout = FALSE) stop("Rcmd not found")
    )
    withr::local_envvar(c(PATH = fake_bin))
    result <- toolchain_PATH_env_var()
    # Should fall back to Sys.which() and find the fake binaries
    expect_false(is.null(result))
    paste0(repair_path(utils::shortPathName(c(fake_bin, fake_bin))), collapse = ";")
  })
})

test_that("toolchain_PATH_env_var() returns NULL when both approaches fail", {
  skip_if(!os_is_windows())

  old_cache <- .cmdstanr$TOOLCHAIN_PATH
  on.exit(.cmdstanr$TOOLCHAIN_PATH <- old_cache)

  .cmdstanr$TOOLCHAIN_PATH <- NULL
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("4.2.0"),
      repair_path = function(path) path
    )
    # Mock .cmdstanr_rcmd to return empty string
    local_mocked_bindings(
      .cmdstanr_rcmd = function(..., stdout = FALSE) ""
    )
    withr::local_envvar(c(PATH = ""))
    result <- toolchain_PATH_env_var()
    expect_null(result)
  })
})

test_that("toolchain_PATH_env_var() returns NULL when only one tool in PATH", {
  skip_if(!os_is_windows())

  old_cache <- .cmdstanr$TOOLCHAIN_PATH
  on.exit(.cmdstanr$TOOLCHAIN_PATH <- old_cache)

  fake_bin <- withr::local_tempdir(pattern = "rtools-partial-")
  file.create(file.path(fake_bin, "make"))

  .cmdstanr$TOOLCHAIN_PATH <- NULL
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("4.2.0"),
      repair_path = function(path) path
    )
    # Mock .cmdstanr_rcmd to return empty (triggers fallback)
    local_mocked_bindings(
      .cmdstanr_rcmd = function(..., stdout = FALSE) ""
    )
    withr::local_envvar(c(PATH = fake_bin))
    result <- toolchain_PATH_env_var()
    # Should return NULL because c++ was not found
    expect_null(result)
  })
})

test_that("toolchain_PATH_env_var() falls back to PATH when executables missing at R_TOOLS_SOFT", {
  skip_if(!os_is_windows())

  old_cache <- .cmdstanr$TOOLCHAIN_PATH
  on.exit(.cmdstanr$TOOLCHAIN_PATH <- old_cache)

  fake_soft <- withr::local_tempdir(pattern = "rtools-soft-")
  dir.create(file.path(fake_soft, "bin"), recursive = TRUE, showWarnings = FALSE)
  # Note: no make.exe or c++.exe created

  fake_bin <- withr::local_tempdir(pattern = "rtools-path-")
  file.create(file.path(fake_bin, "make.exe"))
  file.create(file.path(fake_bin, "c++.exe"))

  .cmdstanr$TOOLCHAIN_PATH <- NULL
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("4.2.0"),
      repair_path = function(path) path
    )
    # Rcmd returns a valid path, but executables don't exist there
    local_mocked_bindings(
      .cmdstanr_rcmd = function(..., stdout = FALSE) fake_soft
    )
    withr::local_envvar(c(PATH = fake_bin))
    result <- toolchain_PATH_env_var()
    # Should fall back to PATH and find the tools there
    expect_false(is.null(result))
    paste0(repair_path(utils::shortPathName(c(fake_bin, fake_bin))), collapse = ";")
  })
})

test_that("check_rtools4x_windows_toolchain() stops when no toolchain found", {
  skip_if(!os_is_windows())

  local_mocked_bindings(toolchain_PATH_env_var = function() NULL)
  expect_error(
    check_rtools4x_windows_toolchain(),
    "No C++ toolchain was found",
    fixed = TRUE
  )
})

test_that("is_ucrt_toolchain() returns correct values for R versions", {
  skip_if(!os_is_windows())

  # is_ucrt_toolchain() is TRUE for R 4.2.x – 4.x.x on Windows
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("4.2.0")
    )
    expect_true(is_ucrt_toolchain())
  })
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("4.4.0")
    )
    expect_true(is_ucrt_toolchain())
  })
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("4.1.0")
    )
    expect_false(is_ucrt_toolchain())
  })
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      current_r_version = function() numeric_version("5.0.0")
    )
    expect_false(is_ucrt_toolchain())
  })
  local({
    local_mocked_bindings(os_is_windows = function() FALSE)
    expect_false(is_ucrt_toolchain())
  })
})
