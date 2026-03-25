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
  expect_snapshot_error(install_cmdstan(version = "2.35.5", wsl = os_is_wsl()))
  expect_snapshot_error(
    install_cmdstan(release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.35.5/cmdstan-2.35.5.tar.gz",
                    wsl = os_is_wsl())
  )
  expect_snapshot_error(
    install_cmdstan(release_url = "https://github.com/stan-dev/cmdstan/releases/tag/v2.24.0", wsl = os_is_wsl())
  )
})

test_that("install_cmdstan() works with version and release_url", {
  # this test is irrelevant if tests are using a release candidate tarball URL so skip
  skip_if(!is.null(cmdstan_test_tarball_url))

  dir <- tempdir(check = TRUE)

  expect_message(
    expect_output(
      install_cmdstan(dir = dir, overwrite = TRUE, cores = CORES,
                      release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.36.0/cmdstan-2.36.0.tar.gz",
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
                        version = "2.36.0",
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
  expect_true(dir.exists(file.path(dir, "cmdstan-2.36.0")))
  set_cmdstan_path(cmdstan_default_path())
})

test_that("toolchain checks on Unix work", {
  skip_if(os_is_windows())
  withr::local_envvar(c("PATH" = ""))
  variant <- if (os_is_macos()) "macos" else NULL
  expect_snapshot_error(check_unix_cpp_compiler(), variant = variant)
  expect_snapshot_error(check_unix_make(), variant = variant)
})

test_that("clean and rebuild works", {
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
      "https://github.com/stan-dev/cmdstan/releases/download/v2.36.0/cmdstan-2.36.0.tar.gz"
    ),
    "2.36.0"
  )
  expect_equal(
    extract_cmdstan_version_from_archive_name(
      "https://github.com/stan-dev/cmdstan/releases/download/v2.36.0/cmdstan-2.36.0-linux-arm64.tar.gz"
    ),
    "2.36.0"
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
      "https://github.com/stan-dev/cmdstan/releases/tag/v2.36.0"
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

  destfile <- file.path(dir, "cmdstan-2.36.0.tar.gz")

  download_with_retries(
    "https://github.com/stan-dev/cmdstan/releases/download/v2.36.0/cmdstan-2.36.0.tar.gz",
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

test_that("rtools4x_toolchain_path prefers static-posix when available", {
  skip_if(arch_is_aarch64())
  env_var <- paste0(
    "RTOOLS", rtools4x_version(),
    if (arch_is_aarch64()) "_AARCH64" else "",
    "_HOME"
  )
  fake_rtools_home <- tempfile(pattern = "rtools-home-pref-", tmpdir = tempdir(check = TRUE))
  on.exit(unlink(fake_rtools_home, recursive = TRUE), add = TRUE)
  dir.create(file.path(fake_rtools_home, "x86_64-w64-mingw32.static.posix", "bin"),
             recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fake_rtools_home, "mingw64", "bin"),
             recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fake_rtools_home, "x86_64-w64-mingw32.static.posix", "bin", "g++.exe"))
  file.create(file.path(fake_rtools_home, "mingw64", "bin", "g++.exe"))

  withr::with_envvar(setNames(fake_rtools_home, env_var), {
    expect_equal(
      rtools4x_toolchain_path(),
      repair_path(file.path(fake_rtools_home, "x86_64-w64-mingw32.static.posix", "bin"))
    )
  })
})

test_that("rtools4x_toolchain_path falls back to mingw64 for legacy layouts", {
  skip_if(arch_is_aarch64())
  env_var <- paste0(
    "RTOOLS", rtools4x_version(),
    if (arch_is_aarch64()) "_AARCH64" else "",
    "_HOME"
  )
  fake_rtools_home <- tempfile(pattern = "rtools-home-fallback-", tmpdir = tempdir(check = TRUE))
  on.exit(unlink(fake_rtools_home, recursive = TRUE), add = TRUE)
  dir.create(file.path(fake_rtools_home, "mingw64", "bin"),
             recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fake_rtools_home, "mingw64", "bin", "g++.exe"))

  withr::with_envvar(setNames(fake_rtools_home, env_var), {
    expect_equal(
      rtools4x_toolchain_path(),
      repair_path(file.path(fake_rtools_home, "mingw64", "bin"))
    )
  })
})

test_that("rtools4x_toolchain_path prefers ABI-compatible legacy fallback", {
  skip_if(arch_is_aarch64())
  env_var <- paste0(
    "RTOOLS", rtools4x_version(),
    if (arch_is_aarch64()) "_AARCH64" else "",
    "_HOME"
  )
  fake_rtools_home <- tempfile(pattern = "rtools-home-abi-", tmpdir = tempdir(check = TRUE))
  on.exit(unlink(fake_rtools_home, recursive = TRUE), add = TRUE)
  dir.create(file.path(fake_rtools_home, "mingw64", "bin"),
             recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fake_rtools_home, "ucrt64", "bin"),
             recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fake_rtools_home, "mingw64", "bin", "g++.exe"))
  file.create(file.path(fake_rtools_home, "ucrt64", "bin", "g++.exe"))

  withr::with_envvar(setNames(fake_rtools_home, env_var), {
    local({
      local_mocked_bindings(is_ucrt_toolchain = function() FALSE)
      expect_equal(
        rtools4x_toolchain_path(),
        repair_path(file.path(fake_rtools_home, "mingw64", "bin"))
      )
    })
    local({
      local_mocked_bindings(is_ucrt_toolchain = function() TRUE)
      expect_equal(
        rtools4x_toolchain_path(),
        repair_path(file.path(fake_rtools_home, "ucrt64", "bin"))
      )
    })
  })
})

test_that("check_rtools4x_windows_toolchain reports checked toolchain paths", {
  env_var <- paste0(
    "RTOOLS", rtools4x_version(),
    if (arch_is_aarch64()) "_AARCH64" else "",
    "_HOME"
  )
  fake_rtools_home <- tempfile(pattern = "rtools-home-invalid-", tmpdir = tempdir(check = TRUE))
  on.exit(unlink(fake_rtools_home, recursive = TRUE), add = TRUE)
  dir.create(file.path(fake_rtools_home, "usr", "bin"),
             recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fake_rtools_home, "usr", "bin", "make.exe"))
  if (arch_is_aarch64()) {
    dir.create(file.path(fake_rtools_home, "aarch64-w64-mingw32.static.posix", "bin"),
               recursive = TRUE, showWarnings = FALSE)
  } else {
    dir.create(file.path(fake_rtools_home, "x86_64-w64-mingw32.static.posix", "bin"),
               recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(fake_rtools_home, "ucrt64", "bin"),
               recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(fake_rtools_home, "mingw64", "bin"),
               recursive = TRUE, showWarnings = FALSE)
  }

  withr::with_envvar(setNames(fake_rtools_home, env_var), {
    expect_error(
      check_rtools4x_windows_toolchain(),
      "Checked the following paths:",
      fixed = TRUE
    )
  })
})

test_that("toolchain_PATH_env_var() handles missing and configured Rtools homes", {
  local({
    local_mocked_bindings(os_is_windows = function() FALSE)
    expect_null(toolchain_PATH_env_var())
  })
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      rtools4x_home_path = function() ""
    )
    expect_null(toolchain_PATH_env_var())
  })
  local({
    local_mocked_bindings(
      os_is_windows = function() TRUE,
      rtools4x_home_path = function() "C:/rtools",
      rtools4x_toolchain_path = function() "C:/rtools/ucrt64/bin",
      repair_path = function(path) path
    )
    expect_equal(
      toolchain_PATH_env_var(),
      "C:/rtools/usr/bin;C:/rtools/ucrt64/bin"
    )
  })
})

test_that("check_rtools4x_windows_toolchain reports missing Rtools and make", {
  fake_rtools_home <- tempfile(pattern = "rtools-home-missing-", tmpdir = tempdir(check = TRUE))
  on.exit(unlink(fake_rtools_home, recursive = TRUE), add = TRUE)

  local({
    local_mocked_bindings(
      rtools4x_home_path = function() "",
      rtools4x_version = function() "44"
    )
    expect_snapshot_error(check_rtools4x_windows_toolchain())
  })

  dir.create(file.path(fake_rtools_home, "usr", "bin"),
             recursive = TRUE, showWarnings = FALSE)
  local({
    local_mocked_bindings(
      rtools4x_home_path = function() fake_rtools_home,
      rtools4x_version = function() "44"
    )
    expect_error(
      check_rtools4x_windows_toolchain(),
      "restart R, and then run cmdstanr::check_cmdstan_toolchain()",
      fixed = TRUE
    )
  })
})

test_that("check_rtools4x_windows_toolchain validates install path and empty candidates", {
  local({
    local_mocked_bindings(
      rtools4x_home_path = function() "C:/Program Files/Rtools44",
      rtools4x_version = function() "44"
    )
    expect_snapshot_error(check_rtools4x_windows_toolchain())
  })

  fake_rtools_home <- tempfile(pattern = "rtools-home-empty-", tmpdir = tempdir(check = TRUE))
  on.exit(unlink(fake_rtools_home, recursive = TRUE), add = TRUE)
  dir.create(file.path(fake_rtools_home, "usr", "bin"),
             recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fake_rtools_home, "usr", "bin", "make.exe"))

  local({
    local_mocked_bindings(
      rtools4x_home_path = function() fake_rtools_home,
      rtools4x_version = function() "44",
      rtools4x_toolchain_candidates = function() character()
    )
    expect_error(
      check_rtools4x_windows_toolchain(),
      "restart R, and then run cmdstanr::check_cmdstan_toolchain()",
      fixed = TRUE
    )
  })
})

test_that("check_cmdstan_toolchain(fix = TRUE) is deprecated", {
  expect_snapshot_warning(check_cmdstan_toolchain(fix = TRUE))
})
