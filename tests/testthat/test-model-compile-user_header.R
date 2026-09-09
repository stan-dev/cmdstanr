local_mocked_stanc <- function(.local_envir = parent.frame()) {
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) "",
    .env = .local_envir
  )
}

# Mocked compiles use temporary model copies to protect test resources.
local_external_model <- function(.local_envir = parent.frame()) {
  stan_file <- file.path(
    withr::local_tempdir(.local_envir = .local_envir),
    "bernoulli_external.stan"
  )
  file.copy(testing_stan_file("bernoulli_external"), stan_file)
  stan_file
}

# Keep mocked compilation tests above the toolchain skip below.
test_that("a user header in cpp_options is rejected", {
  stan_file <- testing_stan_file("bernoulli_external")
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  model <- cmdstan_model(stan_file, compile = FALSE)
  expected <- paste0(
    "The user header cannot be set through `cpp_options`. ",
    "Pass it with the `user_header` argument: `user_header = \"",
    user_header, "\"`."
  )

  for (option_name in c("USER_HEADER", "user_header", "User_Header")) {
    cpp_options <- setNames(list(user_header), option_name)
    expect_error(
      cmdstan_model(stan_file, compile = FALSE, cpp_options = cpp_options),
      expected,
      fixed = TRUE
    )
    expect_error(
      model$compile(cpp_options = cpp_options),
      expected,
      fixed = TRUE
    )
  }
})

test_that("compile() reuses the user header from the previous compilation", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli_external.stan")
  file.copy(testing_stan_file("bernoulli_external"), stan_file)
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )
  model <- cmdstan_model(stan_file, compile = FALSE)
  expect_null(model$user_header())

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0),
    code = model$compile(user_header = user_header, force_recompile = TRUE)
  )
  expect_equal(model$user_header(), resolve_path(user_header))

  received_stancflags <- list()
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0),
    code = model$compile(force_recompile = TRUE)
  )
  expect_equal(model$user_header(), resolve_path(user_header))
  expect_false("USER_HEADER" %in% names(model$cpp_options()))
  expect_equal(
    vapply(received_stancflags, function(x) "--allow-undefined" %in% x, logical(1)),
    rep(TRUE, 2)
  )
})

test_that("a no-op compile preserves the user header", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli_external.stan")
  file.copy(testing_stan_file("bernoulli_external"), stan_file)
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) ""
  )
  model <- cmdstan_model(stan_file, compile = FALSE)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(user_header = user_header, force_recompile = TRUE)
  )
  expect_equal(model$user_header(), resolve_path(user_header))

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(model$compile())
  )
  expect_equal(model$user_header(), resolve_path(user_header))
})

test_that("compile() uses a user header supplied to cmdstan_model()", {
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  model <- cmdstan_model(
    local_external_model(),
    user_header = user_header,
    compile = FALSE
  )
  # Use a successful compile so its options are recorded.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(force_recompile = TRUE)
  )

  expect_equal(model$user_header(), resolve_path(user_header))
  expect_equal(
    vapply(received_stancflags, function(x) "--allow-undefined" %in% x, logical(1)),
    rep(TRUE, 2)
  )
})

test_that("a header configured over a current executable does not rebuild", {
  stan_file <- local_external_model()
  header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_stanc()

  # An executable that already exists and is newer than both the program and
  # the header, built through a different object.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, force_recompile = TRUE)
  )
  exe <- cmdstan_ext(strip_ext(stan_file))
  Sys.setFileTime(stan_file, Sys.time() - 60)
  Sys.setFileTime(header, Sys.time() - 60)
  Sys.setFileTime(exe, Sys.time())

  # A fresh object cannot know which header built an existing executable, so it
  # keeps the executable without rebuilding against the requested header.
  model <- cmdstan_model(stan_file, compile = FALSE, user_header = header)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(model$compile())
  )
  expect_equal(model$user_header(), resolve_path(header))
})

test_that("cmdstan_model() records a user header", {
  header <- withr::local_tempfile(lines = "", fileext = ".hpp")

  model <- cmdstan_model(
    testing_stan_file("bernoulli_external"),
    compile = FALSE,
    user_header = header
  )
  private <- model$.__enclos_env__$private
  expect_equal(private$user_header_, resolve_path(header))
  expect_false(private$user_header_dirty_)
})

test_that("cmdstan_model() honours an explicit user_header = NULL", {
  model <- cmdstan_model(
    testing_stan_file("bernoulli_external"),
    compile = FALSE,
    user_header = NULL
  )

  expect_null(model$user_header())
})

test_that("cmdstan_model() rejects an empty user header", {
  expect_error(
    cmdstan_model(
      testing_stan_file("bernoulli_external"),
      compile = FALSE,
      user_header = character(0)
    ),
    "user_header"
  )
  model <- cmdstan_model(testing_stan_file("bernoulli_external"), compile = FALSE)
  expect_error(model$compile(user_header = character(0)), "user_header")
})

test_that("a relative user header survives a directory change", {
  model_dir <- withr::local_tempdir()
  file.copy(testing_stan_file("bernoulli_external"), model_dir)
  writeLines("", file.path(model_dir, "header.hpp"))
  local_mocked_stanc()

  model <- withr::with_dir(
    model_dir,
    cmdstan_model(
      "bernoulli_external.stan",
      compile = FALSE,
      user_header = "header.hpp"
    )
  )

  expect_equal(
    normalizePath(model$user_header()),
    normalizePath(file.path(model_dir, "header.hpp"))
  )
  # The compile happens from the test's own working directory.
  expect_no_error(model$compile(force_recompile = TRUE, dry_run = TRUE))
})

test_that("a bare retry after a failed compile keeps the newly supplied header", {
  stan_file <- local_external_model()
  h1 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  h2 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_stanc()
  model <- cmdstan_model(stan_file, compile = FALSE)
  private <- model$.__enclos_env__$private

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(user_header = h1, force_recompile = TRUE)
  )
  expect_equal(private$user_header_, resolve_path(h1))
  expect_false(private$user_header_dirty_)

  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(status = 1),
    code = expect_error(model$compile(user_header = h2), "An error occurred")
  )
  expect_equal(private$user_header_, resolve_path(h2))
  expect_true(private$user_header_dirty_)

  # A bare retry must build h2 rather than reverting to h1.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(model$compile())
  )
  expect_equal(private$user_header_, resolve_path(h2))
  expect_false(private$user_header_dirty_)
  expect_equal(model$user_header(), resolve_path(h2))
})

test_that("a header that does not exist is still recorded as the request", {
  stan_file <- local_external_model()
  header <- file.path(withr::local_tempdir(), "not_yet_written.hpp")
  local_mocked_stanc()
  model <- cmdstan_model(stan_file, compile = FALSE)
  private <- model$.__enclos_env__$private

  expect_error(
    model$compile(user_header = header, force_recompile = TRUE),
    "does not exist"
  )
  expect_equal(private$user_header_, resolve_path(header))
  expect_true(private$user_header_dirty_)

  # A bare retry once the header exists must build against it.
  file.create(header)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(model$compile())
  )
  expect_equal(model$user_header(), resolve_path(header))
})

test_that("changing the user header forces compilation", {
  stan_file <- local_external_model()
  h1 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  h2 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_stanc()
  model <- cmdstan_model(stan_file, compile = FALSE)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(user_header = h1, force_recompile = TRUE)
  )
  # Older than the executable, so only the change of header identity can force
  # a rebuild here (#813 only covers a header that was modified in place).
  Sys.setFileTime(h2, file.mtime(model$exe_file()) - 60)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(model$compile(user_header = h2))
  )
  expect_equal(model$user_header(), resolve_path(h2))
})

test_that("user_header = NULL clears a compiled header", {
  header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_stanc()

  model <- cmdstan_model(local_external_model(), compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(user_header = header, force_recompile = TRUE)
  )
  expect_equal(model$user_header(), resolve_path(header))

  # Clearing a compiled header must force a rebuild.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(model$compile(user_header = NULL))
  )
  expect_null(model$user_header())
})

skip_if(os_is_macos())

w_path <- function(f) {
  x <- sapply(f, function(fi) wsl_safe_path(absolute_path(fi)))
  names(x) <- NULL
  x
}

local_cmdstan_make_local(cpp_options = list("PRECOMPILED_HEADERS" = "false"))

hpp <- "
#include <stan/math.hpp>
#include <boost/math/tools/promotion.hpp>
#include <ostream>

namespace bernoulli_external_model_namespace
{
    template <typename T0__,
          stan::require_all_t<stan::is_stan_scalar<T0__>>* = nullptr>
    inline typename boost::math::tools::promote_args<T0__>::type make_odds(
      const T0__ & theta,
      std::ostream *pstream__
    )
    {
        return theta / (1 - theta);
    }
}"

test_that("cmdstan_model works with user_header with mock", {
  file_that_exists <- withr::local_tempfile(pattern = "placeholder_exists")
  file_that_doesnt_exist <- withr::local_tempfile(pattern = "placeholder_doesnt_exist")
  tmpfile <- withr::local_tempfile(lines = hpp, fileext = ".hpp")
  file.create(file_that_exists)
  header_mtime <- Sys.time()
  # On GHA Windows/R 4.1 files created close together sometimes compared equal
  # and skipped the mocked recompile, so set the header mtime to be in the past
  # and ensure the exe mtime is newer
  Sys.setFileTime(file_that_exists, header_mtime - 10)
  Sys.setFileTime(tmpfile, header_mtime)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = "stan_version_major=2\nstan_version_minor=35\nstan_version_patch=0"),
    code = expect_mock_compile(
      mod <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        exe_file = file_that_exists,
        user_header = tmpfile
      )
    )
  )

  with_mocked_cli(
    compile_ret = list(status = 0),
    # The mocked compile installs an executable, so the constructor queries it
    # for compilation info; report a failure rather than an empty list, which
    # model_compile_info() cannot interpret.
    info_ret = list(status = 1),
    code = expect_mock_compile({
      mod_2 <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        exe_file = file_that_doesnt_exist,
        user_header = tmpfile
      )
    })
  )

  # Check recompilation upon changing header
  exe_mtime <- header_mtime + 10
  # The mocked compile above installed the executable with a fresh mtime; pin it
  # so the up-to-date check below compares against a known value.
  Sys.setFileTime(file_that_exists, exe_mtime)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
    code = expect_no_mock_compile({
      mod$compile(quiet = TRUE, user_header = tmpfile)
    })
  )

  header_mtime <- exe_mtime + 10
  Sys.setFileTime(tmpfile, header_mtime) # touch file to trigger recompile
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
    code = expect_mock_compile({
      mod$compile(quiet = TRUE, user_header = tmpfile)
    })
  )

  # Error messages
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_error(
      cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        user_header = "non_existent.hpp"
      ),
      "header file '[^']*' does not exist"
    )
  )
})

test_that("wsl path conversion is done as expected", {
  tmp_file <- withr::local_tempfile(lines = hpp, fileext = ".hpp")
  local_mocked_stanc()

  # Capture the flags handed to make the way with_mocked_cli() does.
  make_args <- NULL
  local_mocked_bindings(
    wsl_compatible_run = function(command, args, ...) {
      if (!is.null(command)
          && command == make_cmd()
          && !is.null(args)
          && startsWith(basename(args[1]), "model-")) {
        make_args <<- args
        mock_exe <- wsl_safe_path(args[1], revert = TRUE)
        writeLines("mock executable", mock_exe)
        Sys.chmod(mock_exe, "0755", use_umask = FALSE)
        list(status = 0)
      } else if (!is.null(args) && args[1] == "info") {
        list(status = 1)
      } else {
        real_wcr(command = command, args = args, ...)
      }
    },
    .package = "cmdstanr"
  )

  mod <- cmdstan_model(local_external_model(), user_header = tmp_file)

  expect_true(paste0("USER_HEADER=", w_path(tmp_file)) %in% make_args)
  expect_equal(mod$user_header(), resolve_path(tmp_file))
  expect_false("USER_HEADER" %in% names(mod$cpp_options()))
})
