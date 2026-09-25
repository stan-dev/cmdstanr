skip_on_cran()

local_mocked_stanc <- function(.local_envir = parent.frame()) {
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name, ...) character(),
    get_standalone_hpp = function(stan_file, stancflags, ...) "",
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

test_that("a user header in cpp_options is rejected", {
  stan_file <- testing_stan_file("bernoulli_external")
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  # The message writes the path as an R literal, so a Windows path shows its
  # backslashes doubled.
  expected <- paste0(
    "The user header cannot be set through `cpp_options`. ",
    "Pass it with the `user_header` argument: `user_header = ",
    encodeString(user_header, quote = '"'), "`."
  )

  for (option_name in c("USER_HEADER", "user_header", "User_Header")) {
    cpp_options <- setNames(list(user_header), option_name)
    expect_error(
      cmdstan_model(stan_file, cpp_options = cpp_options),
      expected,
      fixed = TRUE
    )
  }
})

test_that("cmdstan_model() uses a supplied user header", {
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name, ...) character(),
    get_standalone_hpp = function(stan_file, stancflags, ...) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  model <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(local_external_model(), user_header = user_header)
  )

  expect_equal(model$user_header(), resolve_path(user_header))
  expect_true("--allow-undefined" %in% received_stancflags[[1]])
})

test_that("cmdstan_model() records a user header", {
  header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  model <- mock_cmdstan_model(
    testing_stan_file("bernoulli_external"),
    dir = withr::local_tempdir(),
    user_header = header
  )
  expect_equal(model$user_header(), resolve_path(header))
})

test_that("cmdstan_model() honours an explicit user_header = NULL", {
  model <- cmdstan_model(testing_stan_file("bernoulli"), user_header = NULL)
  expect_null(model$user_header())
})

test_that("cmdstan_model() rejects an empty user header", {
  expect_error(
    cmdstan_model(
      testing_stan_file("bernoulli_external"),
      user_header = character(0)
    ),
    "user_header"
  )
})

test_that("a relative user header survives a directory change", {
  model_dir <- withr::local_tempdir()
  file.copy(testing_stan_file("bernoulli_external"), model_dir)
  writeLines("", file.path(model_dir, "header.hpp"))
  local_mocked_stanc()

  model <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = withr::with_dir(
      model_dir,
      cmdstan_model("bernoulli_external.stan", user_header = "header.hpp")
    )
  )

  expect_equal(
    normalizePath(model$user_header()),
    normalizePath(file.path(model_dir, "header.hpp"))
  )
})

test_that("changing the user header forces compilation", {
  stan_file <- local_external_model()
  h1 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  h2 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_stanc()

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, user_header = h1)
  )

  model <- NULL
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(
      model <- cmdstan_model(stan_file, user_header = h2)
    )
  )
  expect_equal(model$user_header(), resolve_path(h2))
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

test_that("a header that does not exist errors before compiling", {
  expect_error(
    cmdstan_model(
      testing_stan_file("bernoulli_external"),
      user_header = "non_existent.hpp"
    ),
    "header file '[^']*' does not exist"
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
