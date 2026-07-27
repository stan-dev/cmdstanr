# This test is deliberately placed above the file-level skip_if(os_is_macos())
# below: it mocks the stanc call and never compiles, so it needs no toolchain
# and should run on every platform.
test_that("cpp_options user headers allow undefined functions", {
  stan_file <- testing_stan_file("bernoulli_external")
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  for (option_name in c("USER_HEADER", "user_header")) {
    model <- cmdstan_model(stan_file, compile = FALSE)
    model$compile(
      cpp_options = setNames(list(user_header), option_name),
      force_recompile = TRUE,
      dry_run = TRUE
    )
  }

  expect_length(received_stancflags, 4)
  expect_equal(
    vapply(
      received_stancflags,
      function(x) "--allow-undefined" %in% x,
      logical(1)
    ),
    rep(TRUE, 4)
  )
})

# Also above the file-level skip_if() below: the compiler is mocked, so this
# needs no toolchain either.
test_that("compile() commits the user header setting after compiling", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli_external.stan")
  file.copy(testing_stan_file("bernoulli_external"), stan_file)
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) ""
  )
  model <- cmdstan_model(stan_file, compile = FALSE)
  expect_false(model$.__enclos_env__$private$using_user_header_)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0),
    code = model$compile(user_header = user_header, force_recompile = TRUE)
  )
  expect_true(model$.__enclos_env__$private$using_user_header_)

  # a bare recompile doesn't carry the user header over, so the setting is
  # committed as FALSE, matching what was actually compiled
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0),
    code = model$compile(force_recompile = TRUE)
  )
  expect_false(model$.__enclos_env__$private$using_user_header_)
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
    info_ret = list(),
    code = expect_mock_compile({
      mod_2 <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        exe_file = file_that_doesnt_exist,
        cpp_options = list(USER_HEADER = tmpfile),
        stanc_options = list("allow-undefined")
      )
    })
  )

  # Check recompilation upon changing header
  exe_mtime <- header_mtime + 10
  # Mocked compile does not create the executable that real compilation writes.
  file.create(file_that_exists)
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

  # Mocked compile does not create the executable that real compilation writes.
  file.create(mod$exe_file())
  Sys.setFileTime(mod$exe_file(), header_mtime + 10) # make exe newer than header

  # Alternative spec of user header
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
    code = expect_no_mock_compile({
      mod$compile(
        quiet = TRUE,
        cpp_options = list(user_header = tmpfile),
        dry_run = TRUE
      )
    })
  )

  # Error/warning messages
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_error(
      cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        cpp_options = list(USER_HEADER = "non_existent.hpp"),
        stanc_options = list("allow-undefined")
      ),
      "header file '[^']*' does not exist"
    )
  )

  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_warning(
      cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        cpp_options = list(USER_HEADER = tmpfile, user_header = tmpfile),
        dry_run = TRUE
      ),
      "User header specified both"
    )
  )
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_warning(
      cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        user_header = tmpfile,
        cpp_options = list(USER_HEADER = tmpfile),
        dry_run = TRUE
      ),
      "User header specified both"
    )
  )
})

test_that("wsl path conversion is done as expected", {
  tmp_file <- withr::local_tempfile(lines = hpp, fileext = ".hpp")
 # Case 1: arg
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = {
      mod <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        user_header = tmp_file,
        dry_run = TRUE
      )
    }
  )

  # USER_HEADER is converted
  # user_header is NULL
  expect_equal(mod$cpp_options()[['USER_HEADER']],  w_path(tmp_file))
  expect_true(is.null(mod$cpp_options()[['user_header']]))

  # Case 2: cpp opt USER_HEADER
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = {
      mod <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        cpp_options = list(
          USER_HEADER = tmp_file
        ),
        dry_run = TRUE
      )
    }
  )

  # USER_HEADER is converted
  # user_header is unconverted
  expect_equal(mod$cpp_options()[['USER_HEADER']],  w_path(tmp_file))
  expect_true(is.null(mod$cpp_options()[['user_header']]))

  # Case # 3: only user_header opt
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = {
      mod <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        cpp_options = list(
          user_header = tmp_file
        ),
        dry_run = TRUE
      )
    }
  )


  # In  other cases, in the *output* USER_HEADER is windows style user_header is not.
  # In this case, USER_HEADER is null.
  expect_true(is.null(mod$cpp_options()[['USER_HEADER']]))
  expect_equal(mod$cpp_options()[['user_header']],  w_path(tmp_file))
})

test_that("user_header precedence order is correct", {
  tmp_files <- sapply(1:3, function(n) withr::local_tempfile(
    lines = hpp,
    fileext = ".hpp",
    .local_envir = parent.frame(3)
  ))

  # Case # 1: all 3 specified
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_warning({
      mod <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        user_header = tmp_files[1],
        cpp_options = list(
          USER_HEADER = tmp_files[2],
          user_header = tmp_files[3]
        ),
        dry_run = TRUE
      )
    }, "User header specified both")
  )
  # In this case:
  # cpp_options[['USER_HEADER']] == tmp_files[1] <- actually used
  # cpp_options[['user_header']] == tmp_files[3] <- ignored
  # tmp_files[2] is not stored
  expect_equal(
    match(!!(mod$cpp_options()[['USER_HEADER']]), w_path(tmp_files)),
    1
  )
  expect_equal(
    match(!!(mod$cpp_options()[['user_header']]), tmp_files),
    3
  )

  # Case # 2: Both opts, but no arg
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_warning({
      mod <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        cpp_options = list(
          USER_HEADER = tmp_files[2],
          user_header = tmp_files[3]
        ),
        dry_run = TRUE
      )
    }, "User header specified both")
  )
  # In this case:
  # cpp_options[['USER_HEADER']] == tmp_files[2]
  # cpp_options[['user_header']] == tmp_files[3]
  # tmp_files[2] is not stored
  expect_equal(
    match(!!(mod$cpp_options()[['USER_HEADER']]), w_path(tmp_files)),
    2
  )
  expect_equal(
    match(!!(mod$cpp_options()[['user_header']]), tmp_files),
    3
  )

  # Case # 3: Both opts, other order
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_warning({
      mod <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        cpp_options = list(
          user_header = tmp_files[3],
          USER_HEADER = tmp_files[2]
        ),
        dry_run = TRUE
      )
    }, "User header specified both")
  )
  # Same as Case #2
  expect_equal(
    match(!!(mod$cpp_options()[['USER_HEADER']]), w_path(tmp_files)),
    2
  )
  expect_equal(
    match(!!(mod$cpp_options()[['user_header']]), tmp_files),
    3
  )
})
