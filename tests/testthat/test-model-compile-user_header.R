skip_if(os_is_macos())

file_that_exists <- "placeholder_exists"
file_that_doesnt_exist <- "placeholder_doesnt_exist"
withr::local_file(file_that_exists)

w_path <- function(f) {
  x <- sapply(f, function(fi) wsl_safe_path(absolute_path(fi)))
  names(x) <- NULL
  x
}

make_local_orig <- cmdstan_make_local()
cmdstan_make_local(cpp_options = list("PRECOMPILED_HEADERS" = "false"))

withr::defer(
  cmdstan_make_local(cpp_options = make_local_orig, append = FALSE)
)

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
  tmpfile <- withr::local_tempfile(lines = hpp, fileext = ".hpp")

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
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
  file.create(file_that_exists)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
    code = expect_no_mock_compile({
      mod$compile(quiet = TRUE, user_header = tmpfile)
    })
  )

  Sys.setFileTime(tmpfile, Sys.time() + 1) # touch file to trigger recompile
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
    code = expect_mock_compile({
      mod$compile(quiet = TRUE, user_header = tmpfile)
    })
  )

  # mock does not automatically update file mtime
  Sys.setFileTime(mod$exe_file(), Sys.time() + 1) # touch file to trigger recompile

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

  cpp_options <- expect_warning(
    mod$cpp_options(),
    'will be deprecated'
  )
  # USER_HEADER is converted
  # user_header is NULL
  expect_equal(cpp_options[['USER_HEADER']],  w_path(tmp_file))
  expect_true(is.null(cpp_options[['user_header']]))

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

  cpp_options <- expect_warning(
    mod$cpp_options(),
    'will be deprecated'
  )
  # USER_HEADER is converted
  # user_header is unconverted
  expect_equal(cpp_options[['USER_HEADER']],  w_path(tmp_file))
  expect_true(is.null(cpp_options[['user_header']]))

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


  cpp_options <- expect_warning(
    mod$cpp_options(),
    'will be deprecated'
  )
  # In  other cases, in the *output* USER_HEADER is windows style user_header is not.
  # In this case, USER_HEADER is null.
  expect_true(is.null(cpp_options[['USER_HEADER']]))
  expect_equal(cpp_options[['user_header']],  w_path(tmp_file))
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
  cpp_options <- expect_warning(
    mod$cpp_options(),
    'will be deprecated'
  )
  # In this case:
  # cpp_options[['USER_HEADER']] == tmp_files[1] <- actually used
  # cpp_options[['user_header']] == tmp_files[3] <- ignored
  # tmp_files[2] is not stored
  expect_equal(
    match(cpp_options[['USER_HEADER']], w_path(tmp_files)),
    1
  )
  expect_equal(
    match(cpp_options[['user_header']], tmp_files),
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
  cpp_options <- expect_warning(
    mod$cpp_options(),
    'will be deprecated'
  )
  # In this case:
  # cpp_options[['USER_HEADER']] == tmp_files[2]
  # cpp_options[['user_header']] == tmp_files[3]
  # tmp_files[2] is not stored
  expect_equal(
    match(cpp_options[['USER_HEADER']], w_path(tmp_files)),
    2
  )
  expect_equal(
    match(cpp_options[['user_header']], tmp_files),
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
  cpp_options <- expect_warning(
    mod$cpp_options(),
    'will be deprecated'
  )
  # Same as Case #2
  expect_equal(
    match(cpp_options[['USER_HEADER']], w_path(tmp_files)),
    2
  )
  expect_equal(
    match(cpp_options[['user_header']], tmp_files),
    3
  )
})
