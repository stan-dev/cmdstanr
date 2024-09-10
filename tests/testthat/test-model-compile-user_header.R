
file_that_exists <- 'placeholder_exists'
file_that_doesnt_exist <- 'placeholder_doesnt_exist'
file.create(file_that_exists)
on.exit(if(file.exists(file_that_exists)) file.remove(file_that_exists), add=TRUE, after=FALSE)

make_local_orig <- cmdstan_make_local()
cmdstan_make_local(cpp_options = list("PRECOMPILED_HEADERS"="false"))
on.exit(cmdstan_make_local(cpp_options = make_local_orig, append = FALSE), add = TRUE, after = FALSE)

test_that("cmdstan_model works with user_header with mock", {
  skip_if(os_is_macos())
  tmpfile <- tempfile(fileext = ".hpp")
  hpp <-
  "
  #include <stan/math.hpp>
  #include <boost/math/tools/promotion.hpp>
  #include <ostream>

  namespace bernoulli_external_model_namespace
  {
      template <typename T0__,
            stan::require_all_t<stan::is_stan_scalar<T0__>>* = nullptr>
      inline typename boost::math::tools::promote_args<T0__>::type make_odds(const T0__ &
                                                                                 theta,
                                                                             std::ostream *pstream__)
      {
          return theta / (1 - theta);
      }
  }"
  cat(hpp, file = tmpfile, sep = "\n")

  with_mocked_cli(compile_ret = list(status = 0), info_ret = list(), code = expect_message({
    mod <- cmdstan_model(
      stan_file = testing_stan_file("bernoulli_external"),
      exe_file = file_that_exists,
      user_header = tmpfile
    )
  }, message = 'mock-compile-was-called'))

  with_mocked_cli(compile_ret = list(status = 0), info_ret = list(), code = expect_message({
    mod_2 <- cmdstan_model(
      stan_file = testing_stan_file("bernoulli_external"),
      exe_file = file_that_doesnt_exist,
      cpp_options=list(USER_HEADER=tmpfile),
      stanc_options = list("allow-undefined")
    )
  }, message = 'mock-compile-was-called'))

  # Check recompilation upon changing header
  file.create(file_that_exists)
  with_mocked_cli(compile_ret = list(status = 0), info_ret = list(), code = expect_no_message({
    mod$compile(quiet = TRUE, user_header = tmpfile)
  }, message = 'mock-compile-was-called'))

  Sys.setFileTime(tmpfile, Sys.time() + 1) #touch file to trigger recompile
  with_mocked_cli(compile_ret = list(status = 0), info_ret = list(), code = expect_message({
    mod$compile(quiet = TRUE, user_header = tmpfile)
  }, message = 'mock-compile-was-called'))

  # Alternative spec of user header
  with_mocked_cli(compile_ret = list(status = 0), info_ret = list(), code = expect_no_message({
  mod$compile(
    quiet = TRUE,
    cpp_options = list(user_header = tmpfile),
    dry_run = TRUE
  )}, message = 'mock-compile-was-called'))

  # Error/warning messages
  with_mocked_cli(compile_ret = list(status = 1), info_ret = list(), code = expect_error(
    cmdstan_model(
      stan_file = testing_stan_file("bernoulli_external"),
      cpp_options = list(USER_HEADER = "non_existent.hpp"),
      stanc_options = list("allow-undefined")
    ),
    "header file '[^']*' does not exist"
  ))

  with_mocked_cli(compile_ret = list(status = 1), info_ret = list(), code = expect_warning(
    cmdstan_model(
      stan_file = testing_stan_file("bernoulli_external"),
      cpp_options = list(USER_HEADER = tmpfile, user_header = tmpfile),
      dry_run = TRUE
    ),
    "User header specified both"
  ))
  with_mocked_cli(compile_ret = list(status = 1), info_ret = list(), code = expect_warning(
    cmdstan_model(
      stan_file = testing_stan_file("bernoulli_external"),
      user_header = tmpfile,
      cpp_options = list(USER_HEADER = tmpfile),
      dry_run = TRUE
    ),
    "User header specified both"
  ))
})