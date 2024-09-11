test_that("include_paths set on compiled model with mocks", {
  stan_program_w_include <- testing_stan_file("bernoulli_include")

  with_mocked_cli(compile_ret = list(status = 0), info_ret = list(), code = expect_message({
    mod_w_include <- cmdstan_model(stan_file = stan_program_w_include, compile=TRUE,
                                  include_paths = test_path("resources", "stan"))
  }, 'mock-compile-was-called'))
  expect_true(mod_w_include$check_syntax())
})