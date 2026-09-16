set_cmdstan_path()
stan_program <- testing_stan_file("bernoulli")

# Point the session at an installation directory that does not exist while
# keeping the cached version, which is what a deletion leaves behind.
local_gone_installation <- function(.local_envir = parent.frame()) {
  path <- cmdstan_path()
  gone <- repair_path(file.path(
    withr::local_tempdir(.local_envir = .local_envir), "cmdstan"
  ))
  .cmdstanr$PATH <- gone
  withr::defer(.cmdstanr$PATH <- path, envir = .local_envir)
  gone
}

test_that("checked_cmdstan_path() returns the selected installation while it exists", {
  expect_equal(checked_cmdstan_path(), cmdstan_path())
})

test_that("a model is not built when the selected installation is gone", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(stan_program, stan_file)
  gone <- local_gone_installation()
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      expect_error(cmdstan_model(stan_file), gone, fixed = TRUE)
    )
  )
  expect_false(file.exists(cmdstan_ext(strip_ext(stan_file))))
})

test_that("check_syntax() and format() error naming the gone installation", {
  mod <- cmdstan_model(stan_program)
  gone <- local_gone_installation()
  expect_error(mod$check_syntax(), gone, fixed = TRUE)
  expect_error(mod$format(), gone, fixed = TRUE)
})

test_that("variables() works with the installation gone while stan_file_variables() does not", {
  mod <- cmdstan_model(stan_program)
  gone <- local_gone_installation()
  expect_no_error(mod$variables())
  expect_error(stan_file_variables(stan_program), gone, fixed = TRUE)
})

test_that("cmdstan_summary() errors naming the gone installation", {
  fit <- testing_fit("bernoulli", chains = 1, refresh = 0)
  expect_output(fit$cmdstan_summary(), "Inference for Stan model")
  gone <- local_gone_installation()
  expect_error(fit$cmdstan_summary(), gone, fixed = TRUE)
})
