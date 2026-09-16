set_cmdstan_path()
stan_program <- testing_stan_file("bernoulli")
data_file <- test_path("resources", "data", "bernoulli.data.json")

features <- function(mod) mod$.__enclos_env__$private$reported_features_

test_that("a feature inherited from make/local is reported, not requested", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  threaded <- paste0(
    "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\n",
    "STAN_THREADS=true"
  )
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = {
      mod <- cmdstan_model(stan_file, force_recompile = TRUE)
      expect_true(features(mod)$stan_threads)
      expect_identical(
        assert_valid_threads(4, features(mod), multiple_chains = TRUE), 4
      )

      mod2 <- cmdstan_model(stan_file)
      expect_true(features(mod2)$stan_threads)
      expect_identical(
        assert_valid_threads(4, features(mod2), multiple_chains = TRUE), 4
      )
    }
  )
})

test_that("an executable reporting no threading flag is unknown, not off", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  info <- "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0"
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = info),
    code = {
      mod <- cmdstan_model(stan_file, force_recompile = TRUE)
      adopted <- cmdstan_model(exe_file = mod$exe_file())
    }
  )
  expect_null(features(adopted)$stan_threads)
  expect_error(
    adopted$sample(data = data_file, threads_per_chain = 2),
    "does not report threading as enabled", fixed = TRUE
  )
})

test_that("an executable adopted from live info reports threading as unknown", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  info <- "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0"
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = info),
    code = {
      mod <- cmdstan_model(stan_file, force_recompile = TRUE)
      file.remove(build_record_path(mod$exe_file()))
      adopted <- cmdstan_model(exe_file = mod$exe_file())
    }
  )
  expect_null(features(adopted)$stan_threads)
  expect_error(
    adopted$sample(data = data_file, threads_per_chain = 2),
    "does not report threading as enabled", fixed = TRUE
  )
})
