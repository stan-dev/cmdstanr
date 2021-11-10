context("opencl")

set_cmdstan_path()
fit <- testing_fit("bernoulli", method = "sample", seed = 123, chains = 1)

test_that("all methods error when opencl_ids is used with non OpenCL model", {
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file = stan_file)
  expect_error(
    mod$sample(data = testing_data("bernoulli"), opencl_ids = c(0, 0), chains = 1),
    "'opencl_ids' is set but the model was not compiled with for use with OpenCL.",
    fixed = TRUE
  )
  expect_error(
    mod$optimize(data = testing_data("bernoulli"), opencl_ids = c(0, 0)),
    "'opencl_ids' is set but the model was not compiled with for use with OpenCL.",
    fixed = TRUE
  )
  expect_error(
    mod$variational(data = testing_data("bernoulli"), opencl_ids = c(0, 0)),
    "'opencl_ids' is set but the model was not compiled with for use with OpenCL.",
    fixed = TRUE
  )
  stan_file_gq <- testing_stan_file("bernoulli_ppc")
  mod_gq <- cmdstan_model(stan_file = stan_file_gq)
  expect_error(
    mod_gq$generate_quantities(fitted_params = fit, data = testing_data("bernoulli"), opencl_ids = c(0, 0)),
    "'opencl_ids' is set but the model was not compiled with for use with OpenCL.",
    fixed = TRUE
  )
})

test_that("all methods error on invalid opencl_ids", {
  skip_if_not(Sys.getenv("CMDSTANR_OPENCL_TESTS") %in% c("1", "true"))
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file = stan_file, cpp_options = list(stan_opencl = TRUE))
  utils::capture.output(
    expect_warning(
      mod$sample(data = testing_data("bernoulli"), opencl_ids = c(1000, 1000), chains = 1),
      "No chains finished successfully",
      fixed = TRUE
    )
  )
  utils::capture.output(
    expect_warning(
      mod$optimize(data = testing_data("bernoulli"), opencl_ids = c(1000, 1000)),
      "Fitting finished unexpectedly!",
      fixed = TRUE
    )
  )
  utils::capture.output(
    expect_warning(
      mod$variational(data = testing_data("bernoulli"), opencl_ids = c(1000, 1000)),
      "Fitting finished unexpectedly!",
      fixed = TRUE
    )
  )
  stan_file_gq <- testing_stan_file("bernoulli_ppc")
  mod_gq <- cmdstan_model(stan_file = stan_file_gq, cpp_options = list(stan_opencl = TRUE))
  utils::capture.output(
    expect_warning(
      mod_gq$generate_quantities(fitted_params = fit, data = testing_data("bernoulli"), opencl_ids = c(1000, 1000)),
      "Chain 1 finished unexpectedly",
      fixed = TRUE
    )
  )
})

test_that("all methods run with valid opencl_ids", {
  skip_if_not(Sys.getenv("CMDSTANR_OPENCL_TESTS") %in% c("1", "true"))
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file = stan_file, cpp_options = list(stan_opencl = TRUE))
  expect_sample_output(
    fit <- mod$sample(data = testing_data("bernoulli"), opencl_ids = c(0, 0), chains = 1)
  )
  expect_false(is.null(fit$metadata()$opencl_platform_name))
  expect_false(is.null(fit$metadata()$opencl_ids_name))

  stan_file_gq <- testing_stan_file("bernoulli_ppc")
  mod_gq <- cmdstan_model(stan_file = stan_file_gq, cpp_options = list(stan_opencl = TRUE))
  expect_gq_output(
    fit <- mod_gq$generate_quantities(fitted_params = fit, data = testing_data("bernoulli"), opencl_ids = c(0, 0)),
  )
  expect_false(is.null(fit$metadata()$opencl_platform_name))
  expect_false(is.null(fit$metadata()$opencl_ids_name))

  expect_sample_output(
    fit <- mod$sample(data = testing_data("bernoulli"), opencl_ids = c(0, 0))
  )
  expect_false(is.null(fit$metadata()$opencl_platform_name))
  expect_false(is.null(fit$metadata()$opencl_ids_name))

  expect_optim_output(
    fit <- mod$optimize(data = testing_data("bernoulli"), opencl_ids = c(0, 0))
  )
  expect_false(is.null(fit$metadata()$opencl_platform_name))
  expect_false(is.null(fit$metadata()$opencl_ids_name))

  expect_vb_output(
    fit <- mod$variational(data = testing_data("bernoulli"), opencl_ids = c(0, 0))
  )
  expect_false(is.null(fit$metadata()$opencl_platform_name))
  expect_false(is.null(fit$metadata()$opencl_ids_name))
})

test_that("error for runtime selection of OpenCL devices if version less than 2.26", {
  skip_if_not(Sys.getenv("CMDSTANR_OPENCL_TESTS") %in% c("1", "true"))
  fake_cmdstan_version("2.25.0")

  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file = stan_file, cpp_options = list(stan_opencl = TRUE),
                       force_recompile = TRUE)
  expect_error(
    mod$sample(data = data_list, chains = 1, refresh = 0, opencl_ids = c(1,1)),
    "Runtime selection of OpenCL devices is only supported with CmdStan version 2.26 or newer",
    fixed = TRUE
  )
  reset_cmdstan_version()
})
