set_cmdstan_path()
fit <- testing_fit("bernoulli", method = "sample", seed = 123, chains = 1)

test_that("all methods error when opencl_ids is used with non OpenCL model", {
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file = stan_file, force_recompile = TRUE)
  threads <- if (isTRUE(mod$exe_info()$stan_threads)) 1L else NULL
  expect_error(
    mod$sample(data = testing_data("bernoulli"), opencl_ids = c(0, 0), chains = 1),
    "'opencl_ids' is set but the model was not compiled for use with OpenCL.",
    fixed = TRUE
  )
  expect_error(
    mod$optimize(
      data = testing_data("bernoulli"),
      opencl_ids = c(0, 0),
      threads = threads
    ),
    "'opencl_ids' is set but the model was not compiled for use with OpenCL.",
    fixed = TRUE
  )
  expect_error(
    mod$variational(
      data = testing_data("bernoulli"),
      opencl_ids = c(0, 0),
      threads = threads
    ),
    "'opencl_ids' is set but the model was not compiled for use with OpenCL.",
    fixed = TRUE
  )
  stan_file_gq <- testing_stan_file("bernoulli_ppc")
  mod_gq <- cmdstan_model(stan_file = stan_file_gq, force_recompile = TRUE)
  expect_error(
    mod_gq$generate_quantities(fitted_params = fit, data = testing_data("bernoulli"), opencl_ids = c(0, 0)),
    "'opencl_ids' is set but the model was not compiled for use with OpenCL.",
    fixed = TRUE
  )
})

test_that("all methods error on invalid opencl_ids", {
  skip_if_not(Sys.getenv("CMDSTANR_OPENCL_TESTS") %in% c("1", "true"))
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(
    stan_file = stan_file,
    force_recompile = TRUE,
    cpp_options = list(stan_opencl = TRUE, stan_threads = TRUE)
  )
  utils::capture.output(
    expect_warning(
      mod$sample(data = testing_data("bernoulli"), opencl_ids = c(1000, 1000), chains = 1),
      "No chains finished successfully",
      fixed = TRUE
    )
  )
  utils::capture.output(
    expect_warning(
      mod$optimize(
        data = testing_data("bernoulli"),
        opencl_ids = c(1000, 1000),
        threads = 1
      ),
      "Fitting finished unexpectedly!",
      fixed = TRUE
    )
  )
  utils::capture.output(
    expect_warning(
      mod$variational(
        data = testing_data("bernoulli"),
        opencl_ids = c(1000, 1000),
        threads = 1
      ),
      "Fitting finished unexpectedly!",
      fixed = TRUE
    )
  )
  stan_file_gq <- testing_stan_file("bernoulli_ppc")
  mod_gq <- cmdstan_model(
    stan_file = stan_file_gq,
    force_recompile = TRUE,
    cpp_options = list(stan_opencl = TRUE, stan_threads = TRUE)
  )
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
  mod <- cmdstan_model(
    stan_file = stan_file,
    force_recompile = TRUE,
    cpp_options = list(stan_opencl = TRUE, stan_threads = TRUE)
  )
  expect_sample_output(
    fit_sample <- mod$sample(
      data = testing_data("bernoulli"),
      opencl_ids = c(0, 0),
      chains = 4
    )
  )
  expect_equal(fit_sample$num_chains(), 4L)
  expect_equal(fit_sample$num_procs(), 1L)
  expect_length(fit_sample$output_files(), 4L)
  expect_false(is.null(fit_sample$metadata()$opencl_platform_name))
  expect_false(is.null(fit_sample$metadata()$opencl_device_name))
  expect_false(is.null(fit_sample$metadata()$device))
  expect_false(is.null(fit_sample$metadata()$platform))

  stan_file_gq <- testing_stan_file("bernoulli_ppc")
  mod_gq <- cmdstan_model(
    stan_file = stan_file_gq,
    force_recompile = TRUE,
    cpp_options = list(stan_opencl = TRUE, stan_threads = TRUE)
  )
  expect_gq_output(
    fit_gq <- mod_gq$generate_quantities(
      fitted_params = fit_sample,
      data = testing_data("bernoulli"),
      opencl_ids = c(0, 0)
    )
  )
  expect_equal(fit_gq$num_chains(), 4L)
  expect_equal(fit_gq$num_procs(), 1L)
  expect_length(fit_gq$output_files(), 4L)
  expect_false(is.null(fit_gq$metadata()$opencl_platform_name))
  expect_false(is.null(fit_gq$metadata()$opencl_device_name))
  expect_false(is.null(fit_gq$metadata()$device))
  expect_false(is.null(fit_gq$metadata()$platform))

  expect_sample_output(
    fit <- mod$sample(data = testing_data("bernoulli"), opencl_ids = c(0, 0))
  )
  expect_false(is.null(fit$metadata()$opencl_platform_name))
  expect_false(is.null(fit$metadata()$opencl_device_name))
  expect_false(is.null(fit$metadata()$device))
  expect_false(is.null(fit$metadata()$platform))

  expect_optim_output(
    fit <- mod$optimize(
      data = testing_data("bernoulli"),
      opencl_ids = c(0, 0),
      threads = 1
    )
  )
  expect_false(is.null(fit$metadata()$opencl_platform_name))
  expect_false(is.null(fit$metadata()$opencl_device_name))
  expect_false(is.null(fit$metadata()$device))
  expect_false(is.null(fit$metadata()$platform))

  expect_vb_output(
    fit <- mod$variational(
      data = testing_data("bernoulli"),
      opencl_ids = c(0, 0),
      threads = 1
    )
  )
  expect_false(is.null(fit$metadata()$opencl_platform_name))
  expect_false(is.null(fit$metadata()$opencl_device_name))
  expect_false(is.null(fit$metadata()$device))
  expect_false(is.null(fit$metadata()$platform))
})
