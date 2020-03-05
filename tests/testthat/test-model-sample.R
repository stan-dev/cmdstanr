context("model-sample")

if (not_on_cran()) {
  set_cmdstan_path()
  stan_program <- testing_stan_file("bernoulli")
  mod <- testing_model("bernoulli")

  # valid ways to supply data
  data_list <- testing_data("bernoulli")
  data_file_r <- test_path("resources", "data", "bernoulli.data.R")
  data_file_json <- test_path("resources", "data", "bernoulli.data.json")

  # these are all valid for sample()
  ok_arg_values <- list(
    data = data_list,
    output_dir = tempdir(),
    chains = 2,
    cores = 1,
    warmup_iters = 50,
    sampling_iters = 100,
    save_warmup = FALSE,
    thin = 2,
    refresh = 5,
    init = 1.5,
    seed = 12345,
    max_depth = 6,
    metric = "dense_e",
    stepsize = 1.1,
    adapt_engaged = TRUE,
    adapt_delta = 0.7,
    save_extra_diagnostics = FALSE,
    init_buffer = 20,
    term_buffer = 0,
    window = 15
  )

  # using any one of these should cause sample() to error
  bad_arg_values <- list(
    data = "NOT_A_FILE",
    output_dir = "NOT_A_DIRECTORY",
    chains = -1,
    cores = -1,
    warmup_iters = -1,
    sampling_iters = -1,
    save_warmup = "NO",
    thin = 0,
    refresh = -10,
    init = -10,
    seed = -10,
    max_depth = 0,
    metric = "NOT_A_METRIC",
    stepsize = 0,
    adapt_engaged = "NO",
    adapt_delta = 2,
    save_extra_diagnostics = "NOT_LOGICAL",
    init_buffer = "NOT_INTEGER",
    term_buffer = "NOT_INTEGER",
    window = "NOT_INTEGER"
  )

  bad_arg_values_2 <- list(
    data = matrix(1:10),
    output_dir = 1,
    chains = "NOT_A_NUMBER",
    cores = "NOT_A_NUMBER",
    init = "NOT_A_FILE",
    seed = 1:10,
    stepsize = 1:10,
    metric = c("AA", "BB"),
    init_buffer = -5,
    term_buffer = -6,
    window = -7
  )

  bad_arg_values_3 <- list(
    init = rep("NOT_A_FILE", 10),
    metric = c("AA", "BB", "CC")
  )
}

test_that("code() and print() methods work", {
  skip_on_cran()

  expect_known_output(mod$print(), file = test_path("answers", "model-print-output.stan"))
  expect_known_value(mod$code(), file = test_path("answers", "model-code-output.rds"))
})

test_that("sample() method works with data list", {
  skip_on_cran()

  expect_sample_output(fit <- mod$sample(data = data_list, chains = 1), 1)
  expect_is(fit, "CmdStanMCMC")
})

test_that("sample() method works with data files", {
  skip_on_cran()

  expect_sample_output(fit_r <- mod$sample(data = data_file_r, chains = 1), 1)
  expect_is(fit_r, "CmdStanMCMC")

  expect_sample_output(fit_json <- mod$sample(data = data_file_json, chains = 1), 1)
  expect_is(fit_json, "CmdStanMCMC")
})

test_that("sample() method works with init file", {
  skip_on_cran()

  init_list <- list(theta = 0.5)
  init_file <- tempfile(
    tmpdir = cmdstan_tempdir(),
    pattern = "testing-inits-",
    fileext = ".json"
  )
  write_stan_json(init_list, file = init_file)
  expect_sample_output(mod$sample(data = data_file_r, init = init_file, chains = 1), 1)
})

test_that("sample() method runs when all arguments specified", {
  skip_on_cran()

  expect_sample_output(fit <- do.call(mod$sample, ok_arg_values), 2)
  expect_is(fit, "CmdStanMCMC")
})

test_that("sample() method errors for any invalid arguments before calling cmdstan", {
  skip_on_cran()

  utils::capture.output(mod$compile())
  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(do.call(mod$sample, args), regexp = nm)
  }

  for (nm in names(bad_arg_values_2)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values_2[[nm]]
    expect_error(do.call(mod$sample, args), regexp = nm)
  }

  for (nm in names(bad_arg_values_3)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values_3[[nm]]
    expect_error(do.call(mod$sample, args), regexp = nm)
  }
})

test_that("sample works for warmup-only run", {
  skip_on_cran()
  expect_output(
    fit <- mod$sample(chains = 2, data = data_list, sampling_iters = 0),
    "Iteration: 1000 / 1000 [100%]  (Warmup)",
    fixed = TRUE
  )
})

test_that("sampling in parallel works", {
  skip_on_cran()
  expect_output(
    mod$sample(data = data_list, chains = 2, cores = 2),
    "Running MCMC with 2 chain(s) on 2 core(s)",
    fixed = TRUE
  )

  expect_output(
    mod$sample(data = data_list, chains = 2, cores = 2),
    "Both chains finished succesfully",
    fixed = TRUE
  )
})

test_that("mc.cores option detected", {
  skip_on_cran()

  options(mc.cores = 3)
  expect_output(
    mod$sample(data = data_list, chains = 3),
    "Running MCMC with 3 chain(s) on 3 core(s)",
    fixed = TRUE
  )

  options(mc.cores = NULL)
  expect_output(
    mod$sample(data = data_list, chains = 2),
    "Running MCMC with 2 chain(s) on 1 core(s)",
    fixed = TRUE
  )
})
