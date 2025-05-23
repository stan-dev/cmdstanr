context("model-sample")

set_cmdstan_path()
stan_program <- testing_stan_file("bernoulli")
mod <- testing_model("bernoulli")
stan_program_fp <- testing_stan_file("bernoulli_fp")
mod_fp <- testing_model("bernoulli_fp")

# valid ways to supply data
data_list <- testing_data("bernoulli")
data_file_r <- test_path("resources", "data", "bernoulli.data.R")
data_file_json <- test_path("resources", "data", "bernoulli.data.json")

# these are all valid for sample()
ok_arg_values <- list(
  data = data_list,
  output_dir = tempdir(),
  chains = 2,
  parallel_chains = 1,
  iter_warmup = 50,
  iter_sampling = 100,
  save_warmup = FALSE,
  thin = 2,
  refresh = 5,
  init = 1.5,
  seed = 12345,
  max_treedepth = 6,
  metric = "dense_e",
  step_size = 1.1,
  adapt_engaged = TRUE,
  adapt_delta = 0.7,
  save_latent_dynamics = FALSE,
  init_buffer = 20,
  term_buffer = 0,
  window = 15,
  save_metric = TRUE,
  save_cmdstan_config = TRUE
)

# using any one of these should cause sample() to error
bad_arg_values <- list(
  data = "NOT_A_FILE",
  output_dir = "NOT_A_DIRECTORY",
  chains = -1,
  parallel_chains = -1,
  iter_warmup = -1,
  iter_sampling = -1,
  save_warmup = "NO",
  thin = 0,
  refresh = -10,
  init = -10,
  seed = -10,
  max_treedepth = 0,
  metric = "NOT_A_METRIC",
  step_size = 0,
  adapt_engaged = "NO",
  adapt_delta = 2,
  save_latent_dynamics = "NOT_LOGICAL",
  init_buffer = "NOT_INTEGER",
  term_buffer = "NOT_INTEGER",
  window = "NOT_INTEGER",
  save_metric = "NOT_LOGICAL",
  save_cmdstan_config = "NOT_LOGICAL"
)

bad_arg_values_2 <- list(
  data = matrix(1:10),
  output_dir = 1,
  chains = "NOT_A_NUMBER",
  parallel_chains = "NOT_A_NUMBER",
  init = "NOT_A_FILE",
  seed = 1:10,
  step_size = 1:10,
  metric = c("AA", "BB"),
  init_buffer = -5,
  term_buffer = -6,
  window = -7
)

bad_arg_values_3 <- list(
  init = rep("NOT_A_FILE", 10),
  metric = c("AA", "BB", "CC")
)


test_that("sample() method works with data list", {
  expect_sample_output(fit <- mod$sample(data = data_list, chains = 1), 1)
  expect_is(fit, "CmdStanMCMC")
})

test_that("sample() method works with data files", {
  expect_sample_output(fit_r <- mod$sample(data = data_file_r, chains = 1), 1)
  expect_is(fit_r, "CmdStanMCMC")

  expect_sample_output(fit_json <- mod$sample(data = data_file_json, chains = 1), 1)
  expect_is(fit_json, "CmdStanMCMC")
})

test_that("sample() method works with init file", {
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
  expect_sample_output(fit <- do.call(mod$sample, ok_arg_values), 2)
  expect_is(fit, "CmdStanMCMC")
})

test_that("sample() method runs when the stan file is removed", {
  stan_file_tmp <- tempfile(pattern = "tmp", fileext = ".stan")
  file.copy(stan_program, stan_file_tmp)
  mod_tmp <- cmdstan_model(stan_file_tmp)
  file.remove(stan_file_tmp)
  expect_sample_output(
    mod_tmp$sample(data = data_list)
  )
})

test_that("sample() prints informational messages depening on show_exceptions", {
  mod_info_msg <- testing_model("info_message")
  expect_sample_output(
    expect_message(
      mod_info_msg$sample(),
      "Informational Message: The current Metropolis proposal is about to be rejected"
    )
  )
  expect_sample_output(
    expect_message(mod_info_msg$sample(show_exceptions = FALSE), regexp = NA)
  )
})

test_that("sample() method errors for any invalid arguments before calling cmdstan", {
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
  expect_output(
    fit <- mod$sample(chains = 2, data = data_list, iter_sampling = 0),
    "Iteration: 1000 / 1000 [100%]  (Warmup)",
    fixed = TRUE
  )
})

test_that("sampling in parallel works", {
  expect_output(
    mod$sample(data = data_list, chains = 2, parallel_chains = 2),
    "Running MCMC with 2 parallel chains",
    fixed = TRUE
  )

  expect_output(
    mod$sample(data = data_list, chains = 2, parallel_chains = 2),
    "Both chains finished successfully",
    fixed = TRUE
  )
})

test_that("mc.cores option detected", {
  options(mc.cores = 3)
  expect_output(
    mod$sample(data = data_list, chains = 3),
    "Running MCMC with 3 parallel chains",
    fixed = TRUE
  )

  options(mc.cores = NULL)
  expect_output(
    mod$sample(data = data_list, chains = 2),
    "Running MCMC with 2 sequential chains",
    fixed = TRUE
  )
})

test_that("sample() method runs when fixed_param = TRUE", {
  mod_fp$compile()

  expect_sample_output(fit_1000 <- mod_fp$sample(fixed_param = TRUE, iter_sampling = 1000), 4)
  expect_is(fit_1000, "CmdStanMCMC")
  expect_equal(dim(fit_1000$draws()), c(1000,4,10))

  expect_sample_output(fit_500 <- mod_fp$sample(fixed_param = TRUE, iter_sampling = 500), 4)
  expect_equal(dim(fit_500$draws()), c(500,4,10))

  expect_sample_output(fit_500_w <- mod_fp$sample(fixed_param = TRUE, iter_sampling = 500, iter_warmup = 5000), 4)
  expect_equal(dim(fit_500_w$draws()), c(500,4,10))

  expect_equal(fit_1000$metadata()$algorithm, "fixed_param")
  expect_equal(fit_500$metadata()$algorithm, "fixed_param")
  expect_equal(fit_500_w$metadata()$algorithm, "fixed_param")
})

test_that("sample() method runs when adapt_engaged = FALSE", {
  expect_sample_output(fit <- mod$sample(data = data_list, chains = 1, adapt_engaged = FALSE), 1)
  draws <- try(fit$draws(), silent = TRUE)
  expect_false(inherits(draws, "try-error"))
})

test_that("chain_ids work with sample()", {
  mod$compile()
  expect_sample_output(fit12 <- mod$sample(data = data_list, chains = 2, chain_ids = c(10,12)))
  expect_is(fit12, "CmdStanMCMC")
  expect_equal(fit12$metadata()$id, c(10,12))

  expect_sample_output(fit12 <- mod$sample(data = data_list, chains = 2, chain_ids = c(100,7)))
  expect_is(fit12, "CmdStanMCMC")
  expect_equal(fit12$metadata()$id, c(100,7))

  expect_sample_output(fit12 <- mod$sample(data = data_list, chains = 1, chain_ids = c(6)))
  expect_is(fit12, "CmdStanMCMC")
  expect_equal(fit12$metadata()$id, c(6))

  expect_error(mod$sample(data = data_list, chains = 1, chain_ids = c(0)),
               "Assertion on 'chain_ids' failed: Element 1 is not >= 1.")

  expect_error(mod$sample(data = data_list, chains = 2, chain_ids = c(1,1)),
               "Assertion on 'chain_ids' failed: Contains duplicated values, position 2.")

  expect_error(mod$sample(data = data_list, chains = 1, chain_ids = c(1,2)),
               "Assertion on 'chain_ids' failed: Must have length 1, but has length 2.")
})

test_that("print statements in transformed data work", {
  mod <- cmdstan_model(write_stan_file(
    'transformed data {
    int N = 2;
    print("*N = ", N, "*");
  }
  parameters {
    real x;
  }
  model {
    x ~ normal(0, 1);
  }'
  ))

  out <- capture.output(fit <- mod$sample(iter_warmup = 1, iter_sampling = 5, chains = 1))
  expect_true(any(grepl("*N = 2*", out)))
})

test_that("seed works for multi chain sampling", {
  m <- "
  transformed data {
    int N = 100;
    vector[N] a;
    for (i in 1:N) {
      a[i] = uniform_rng(0, 1);
    }
  }
  parameters {
    real y;
  }
  model {
    y ~ std_normal();
  }
  generated quantities {
    vector[N] tdata = a;
    vector[N] gq;
    for (i in 1:N) {
      gq[i] = uniform_rng(0, 4);
    }
  }
  "
  f <- write_stan_file(m, basename = "rngs.stan")
  mod <- cmdstan_model(f)
  utils::capture.output(
    fit_sample <- mod$sample(chains = 2, iter_sampling = 5, iter_warmup = 100, seed = 2)
  )
  chain_tdata_1 <- posterior::subset_draws(fit_sample$draws("tdata"), chain = 1)
  chain_tdata_2 <- posterior::subset_draws(fit_sample$draws("tdata"), chain = 2)
  expect_equal(chain_tdata_1, chain_tdata_2)
  chain_tdata_1 <- posterior::subset_draws(fit_sample$draws("gq"), chain = 1)
  chain_tdata_2 <- posterior::subset_draws(fit_sample$draws("gq"), chain = 2)
  expect_false(all(chain_tdata_1 == chain_tdata_2))

  utils::capture.output(
    fit_sample <- mod$sample(chains = 2, iter_sampling = 5, iter_warmup = 100,
                             seed = c(1, 2))
  )
  chain_tdata_1 <- posterior::subset_draws(fit_sample$draws("tdata"), chain = 1)
  chain_tdata_2 <- posterior::subset_draws(fit_sample$draws("tdata"), chain = 2)
  expect_false(all(chain_tdata_1 == chain_tdata_2))
  chain_tdata_1 <- posterior::subset_draws(fit_sample$draws("gq"), chain = 1)
  chain_tdata_2 <- posterior::subset_draws(fit_sample$draws("gq"), chain = 2)
  expect_false(all(chain_tdata_1 == chain_tdata_2))
})

test_that("Correct behavior if fixed_param not set when the model has no parameters", {
  code <- "
  model {}
  generated quantities  {
    real y = normal_rng(0, 1);
  }
  "
  stan_file <- write_stan_file(code)
  m <- cmdstan_model(stan_file)
  fake_cmdstan_version("2.35.0", m)
  expect_error(
    m$sample(),
    "Model contains no parameters. Please use 'fixed_param = TRUE'."
  )

  reset_cmdstan_version(m)
  if (cmdstan_version() >= "2.36.0") {
    # as of 2.36.0 we don't need fixed_param if no parameters
    expect_no_error(
      utils::capture.output(
        fit <- m$sample(iter_warmup = 10, iter_sampling = 10, diagnostics = NULL)
      )
    )
  }
})

test_that("sig_figs warning if version less than 2.25", {
  fake_cmdstan_version("2.24.0", mod)
  expect_warning(
    expect_sample_output(mod$sample(data = data_list, chains = 1, refresh = 0, sig_figs = 3)),
    "The 'sig_figs' argument is only supported with cmdstan 2.25+ and will be ignored!",
    fixed = TRUE
  )
  reset_cmdstan_version(mod)
})

test_that("Errors are suppressed with show_exceptions", {
  errmodcode <- "
  data {
    real y_mean;
  }
  transformed data {
    vector[1] small;
    small[2] = 1.0;
  }
  parameters {
    real y;
  }
  model {
    y ~ normal(y_mean, 1);
  }
  "
  errmod <- cmdstan_model(write_stan_file(errmodcode), force_recompile = TRUE)

  expect_sample_output(
    expect_message(
      suppressWarnings(errmod$sample(data = list(y_mean = 1), chains = 1)),
      "Chain 1 Exception: vector[uni] assign: accessing element out of range",
      fixed = TRUE
    ))

  expect_sample_output(
    expect_no_message(
      suppressWarnings(errmod$sample(data = list(y_mean = 1), chains = 1, show_exceptions = FALSE))
    ))
})

test_that("All output can be suppressed by show_messages", {
  stan_program <- testing_stan_file("bernoulli")
  data_list <- testing_data("bernoulli")
  mod <- cmdstan_model(stan_program, force_recompile = TRUE)
  options("cmdstanr_verbose" = FALSE)
  output <- capture.output(
    fit <- mod$sample(data = data_list, show_messages = FALSE)
  )

  expect_length(output, 0)
})
