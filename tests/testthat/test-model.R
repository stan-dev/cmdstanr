# Setup -------------------------------------------------------------------
NOT_CRAN <-
  identical(Sys.getenv("NOT_CRAN"), "true") ||
  identical(Sys.getenv("TRAVIS"), "true")

if (NOT_CRAN) {
  set_cmdstan_path()
  stan_program <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  mod <- cmdstan_model(stan_file = stan_program, compile = FALSE)

  # valid ways to supply data
  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
  data_file_r <- test_path("resources", "data", "bernoulli.data.R")
  data_file_json <- test_path("resources", "data", "bernoulli.data.json")
}

expect_experimental_warning <- function(object) {
  testthat::expect_warning(
    object,
    regexp = "experimental and the structure of returned object may change"
  )
}
expect_sample_output <- function(object) {
  testthat::expect_output(object, "Gradient evaluation took")
}
expect_optim_output <- function(object) {
  expect_experimental_warning(
    expect_output(
      object,
      regexp = "Initial log joint probability"
    )
  )
}
expect_vb_output <- function(object) {
  expect_experimental_warning(
    expect_output(
      object,
      regexp = "Drawing a sample of size"
    )
  )
}

# Compile -----------------------------------------------------------------
context("CmdStanModel-compile")

test_that("object initialized correctly", {
  skip_on_cran()
  expect_equal(mod$stan_file(), stan_program)
  expect_equal(mod$exe_file(), character(0))
})

test_that("code() and print() methods work", {
  skip_on_cran()
  expect_known_output(mod$print(), file = test_path("answers", "model-print-output.stan"))
  expect_known_value(mod$code(), file = test_path("answers", "model-code-output.rds"))
})


test_that("error if no compile() before sample()", {
  skip_on_cran()
  expect_error(
    mod$sample(),
    "Model not compiled. Try running the compile() method first.",
    fixed = TRUE
  )
})

test_that("compile() method works", {
  skip_on_cran()
  expected <- if (!file.exists(strip_ext(mod$stan_file())))
    "Translating Stan model" else "is up to date"
  out <- utils::capture.output(mod$compile(quiet = FALSE))
  expect_output(print(out), expected)
  expect_equal(mod$exe_file(), strip_ext(stan_program))
})

test_that("compilation works when stan program not in cmdstan dir", {
  skip_on_cran()

  stan_program_2 <- test_path("resources", "stan", "bernoulli.stan")
  expect_message(
    mod_2 <- cmdstan_model(stan_file = stan_program_2, quiet = TRUE),
    "Compiling Stan program..."
  )
  expect_equal(mod_2$exe_file(), strip_ext(absolute_path(stan_program_2)))

  out <- utils::capture.output(
    mod_2 <- suppressMessages(cmdstan_model(stan_file = stan_program_2, quiet = FALSE))
  )
  expect_output(print(out), "is up to date")

  # cleanup
  file.remove(paste0(mod_2$exe_file(), c("", ".o",".hpp")))
})

# Sample ------------------------------------------------------------------
context("CmdStanModel-sample")

if (NOT_CRAN) {
  if (!length(mod$exe_file())) {
    utils::capture.output(mod$compile())
  }

  # these are all valid for sample()
  ok_arg_values <- list(
    data = data_list,
    num_chains = 2,
    num_warmup = 50,
    num_samples = 100,
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
    save_diagnostics = FALSE
  )

  # using any one of these should cause sample() to error
  bad_arg_values <- list(
    data = "NOT_A_FILE",
    num_chains = -1,
    num_warmup = -1,
    num_samples = -1,
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
    save_diagnostics = "NOT_LOGICAL"
  )

  bad_arg_values_2 <- list(
    init = "NOT_A_FILE",
    seed = 1:10,
    stepsize = 1:10,
    metric = c("AA", "BB")
  )

  bad_arg_values_3 <- list(
    init = rep("NOT_A_FILE", 10),
    metric = c("AA", "BB", "CC")
  )
}

test_that("sample() method works with data list", {
  skip_on_cran()

  expect_sample_output(fit <- mod$sample(data = data_list, num_chains = 1))
  expect_is(fit, "CmdStanMCMC")
})

test_that("sample() method works with data files", {
  skip_on_cran()

  expect_sample_output(fit_r <- mod$sample(data = data_file_r, num_chains = 1))
  expect_is(fit_r, "CmdStanMCMC")

  expect_sample_output(fit_json <- mod$sample(data = data_file_json, num_chains = 1))
  expect_is(fit_json, "CmdStanMCMC")
})

test_that("sample() method works with init file", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  init_list <- list(theta = 0.5)
  init_file <- tempfile(
    tmpdir = cmdstan_tempdir(),
    pattern = "testing-inits-",
    fileext = ".json"
  )
  write_stan_json(init_list, file = init_file)
  expect_sample_output(mod$sample(data = data_file_r, init = init_file))
})

test_that("sample() method runs when all arguments specified", {
  skip_on_cran()

  expect_sample_output(fit <- do.call(mod$sample, ok_arg_values))
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
})



# Optimize ----------------------------------------------------------------
context("CmdStanModel-optimize")

if (NOT_CRAN) {
  # these are all valid for optimize()
  ok_arg_values <- list(
    data = data_list,
    refresh = 5,
    init = NULL,
    seed = 12345,
    algorithm = "lbfgs",
    iter = 100,
    init_alpha = 0.002,
    save_diagnostics = FALSE
  )

  # using any of these should cause optimize() to error
  bad_arg_values <- list(
    data = "NOT_A_FILE",
    refresh = -20,
    init = "NOT_A_FILE",
    seed = "NOT_A_SEED",
    algorithm = "NOT_AN_ALGORITHM",
    iter = -20,
    init_alpha = -20,
    save_diagnostics = "NOT_LOGICAL"
  )
}


test_that("optimize() method runs when all arguments specified validly", {
  skip_on_cran()

  # specifying all arguments validly
  expect_optim_output(fit1 <- do.call(mod$optimize, ok_arg_values))
  expect_is(fit1, "CmdStanMLE")

  # leaving all at default (except 'data')
  expect_optim_output(fit2 <- mod$optimize(data = data_file_json))
  expect_is(fit2, "CmdStanMLE")
})

test_that("optimize() method errors for any invalid argument before calling cmdstan", {
  skip_on_cran()

  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(
      expect_experimental_warning(do.call(mod$optimize, args)),
      regexp = nm
    )
  }
})

test_that("optimize() errors when combining 'newton' with 'init_alpha'", {
  skip_on_cran()
  expect_error(
    expect_experimental_warning(
      mod$optimize(data = data_list, algorithm = "newton", init_alpha = 0.1)
    ),
    "'init_alpha' can't be used when algorithm is 'newton'"
  )
})



# Variational -------------------------------------------------------------
context("CmdStanModel-variational")

if (NOT_CRAN) {
  # these are all valid for variational()
  ok_arg_values <- list(
    data = data_list,
    refresh = 5,
    init = 0,
    seed = 12345,
    algorithm = "meanfield",
    iter = 10000,
    grad_samples = 2,
    elbo_samples = 101,
    eta = 1.5,
    adapt_engaged = TRUE,
    adapt_iter = 51,
    tol_rel_obj = 0.011,
    eval_elbo = 101,
    output_samples = 10,
    save_diagnostics = FALSE
  )

  # using any one of these should cause sample() to error
  bad_arg_values <- list(
    data = "NOT_A_FILE",
    refresh = -10,
    init = -10,
    seed = "NOT_A_SEED",
    algorithm = "NOT_AN_ALGORITHM",
    iter = -10,
    grad_samples = -10,
    elbo_samples = -10,
    eta = -1.5,
    adapt_engaged = "NOT_VALID",
    adapt_iter = -10,
    tol_rel_obj = -0.5,
    eval_elbo = -10,
    output_samples = -10,
    save_diagnostics = "NOT_LOGICAL"
  )
}

test_that("variational() method runs when all arguments specified validly", {
  skip_on_cran()

  # specifying all arguments validly
  expect_vb_output(fit1 <- do.call(mod$variational, ok_arg_values))
  expect_is(fit1, "CmdStanVB")

  # leaving all at default (except 'data')
  expect_vb_output(fit2 <- mod$variational(data = data_list))
  expect_is(fit2, "CmdStanVB")
})

test_that("variational() method errors for any invalid argument before calling cmdstan", {
  skip_on_cran()

  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(
      expect_experimental_warning(do.call(mod$variational, args)),
      regexp = nm
    )
  }
})


