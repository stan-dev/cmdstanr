context("CmdStanModel")

if (is.null(.cmdstanr$PATH)) {
  set_cmdstan_path("/Users/jgabry/Documents/Stan/cmdstan-2.20.0")
}

my_stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.R")
my_data_list <- list(N = 10, y =c(0,1,0,0,0,0,0,0,0,1))
mod <- cmdstan_model(stan_file = my_stan_program)

# these are all valid
ok_arg_values <- list(
  data = my_data_list,
  num_chains = 2,
  num_warmup = 50,
  num_samples = 100,
  save_warmup = FALSE,
  thin = 2,
  refresh = 5,
  init = NULL,
  seed = 12345,
  max_depth = 6,
  metric = "dense_e",
  stepsize = 1.1,
  adapt_engaged = TRUE,
  adapt_delta = 0.7
)

# using any one of these should cause an error
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
  adapt_delta = 2
)

bad_arg_values_2 <- list(
  init = "NOT_A_FILE",
  seed = 1:10,
  stepsize = 1:10,
  metric = c("AA", "BB", "CC")
)

# mod$compile()

test_that("compile() method works", {
  skip_on_cran()
  # skip_on_travis()
  out <- capture.output(mod$compile())
  expect_output(print(out), "Running make")
})

test_that("object initializes correctly", {
  skip_on_cran()
  # skip_on_travis()
  expect_equal(mod$exe_file(), strip_ext(my_stan_program))
  expect_equal(mod$stan_file(), my_stan_program)
})

test_that("code() and print() methods work", {
  skip_on_cran()
  # skip_on_travis()
  expect_known_output(mod$print(), file = test_path("answers", "model-print-output"))
  expect_known_output(cat(mod$code()), file = test_path("answers", "model-code-output"))
})

test_that("sample() method doesn't error with data list", {
  skip_on_cran()
  # skip_on_travis()
  capture.output(fit <- mod$sample(data = my_data_list, num_chains = 1))
  expect_is(fit, "CmdStanMCMC")
})

test_that("sample() method doesn't error with data file", {
  skip_on_cran()
  # skip_on_travis()
  capture.output(fit <- mod$sample(data = my_data_file, num_chains = 1))
  expect_is(fit, "CmdStanMCMC")
})

test_that("sample() method runs when all arguments specified validly", {
  skip_on_cran()
  # skip_on_travis()
  capture.output(fit <- do.call(mod$sample, ok_arg_values))
  expect_is(fit, "CmdStanMCMC")
})

test_that("sample() method errors for invalid arguments before calling cmdstan", {
  skip_on_cran()
  # skip_on_travis()

  capture.output(mod$compile())
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

