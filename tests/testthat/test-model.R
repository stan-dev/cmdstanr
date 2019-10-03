context("CmdStanModel")

set_cmdstan_path("/Users/jgabry/Documents/Stan/cmdstan-2.20.0")
my_stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.R")
my_data_list <- list(N = 10, y =c(0,1,0,0,0,0,0,0,0,1))


mod <- cmdstan_model(stan_file = my_stan_program)

# mod$compile()

test_that("compile() method works", {
  skip_on_cran()
  skip_on_travis()
  expect_output(mod$compile(), "Running make")
})

test_that("object initializes correctly", {
  skip_on_cran()
  skip_on_travis()
  expect_equal(mod$exe_file, strip_ext(my_stan_program))
  expect_equal(mod$stan_file, my_stan_program)
})

test_that("code() and print() methods work", {
  skip_on_cran()
  skip_on_travis()
  expect_known_output(mod$print(), file = test_path("answers", "model-print-output"))
  expect_known_output(cat(mod$code()), file = test_path("answers", "model-code-output"))
})

test_that("sample() method doesn't error with data list", {
  skip_on_cran()
  skip_on_travis()
  capture.output(fit <- mod$sample(data = my_data_list, num_chains = 1))
  expect_is(fit, "CmdStanFit")
})

test_that("sample() method doesn't error with data file", {
  skip_on_cran()
  skip_on_travis()
  capture.output(fit <- mod$sample(data = my_data_file, num_chains = 1))
  expect_is(fit, "CmdStanFit")
})

# test_that("sample() method handles arguments correctly", {
#   arg_values <- list(
#     data = NULL,
#     num_chains = NULL,
#     num_warmup = NULL,
#     num_samples = NULL,
#     save_warmup = FALSE,
#     thin = NULL,
#     refresh = NULL,
#     inits = NULL,
#     seed = NULL,
#     max_depth = NULL,
#     metric = NULL,
#     stepsize = NULL,
#     adapt_engaged = NULL,
#     adapt_delta = NULL
#   )
# })
