context("cmdstanr_example")

test_that("cmdstanr_example works", {
  skip_on_cran()
  fit_mcmc <- cmdstanr_example("logistic", chains = 2)
  checkmate::expect_r6(fit_mcmc, "CmdStanMCMC")
  expect_equal(fit_mcmc$num_chains(), 2)

  expect_output(cmdstanr_example("logistic", chains = 2, quiet = FALSE),
                "Running MCMC with 2 sequential chains")

  fit_mle <- cmdstanr_example("logistic", method = "optimize")
  checkmate::expect_r6(fit_mle, "CmdStanMLE")

  fit_vb <- cmdstanr_example("logistic", method = "variational")
  checkmate::expect_r6(fit_vb, "CmdStanVB")

  expect_output(print_example_program("schools"), "vector[J] theta", fixed=TRUE)
  expect_output(print_example_program("schools_ncp"), "vector[J] theta_raw", fixed=TRUE)
})


# used in multiple tests below
stan_program <- "
  data {
    int<lower=0> N;
    int<lower=0,upper=1> y[N];
  }
  parameters {
    real<lower=0,upper=1> theta;
  }
  model {
    y ~ bernoulli(theta);
  }
  "

test_that("write_stan_file writes Stan file correctly", {
  f1 <- write_stan_file(stan_program)
  checkmate::expect_file_exists(f1, extension = "stan")
  f1_lines <- readLines(f1)

  f2 <- write_stan_file(f1_lines)
  checkmate::expect_file_exists(f2, extension = "stan")
  f2_lines <- readLines(f2)

  expect_identical(f1_lines, f2_lines)
})

test_that("write_stan_file writes to specified directory and filename", {
  dir <- file.path(test_path(), "answers")

  expect_equal(dirname(f1 <- write_stan_file(stan_program, dir = dir)), dir)
  expect_equal(f2 <- write_stan_file(stan_program, dir = dir, basename = "fruit.stan"),
               file.path(dir, "fruit.stan"))
  expect_equal(f3 <- write_stan_file(stan_program, dir = dir, basename = "vegetable"),
               file.path(dir, "vegetable.stan")) # should add .stan extension if missing
  expect_equal(f4 <- write_stan_file(stan_program, dir = tempdir(), basename = "test"),
               file.path(tempdir(), "test.stan"))

  try(file.remove(f1, f2, f3, f4), silent = TRUE)
})

test_that("write_stan_file creates dir if necessary", {
  expect_match(
    write_stan_file(stan_program, file.path(tempdir(), "foo"), basename = "bar"),
    "/foo/bar.stan"
  )
})

test_that("write_stan_file by default creates the same file for the same Stan model", {
  dir <- file.path(test_path(), "answers")

  f1 <- write_stan_file(stan_program, dir = dir)
  mtime1 <- file.info(f1)$mtime

  f2 <- write_stan_file(paste0(stan_program, "\n\n"), dir = dir)
  expect_true(f1 != f2)

  # Test that writing the some model will not touch the file
  # Wait a tiny bit to make sure the modified time will be different if
  # overwrite happened
  Sys.sleep(0.001)
  f3 <- write_stan_file(stan_program, dir = dir)
  expect_equal(f1, f3)

  mtime3 <- file.info(f3)$mtime
  expect_equal(mtime1, mtime3)

  # f4 <- write_stan_file(stan_program, hash_salt = "aaa")
  # expect_true(f1 != f4)

  try(file.remove(f1, f2, f3, f4), silent = TRUE)
})

test_that("write_stan_tempfile is deprecated", {
  expect_warning(write_stan_tempfile(stan_program), "deprecated")
})
