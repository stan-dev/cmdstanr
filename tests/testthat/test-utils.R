context("utils")

if (not_on_cran()) {
  set_cmdstan_path()
  fit_mcmc <- testing_fit("logistic", method = "sample",
                          seed = 123, num_chains = 2)
}

test_that("check_divergences() works", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  output <- "14 of 100 \\(14.0%\\) transitions ended with a divergence."
  expect_message(check_divergences(csv_output), output)

  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"),
                 test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  output <- "28 of 200 \\(14.0%\\) transitions ended with a divergence."
  expect_message(check_divergences(csv_output), output)

  csv_files <- c(test_path("resources", "csv", "model1-2-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  output <- "1 of 100 \\(1.0%\\) transitions ended with a divergence."
  expect_message(check_divergences(csv_output), output)
})

test_that("check_sampler_transitions_treedepth() works", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  output <- "16 of 100 \\(16.0%\\) transitions hit the maximum treedepth limit of 5 or 2\\^5-1 leapfrog steps."
  expect_message(check_sampler_transitions_treedepth(csv_output), output)

  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"),
                 test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  output <- "32 of 200 \\(16.0%\\) transitions hit the maximum treedepth limit of 5 or 2\\^5-1 leapfrog steps."
  expect_message(check_sampler_transitions_treedepth(csv_output), output)

  csv_files <- c(test_path("resources", "csv", "model1-2-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  output <- "1 of 100 \\(1.0%\\) transitions hit the maximum treedepth limit of 5 or 2\\^5-1 leapfrog steps."
  expect_message(check_sampler_transitions_treedepth(csv_output), output)

})

test_that("cmdstan_summary works if bin/stansummary deleted file", {
  skip_on_cran()
  delete_and_run <- function() {
    file.remove(file.path(cmdstan_path(), "bin", cmdstan_ext("stansummary")))
    fit_mcmc$cmdstan_summary()
  }
  expect_output(delete_and_run(), "Inference for Stan model: logistic_model\\n2 chains: each with iter")
})

test_that("cmdstan_diagnose works if bin/diagnose deleted file", {
  skip_on_cran()
  delete_and_run <- function() {
    file.remove(file.path(cmdstan_path(), "bin", cmdstan_ext("diagnose")))
    fit_mcmc$cmdstan_diagnose()
  }
  expect_output(delete_and_run(), "Checking sampler transitions treedepth")
})

test_that("repair_path() fixes slashes", {
  # all slashes should be single "/", and no trailing slash
  expect_equal(repair_path("a//b\\c/"), "a/b/c")
})

test_that("repair_path works with zero length path or non-string path", {
  expect_equal(repair_path(""), "")
  expect_equal(repair_path(5), 5)
})

test_that("list_to_array works with empty list", {
  expect_equal(list_to_array(list()), NULL)
})

test_that("list_to_array fails for non-numeric values", {
  expect_error(list_to_array(list(k = "test")), "All elements of the list must be numeric!")
})

test_that("cpp_options_to_compile_flags() works", {
  options = list(
    stan_threads = TRUE
  )
  expect_equal(cpp_options_to_compile_flags(options), "STAN_THREADS=TRUE")
  options = list(
    stan_threads = TRUE,
    stanc2 = TRUE
  )
  expect_equal(cpp_options_to_compile_flags(options), "STAN_THREADS=TRUE STANC2=TRUE")
  options = list()
  expect_equal(cpp_options_to_compile_flags(options), NULL)
})
