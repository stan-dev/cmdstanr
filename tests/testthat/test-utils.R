context("utils")

test_that("check_divergences() works", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  output <- "14 of 100 \\(14%\\) transitions ended with a divergence."
  expect_message(check_divergences(csv_output), output)
})

test_that("check_sampler_transitions_treedepth() works", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  output <- "16 of 100 \\(16%\\) transitions hit the maximum treedepth limit of 5 or 2\\^5-1 leapfrog steps."
  expect_message(check_sampler_transitions_treedepth(csv_output), output)
})
