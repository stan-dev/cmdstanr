context("read-sample-csv")

if (not_on_cran()) {
  set_cmdstan_path()
  fit_bernoulli_optimize <- testing_fit("bernoulli", method = "optimize")
  fit_bernoulli_diag_e_no_samples <- testing_fit("bernoulli", method = "sample",
                          seed = 123, num_chains = 2, num_samples = 0, metric = "diag_e")
  fit_bernoulli_dense_e_no_samples <- testing_fit("bernoulli", method = "sample",
                          seed = 123, num_chains = 2, num_samples = 0, metric = "dense_e")
  fit_bernoulli_thin_1 <- testing_fit("bernoulli", method = "sample",
                          seed = 123, num_chains = 2, num_samples = 1000, num_warmup = 1000, thin = 1)
  fit_logistic_thin_1 <- testing_fit("logistic", method = "sample",
                          seed = 123, num_chains = 2, num_samples = 1000, num_warmup = 1000, thin = 1)
  fit_logistic_thin_1_with_warmup <- testing_fit("logistic", method = "sample",
                          seed = 123, num_chains = 2, num_samples = 1000, num_warmup = 1000, thin = 1, save_warmup = 1)
  fit_logistic_thin_10 <- testing_fit("logistic", method = "sample",
                          seed = 123, num_chains = 2, num_samples = 1000, num_warmup = 1000, thin = 10, save_warmup = 0)
  fit_logistic_thin_10_with_warmup <- testing_fit("logistic", method = "sample",
                          seed = 123, num_chains = 2, num_samples = 1000, num_warmup = 1000, thin = 10, save_warmup = 1)
}

test_that("read_sample_csv() fails for different model names", {
  skip_on_cran()
  csv_files <- c(fit_bernoulli_thin_1$output_files(),
                 fit_logistic_thin_1$output_files())
  expect_error(read_sample_csv(csv_files),
               "Supplied CSV files were not generated wtih the same model!")
})

test_that("read_sample_csv() fails for different sampling settings", {
  skip_on_cran()
  csv_files <- c(fit_logistic_thin_1$output_files(),
                 fit_logistic_thin_10$output_files())
  expect_error(read_sample_csv(csv_files),
               "Supplied CSV files do not match in all sampling settings!")
})

test_that("read_sample_csv() fails for different parameters", {
  skip_on_cran()
  csv_files <- c(fit_bernoulli_thin_1$output_files(),
                 test_path("resources", "csv", "bernoulli-3-diff_params.csv"))
  expect_error(read_sample_csv(csv_files),
               "Supplied CSV files have samples for different parameters!")
})

test_that("read_sample_csv() fails if the file does not exist", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-doesntexist.csv"))
  expect_error(read_sample_csv(csv_files),
               "Assertion on 'output_file' failed: File does not exist: 'resources/csv/model1-1-doesntexist.csv'.")
})

test_that("read_sample_csv() fails for non-sampling csv", {
  skip_on_cran()
  expect_error(read_sample_csv(fit_bernoulli_optimize$output_files()),
               "Supplied CSV file was not generated with sampling. Consider using read_optim_csv or read_vb_csv!")
})

test_that("read_sample_csv() fails with empty csv file", {
  skip_on_cran()
  file_path <- test_path("resources", "csv", "empty.csv")
  file.create(file_path)
  expect_error(read_sample_csv(file_path),
               "Supplied CSV file is corrupt!")
  file.remove(file_path)
})

test_that("read_sample_csv() fails with the no params listed", {
  skip_on_cran()
  file_path <- test_path("resources", "csv", "model1-3-no-params.csv")
  expect_error(read_sample_csv(file_path),
               "no lines available in input")
})

test_that("read_sample_csv() matches rstan::read_stan_csv()", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-2-warmup.csv"))

  draws_array <- readRDS(test_path("answers", "rstan-read-stan-csv-no-warmup.rds"))
  draws_array <- posterior::as_draws_array(draws_array)
  csv_output <- read_sample_csv(csv_files)
  expect_equal(csv_output$post_warmup_draws[,, "mu"],
               draws_array[,,"mu"])
  expect_equal(csv_output$post_warmup_draws[,, "sigma"],
               draws_array[,,"sigma"])
  expect_equal(csv_output$post_warmup_draws[,, "lp__"],
               draws_array[,,"lp__"])
})

test_that("read_sample_csv() matches rstan::read_stan_csv() with save_warmup", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-2-warmup.csv"))

  draws_array <- readRDS(test_path("answers", "rstan-read-stan-csv-warmup.rds"))
  draws_array <- posterior::as_draws_array(draws_array)
  csv_output <- read_sample_csv(csv_files)

  warmup_iter <- csv_output$sampling_info$num_warmup
  num_iter <- csv_output$sampling_info$num_samples+csv_output$sampling_info$num_warmup

  draws_array_post_warmup <- draws_array[(warmup_iter+1):num_iter,,]
  draws_array_warmup <- draws_array[1:warmup_iter,,]

  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_draws, "mu"),
               posterior::extract_one_variable_matrix(draws_array_post_warmup, "mu"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_draws, "sigma"),
               posterior::extract_one_variable_matrix(draws_array_post_warmup, "sigma"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_draws, "lp__"),
               posterior::extract_one_variable_matrix(draws_array_post_warmup, "lp__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup_draws, "mu"),
               posterior::extract_one_variable_matrix(draws_array_warmup, "mu"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup_draws, "sigma"),
               posterior::extract_one_variable_matrix(draws_array_warmup, "sigma"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup_draws, "lp__"),
               posterior::extract_one_variable_matrix(draws_array_warmup, "lp__"))
})

test_that("read_sample_csv() matches rstan::read_stan_csv() for csv file without warmup", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))

  draws_array <- readRDS(test_path("answers", "rstan-read-stan-csv-no-warmup-file.rds"))
  draws_array <- posterior::as_draws_array(draws_array)
  csv_output <- read_sample_csv(csv_files)

  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_draws, "mu"),
               posterior::extract_one_variable_matrix(draws_array, "mu"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_draws, "sigma"),
               posterior::extract_one_variable_matrix(draws_array, "sigma"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_draws, "lp__"),
               posterior::extract_one_variable_matrix(draws_array, "lp__"))
})

test_that("read_sample_csv() returns correct diagonal of inverse mass matrix", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  expect_equal(as.vector(csv_output$inverse_metric[[2]]),
               c(0.909635, 0.066384))
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),test_path("resources", "csv", "model1-2-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  expect_equal(as.vector(csv_output$inverse_metric[[1]]),
               c(1.00098, 0.068748))
  expect_equal(as.vector(csv_output$inverse_metric[[2]]),
               c(0.909635, 0.066384))
})

test_that("read_sample_csv() returns correct dense inverse mass matrix", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-dense_e_metric.csv"))
  csv_output <- read_sample_csv(csv_files)
  expect_equal(as.vector(csv_output$inverse_metric[[1]]),
               c(10.2742, -0.189148, 5.92065, 8.2658, 10.9931, 8.67196, 9.75007, 8.30008, 6.3396, 8.75422,
                -0.189148, 0.552614, 2.28054, 0.587285, -0.557112, 0.0689745, -1.06614, -0.502288, 1.49863, 0.450733,
                5.92065, 2.28054, 52.7011, 7.89278, 3.9639, 5.71556, 2.16445, 1.88834, 13.8962, 15.1166,
                8.2658, 0.587285, 7.89278, 30.7924, 8.39762, 8.51489, 8.27253, 9.96521, 7.58758, 7.8403,
                10.9931, -0.557112, 3.9639, 8.39762, 39.6164, 9.88103, 9.62453, 9.80744, 6.28594, 6.77034,
                8.67196, 0.0689745, 5.71556, 8.51489, 9.88103, 29.6702, 8.5937, 8.3289, 8.76294, 5.63637,
                9.75007, -1.06614, 2.16445, 8.27253, 9.62453, 8.5937, 26.3294, 10.1908, 2.76266, 4.56938,
                8.30008, -0.502288, 1.88834, 9.96521, 9.80744, 8.3289, 10.1908, 26.5846, 3.7247, 4.26521,
                6.3396, 1.49863, 13.8962, 7.58758, 6.28594, 8.76294, 2.76266, 3.7247, 28.8872, 8.43035,
                8.75422, 0.450733, 15.1166, 7.8403, 6.77034, 5.63637, 4.56938, 4.26521, 8.43035, 42.5438))
})

test_that("read_sample_csv() returns correct dense inverse mass matrix for 2 csv files ", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-dense_e_metric.csv"),
                 test_path("resources", "csv", "model1-2-dense_e_metric.csv"))
  csv_output <- read_sample_csv(csv_files)
  expect_equal(as.vector(csv_output$inverse_metric[[1]]),
             c(10.2742, -0.189148, 5.92065, 8.2658, 10.9931, 8.67196, 9.75007, 8.30008, 6.3396, 8.75422,
                -0.189148, 0.552614, 2.28054, 0.587285, -0.557112, 0.0689745, -1.06614, -0.502288, 1.49863, 0.450733,
                5.92065, 2.28054, 52.7011, 7.89278, 3.9639, 5.71556, 2.16445, 1.88834, 13.8962, 15.1166,
                8.2658, 0.587285, 7.89278, 30.7924, 8.39762, 8.51489, 8.27253, 9.96521, 7.58758, 7.8403,
                10.9931, -0.557112, 3.9639, 8.39762, 39.6164, 9.88103, 9.62453, 9.80744, 6.28594, 6.77034,
                8.67196, 0.0689745, 5.71556, 8.51489, 9.88103, 29.6702, 8.5937, 8.3289, 8.76294, 5.63637,
                9.75007, -1.06614, 2.16445, 8.27253, 9.62453, 8.5937, 26.3294, 10.1908, 2.76266, 4.56938,
                8.30008, -0.502288, 1.88834, 9.96521, 9.80744, 8.3289, 10.1908, 26.5846, 3.7247, 4.26521,
                6.3396, 1.49863, 13.8962, 7.58758, 6.28594, 8.76294, 2.76266, 3.7247, 28.8872, 8.43035,
                8.75422, 0.450733, 15.1166, 7.8403, 6.77034, 5.63637, 4.56938, 4.26521, 8.43035, 42.5438))
  expect_equal(as.vector(csv_output$inverse_metric[[2]]),
             c( 11.08, -0.305763, 5.27013, 7.33046, 7.31263, 6.93229, 10.1923, 7.46852, 7.51557, 7.78791,
                -0.305763, 0.678461, 1.70598, 0.337143, -0.69887, 0.423236, -0.974023, -0.605539, 1.83794, 0.0780934,
                5.27013, 1.70598, 36.2726, 7.9386, 3.88642, 12.0214, 4.2487, 3.84886, 12.9738, 4.34037,
                7.33046, 0.337143, 7.9386, 23.9878, 4.93047, 5.76139, 7.34008, 7.78631, 6.79063, 8.13132,
                7.31263, -0.69887, 3.88642, 4.93047, 25.3662, 7.2269, 7.5408, 8.18066, 5.77239, 7.53072,
                6.93229, 0.423236, 12.0214, 5.76139, 7.2269, 24.0619, 7.24315, 7.194, 10.1647, 5.61617,
                10.1923, -0.974023, 4.2487, 7.34008, 7.5408, 7.24315, 26.2403, 6.91029, 1.26095, 4.72335,
                7.46852, -0.605539, 3.84886, 7.78631, 8.18066, 7.194, 6.91029, 30.2862, 5.61914, 8.10162,
                7.51557, 1.83794, 12.9738, 6.79063, 5.77239, 10.1647, 1.26095, 5.61914, 34.5498, 7.75486,
                7.78791, 0.0780934, 4.34037, 8.13132, 7.53072, 5.61617, 4.72335, 8.10162, 7.75486, 35.6602))
})

test_that("read_sample_csv() matches rstan::read_stan_csv() for csv file", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-warmup.csv"))

  sampler_diagnostics <- readRDS(test_path("answers", "rstan-read-stan-csv-sampler-params.rds"))
  sampler_diagnostics <- posterior::as_draws_array(sampler_diagnostics[[1]])
  csv_output <- read_sample_csv(csv_files)
  num_warmup <- csv_output$sampling_info$num_warmup/csv_output$sampling_info$thin
  if(csv_output$sampling_info$save_warmup) {
    num_iter <- csv_output$sampling_info$num_samples + csv_output$sampling_info$num_warmup
  } else {
    num_iter <- csv_output$sampling_info$num_samples
  }

  # match warmup sampler info
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup_sampler_diagnostics, "divergent__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[1:num_warmup,,], "divergent__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup_sampler_diagnostics, "accept_stat__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[1:num_warmup,,], "accept_stat__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup_sampler_diagnostics, "treedepth__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[1:num_warmup,,], "treedepth__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup_sampler_diagnostics, "stepsize__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[1:num_warmup,,], "stepsize__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup_sampler_diagnostics, "n_leapfrog__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[1:num_warmup,,], "n_leapfrog__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup_sampler_diagnostics, "energy__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[1:num_warmup,,], "energy__"))
  # match post-warmup sampling info
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_sampler_diagnostics, "divergent__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[(num_warmup+1):num_iter,,], "divergent__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_sampler_diagnostics, "accept_stat__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[(num_warmup+1):num_iter,,], "accept_stat__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_sampler_diagnostics, "treedepth__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[(num_warmup+1):num_iter,,], "treedepth__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_sampler_diagnostics, "stepsize__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[(num_warmup+1):num_iter,,], "stepsize__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_sampler_diagnostics, "n_leapfrog__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[(num_warmup+1):num_iter,,], "n_leapfrog__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup_sampler_diagnostics, "energy__"),
               posterior::extract_one_variable_matrix(sampler_diagnostics[(num_warmup+1):num_iter,,], "energy__"))
})

test_that("read_sample_csv() works with thin", {
  skip_on_cran()

  csv_output_1 <- read_sample_csv(fit_logistic_thin_1$output_files())
  csv_output_10 <- read_sample_csv(fit_logistic_thin_10$output_files())
  csv_output_1_with_warmup <- read_sample_csv(fit_logistic_thin_1_with_warmup$output_files())
  csv_output_10_with_warmup <- read_sample_csv(fit_logistic_thin_10_with_warmup$output_files())
  expect_equal(dim(csv_output_1$post_warmup_draws), c(1000, 2, 5))
  expect_equal(dim(csv_output_10$post_warmup_draws), c(100, 2, 5))
  expect_equal(dim(csv_output_1_with_warmup$post_warmup_draws), c(1000, 2, 5))
  expect_equal(dim(csv_output_1_with_warmup$warmup_draws), c(1000, 2, 5))
  expect_equal(dim(csv_output_10_with_warmup$post_warmup_draws), c(100, 2, 5))
  expect_equal(dim(csv_output_10_with_warmup$warmup_draws), c(100, 2, 5))
})

test_that("read_sample_csv() works with no samples", {
  skip_on_cran()

  csv_output_diag_e_0 <- read_sample_csv(fit_bernoulli_diag_e_no_samples$output_files())
  expect_equal(dim(csv_output_diag_e_0$post_warmup_draws), c(0,2,2))
  csv_output_dense_e_0 <- read_sample_csv(fit_bernoulli_dense_e_no_samples$output_files())
  expect_equal(dim(csv_output_dense_e_0$post_warmup_draws), c(0,2,2))
})

test_that("read_sample_csv() reads values up to adaptation", {
  skip_on_cran()

  csv_files <- test_path("resources", "csv", "bernoulli-3-diff_params.csv")

  csv_out <- read_sample_csv(csv_files)
  expect_equal(csv_out$sampling_info$pi, 3.14)
  expect_true(is.null(csv_out$sampling_info$pi_square))
})
