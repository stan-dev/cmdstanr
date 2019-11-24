context("read-sample-csv")

if (not_on_cran()) {

}

test_that("read_sample_csv() fails for different stan version", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-3-diff_stan_version.csv"))
  expect_error(read_sample_csv(csv_files),
               "Supplied CSV files were not generated with the same version of Cmdstan!")
})

test_that("read_sample_csv() fails for different model names", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-3-diff_name.csv"))
  expect_error(read_sample_csv(csv_files),
               "Supplied CSV files were not generated wtih the same model!")
})

test_that("read_sample_csv() fails for different data file names names", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-3-diff_data_file.csv"))
  expect_error(read_sample_csv(csv_files),
               "Supplied CSV files have samples from chains run with non-matching data!")
})

test_that("read_sample_csv() fails for different sampling settings", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-3-diff_args.csv"))
  expect_error(read_sample_csv(csv_files),
               "Supplied CSV files do not match in all sampling settings!")
})

test_that("read_sample_csv() fails for different parameters", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-3-diff_params.csv"))
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
  csv_files <- c(test_path("resources", "csv", "model1-1-nonsampling.csv"))
  expect_error(read_sample_csv(csv_files),
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

test_that("read_sample_csv() fails with the number of samples found not matchin sampling arguments", {
  skip_on_cran()
  file_path <- test_path("resources", "csv", "model1-3-no-samples.csv")
  expect_error(read_sample_csv(file_path),
               "Supplied CSV file is corrupt. The number of samples does not match the sampling arguments!")
  file_path <- test_path("resources", "csv", "model1-3-diff_params.csv")
  expect_error(read_sample_csv(file_path),
               "Supplied CSV file is corrupt. The number of samples does not match the sampling arguments!")
})

test_that("read_sample_csv() matches rstan::read_stan_csv()", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-2-warmup.csv"))

  draws_array <- readRDS(test_path("answers", "rstan-read-stan-csv-no-warmup.rds"))
  draws_array <- posterior::as_draws_array(draws_array)
  csv_output <- read_sample_csv(csv_files)
  expect_equal(csv_output$post_warmup[,, "mu"],
               draws_array[,,"mu"])
  expect_equal(csv_output$post_warmup[,, "sigma"],
               draws_array[,,"sigma"])
  expect_equal(csv_output$post_warmup[,, "lp__"],
               draws_array[,,"lp__"])
})

test_that("read_sample_csv() matches rstan::read_stan_csv() with save_warmup", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-2-warmup.csv"))

  draws_array <- readRDS(test_path("answers", "rstan-read-stan-csv-warmup.rds"))
  draws_array <- posterior::as_draws_array(draws_array)
  csv_output <- read_sample_csv(csv_files)

  warmup_iter <- csv_output$sampling_info$sample_num_warmup
  all_iter <- warmup_iter + csv_output$sampling_info$sample_num_samples

  draws_array_post_warmup <- draws_array[(warmup_iter+1):all_iter,,]
  draws_array_warmup <- draws_array[1:warmup_iter,,]

  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup, "mu"),
               posterior::extract_one_variable_matrix(draws_array_post_warmup, "mu"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup, "sigma"),
               posterior::extract_one_variable_matrix(draws_array_post_warmup, "sigma"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup, "lp__"),
               posterior::extract_one_variable_matrix(draws_array_post_warmup, "lp__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup, "mu"),
               posterior::extract_one_variable_matrix(draws_array_warmup, "mu"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup, "sigma"),
               posterior::extract_one_variable_matrix(draws_array_warmup, "sigma"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$warmup, "lp__"),
               posterior::extract_one_variable_matrix(draws_array_warmup, "lp__"))
})

test_that("read_sample_csv() matches rstan::read_stan_csv() for csv file without warmup", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-3-no-warmup.csv"))

  draws_array <- readRDS(test_path("answers", "rstan-read-stan-csv-no-warmup-file.rds"))
  draws_array <- posterior::as_draws_array(draws_array)
  csv_output <- read_sample_csv(csv_files)

  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup, "mu"),
               posterior::extract_one_variable_matrix(draws_array, "mu"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup, "sigma"),
               posterior::extract_one_variable_matrix(draws_array, "sigma"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$post_warmup, "lp__"),
               posterior::extract_one_variable_matrix(draws_array, "lp__"))
})

test_that("read_sample_csv() returns correct diagonal of inverse mass matrix", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-3-no-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  expect_equal(as.vector(csv_output$inverse_mass_matrix_diag),
               c(0.909635, 0.066384))
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),test_path("resources", "csv", "model1-2-warmup.csv"))
  csv_output <- read_sample_csv(csv_files)
  expect_equal(as.vector(csv_output$inverse_mass_matrix_diag),
               c(1.00098, 0.068748,
                 0.909635, 0.066384))
})

test_that("read_sample_csv() matches rstan::read_stan_csv() for csv file without warmup", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-warmup.csv"))

  sampler_params <- readRDS(test_path("answers", "rstan-read-stan-csv-sampler-params.rds"))
  sampler_params <- posterior::as_draws_array(sampler_params[[1]])
  csv_output <- read_sample_csv(csv_files)
  expect_equal(posterior::extract_one_variable_matrix(csv_output$sampler, "divergent__"),
               posterior::extract_one_variable_matrix(sampler_params, "divergent__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$sampler, "accept_stat__"),
               posterior::extract_one_variable_matrix(sampler_params, "accept_stat__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$sampler, "treedepth__"),
               posterior::extract_one_variable_matrix(sampler_params, "treedepth__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$sampler, "stepsize__"),
               posterior::extract_one_variable_matrix(sampler_params, "stepsize__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$sampler, "n_leapfrog__"),
               posterior::extract_one_variable_matrix(sampler_params, "n_leapfrog__"))
  expect_equal(posterior::extract_one_variable_matrix(csv_output$sampler, "energy__"),
               posterior::extract_one_variable_matrix(sampler_params, "energy__"))
})


