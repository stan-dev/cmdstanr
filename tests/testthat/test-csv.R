context("read_cmdstan_csv")

if (not_on_cran()) {
  set_cmdstan_path()
  fit_bernoulli_optimize <- testing_fit("bernoulli", method = "optimize", seed = 1234)
  fit_bernoulli_variational <- testing_fit("bernoulli", method = "variational", seed = 123)
  fit_logistic_optimize <- testing_fit("logistic", method = "optimize", seed = 123)
  fit_logistic_variational <- testing_fit("logistic", method = "variational", seed = 123)
  fit_logistic_variational_short <- testing_fit("logistic", method = "variational", output_samples = 100, seed = 123)

  fit_bernoulli_diag_e_no_samples <- testing_fit("bernoulli", method = "sample",
                          seed = 123, chains = 2, iter_sampling = 0, metric = "diag_e")
  fit_bernoulli_dense_e_no_samples <- testing_fit("bernoulli", method = "sample",
                          seed = 123, chains = 2, iter_sampling = 0, metric = "dense_e")
  fit_bernoulli_dense_e_no_samples_warmup <- testing_fit("bernoulli", method = "sample", seed = 123, chains = 2,
                                                         iter_warmup = 100, iter_sampling = 0, save_warmup = 1,
                                                         metric = "dense_e")
  fit_bernoulli_thin_1 <- testing_fit("bernoulli", method = "sample",
                          seed = 123, chains = 2, iter_sampling = 1000, iter_warmup = 1000, thin = 1)
  fit_logistic_thin_1 <- testing_fit("logistic", method = "sample",
                          seed = 123, chains = 2, iter_sampling = 1000, iter_warmup = 1000, thin = 1)
  fit_logistic_thin_1a <- testing_fit("logistic", method = "sample",
                                     seed = 123, chains = 2, iter_sampling = 500, iter_warmup = 1000, thin = 1)
  fit_logistic_thin_1b <- testing_fit("logistic", method = "sample",
                                      seed = 123, chains = 2, iter_sampling = 1000, iter_warmup = 500, thin = 1)
  fit_logistic_thin_1_with_warmup <- testing_fit("logistic", method = "sample",
                          seed = 123, chains = 2, iter_sampling = 1000, iter_warmup = 1000, thin = 1, save_warmup = 1)
  fit_logistic_thin_3 <- testing_fit("logistic", method = "sample",
                          seed = 123, chains = 2, iter_sampling = 1000, iter_warmup = 1000, thin = 3)
  fit_logistic_thin_3_with_warmup <- testing_fit("logistic", method = "sample",
                          seed = 123, chains = 2, iter_sampling = 1000, iter_warmup = 1000, thin = 3, save_warmup = 1)
  fit_logistic_thin_10 <- testing_fit("logistic", method = "sample",
                          seed = 123, chains = 2, iter_sampling = 1000, iter_warmup = 1000, thin = 10, save_warmup = 0)
  fit_logistic_thin_10_with_warmup <- testing_fit("logistic", method = "sample",
                          seed = 123, chains = 2, iter_sampling = 1000, iter_warmup = 1000, thin = 10, save_warmup = 1)

  fit_gq <- testing_fit("bernoulli_ppc", method = "generate_quantities", seed = 123, fitted_params = fit_bernoulli_thin_1)
}

test_that("read_cmdstan_csv() fails for different model names", {
  skip_on_cran()
  csv_files <- c(fit_bernoulli_thin_1$output_files(),
                 fit_logistic_thin_1$output_files())
  expect_error(read_cmdstan_csv(csv_files),
               "Supplied CSV files were not generated with the same model!")
})

test_that("read_cmdstan_csv() fails for different number of samples in csv", {
  skip_on_cran()
  csv_files <- c(fit_logistic_thin_1$output_files(),
                 fit_logistic_thin_10$output_files())
  expect_error(read_cmdstan_csv(csv_files),
               "Supplied CSV files do not match in the number of output samples!")
  csv_files <- c(fit_logistic_thin_1$output_files(),
                 fit_logistic_thin_1a$output_files())
  expect_error(read_cmdstan_csv(csv_files),
               "Supplied CSV files do not match in the number of output samples!")
  csv_files <- c(fit_logistic_thin_1$output_files(),
                 fit_logistic_thin_1b$output_files())
  expect_warning(read_cmdstan_csv(csv_files),
               "Supplied CSV files do not match in the following arguments: iter_warmup")
  csv_files <- c(fit_logistic_thin_1$output_files(),
                 fit_logistic_thin_1_with_warmup$output_files())
  expect_error(read_cmdstan_csv(csv_files),
                 "Supplied CSV files do not match in the number of output samples!")
})

test_that("read_cmdstan_csv() fails for different variables", {
  skip_on_cran()
  csv_files <- c(fit_bernoulli_thin_1$output_files(),
                 test_path("resources", "csv", "bernoulli-3-diff_params.csv"))
  expect_error(read_cmdstan_csv(csv_files),
               "Supplied CSV files have samples for different variables!")
})

test_that("read_cmdstan_csv() fails if the file does not exist", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-doesntexist.csv"))
  expect_error(read_cmdstan_csv(csv_files),
               "Assertion on 'files' failed: File does not exist: 'resources/csv/model1-1-doesntexist.csv'.")
  expect_error(read_cmdstan_csv(NULL),
               "Assertion on 'files' failed: No file provided.")
  expect_error(read_cmdstan_csv(character(0)),
               "Assertion on 'files' failed: No file provided.")
})

test_that("read_cmdstan_csv() fails with empty csv file", {
  skip_on_cran()
  file_path <- test_path("resources", "csv", "empty.csv")
  file.create(file_path)
  expect_error(read_cmdstan_csv(file_path),
               "Supplied CSV file is corrupt!")
  file.remove(file_path)
})

test_that("read_cmdstan_csv() fails with the no params listed", {
  skip_on_cran()
  file_path <- test_path("resources", "csv", "model1-3-no-params.csv")
  expect_error(read_cmdstan_csv(file_path),
               "Supplied CSV file does not contain any variable names or data!")
})

test_that("read_cmdstan_csv() matches utils::read.csv", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),
                 test_path("resources", "csv", "model1-2-warmup.csv"))

  draws_array_1 <- utils::read.csv(test_path("resources", "csv", "model1-1-warmup.csv"), comment.char = "#")
  draws_array_2 <- utils::read.csv(test_path("resources", "csv", "model1-2-warmup.csv"), comment.char = "#")
  post_warmup_draws_array_1 <- posterior::as_draws_array(draws_array_1[101:200,,])
  post_warmup_draws_array_2 <- posterior::as_draws_array(draws_array_2[101:200,,])
  warmup_draws_array_1 <- posterior::as_draws_array(draws_array_1[1:100,,])
  warmup_draws_array_2 <- posterior::as_draws_array(draws_array_2[1:100,,])

  csv_output <- read_cmdstan_csv(csv_files)
  for (param in c("mu", "sigma", "lp__")) {
    expect_equal(posterior::subset_draws(csv_output$post_warmup_draws, chain = 1, variable = param),
                 post_warmup_draws_array_1[,,param])
    expect_equal(posterior::subset_draws(csv_output$warmup_draws, chain = 1, variable = param),
                 warmup_draws_array_1[,,param])
    expect_equal(posterior::subset_draws(csv_output$post_warmup_draws, chain = 2, variable = param),
                 post_warmup_draws_array_2[,,param])
    expect_equal(posterior::subset_draws(csv_output$warmup_draws, chain = 2, variable = param),
                 warmup_draws_array_2[,,param])
  }
  for (diagnostic in c("divergent__", "accept_stat__", "treedepth__", "stepsize__", "n_leapfrog__", "energy__")) {
    expect_equal(posterior::subset_draws(csv_output$post_warmup_sampler_diagnostics, chain = 1, variable = diagnostic),
                 post_warmup_draws_array_1[,,diagnostic])
    expect_equal(posterior::subset_draws(csv_output$warmup_sampler_diagnostics, chain = 1, variable = diagnostic),
                 warmup_draws_array_1[,,diagnostic])
    expect_equal(posterior::subset_draws(csv_output$post_warmup_sampler_diagnostics, chain = 2, variable = diagnostic),
                 post_warmup_draws_array_2[,,diagnostic])
    expect_equal(posterior::subset_draws(csv_output$warmup_sampler_diagnostics, chain = 2, variable = diagnostic),
                 warmup_draws_array_2[,,diagnostic])
  }
})

test_that("read_cmdstan_csv() matches utils::read.csv for csv file without warmup", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))

  draws_array <- utils::read.csv(test_path("resources", "csv", "model1-2-no-warmup.csv"), comment.char = "#")
  draws_array <- posterior::as_draws_array(draws_array)
  csv_output <- read_cmdstan_csv(csv_files)
  expect_equal(posterior::subset_draws(csv_output$post_warmup_draws, chain = 1, variable = "mu"),
               draws_array[,,"mu"])
  expect_equal(posterior::subset_draws(csv_output$post_warmup_draws, chain = 1, variable = "sigma"),
               draws_array[,,"sigma"])
  expect_equal(posterior::subset_draws(csv_output$post_warmup_draws, chain = 1, variable = "lp__"),
               draws_array[,,"lp__"])

})

test_that("read_cmdstan_csv() returns correct diagonal of inverse mass matrix", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  expect_equal(as.vector(csv_output$inv_metric[[as.character(2)]]),
               c(0.909635, 0.066384))
  csv_files <- c(test_path("resources", "csv", "model1-1-warmup.csv"),test_path("resources", "csv", "model1-2-warmup.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  expect_equal(as.vector(csv_output$inv_metric[[as.character(1)]]),
               c(1.00098, 0.00068748))
  expect_equal(as.vector(csv_output$inv_metric[[as.character(2)]]),
               c(0.909635, 0.066384))
})

test_that("read_cmdstan_csv() returns correct dense inverse mass matrix", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-dense_e_metric.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  expect_equal(as.vector(csv_output$inv_metric[[1]]),
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

test_that("read_cmdstan_csv() returns correct dense inverse mass matrix for 2 csv files ", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-dense_e_metric.csv"),
                 test_path("resources", "csv", "model1-2-dense_e_metric.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  expect_equal(as.vector(csv_output$inv_metric[[1]]),
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
  expect_equal(as.vector(csv_output$inv_metric[[2]]),
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

test_that("read_cmdstan_csv() works with thin", {
  skip_on_cran()

  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files())
  csv_output_3 <- read_cmdstan_csv(fit_logistic_thin_3$output_files())
  csv_output_10 <- read_cmdstan_csv(fit_logistic_thin_10$output_files())
  csv_output_1_with_warmup <- read_cmdstan_csv(fit_logistic_thin_1_with_warmup$output_files())
  csv_output_3_with_warmup <- read_cmdstan_csv(fit_logistic_thin_3_with_warmup$output_files())
  csv_output_10_with_warmup <- read_cmdstan_csv(fit_logistic_thin_10_with_warmup$output_files())
  expect_equal(dim(csv_output_1$post_warmup_draws), c(1000, 2, 5))
  expect_equal(dim(csv_output_3$post_warmup_draws), c(334, 2, 5))
  expect_equal(dim(csv_output_10$post_warmup_draws), c(100, 2, 5))
  expect_equal(dim(csv_output_1_with_warmup$post_warmup_draws), c(1000, 2, 5))
  expect_equal(dim(csv_output_1_with_warmup$warmup_draws), c(1000, 2, 5))
  expect_equal(dim(csv_output_3_with_warmup$post_warmup_draws), c(334, 2, 5))
  expect_equal(dim(csv_output_3_with_warmup$warmup_draws), c(334, 2, 5))
  expect_equal(dim(csv_output_10_with_warmup$post_warmup_draws), c(100, 2, 5))
  expect_equal(dim(csv_output_10_with_warmup$warmup_draws), c(100, 2, 5))
})

test_that("read_cmdstan_csv() works with filtered variables", {
  skip_on_cran()
  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = NULL, sampler_diagnostics = list())
  expect_equal(dim(csv_output_1$post_warmup_draws), c(1000, 2, 5))
  expect_equal(dim(csv_output_1$post_warmup_sampler_diagnostics), NULL)
  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = "", sampler_diagnostics = "")
  expect_equal(dim(csv_output_1$post_warmup_draws), NULL)
  expect_equal(dim(csv_output_1$post_warmup_sampler_diagnostics), NULL)
  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = "", sampler_diagnostics = NULL)
  expect_equal(dim(csv_output_1$post_warmup_draws), NULL)
  expect_equal(dim(csv_output_1$post_warmup_sampler_diagnostics), c(1000, 2, 6))
  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = c("lp__", "alpha"), sampler_diagnostics = "")
  expect_equal(dim(csv_output_1$post_warmup_draws), c(1000, 2, 2))
  expect_equal(dim(csv_output_1$post_warmup_sampler_diagnostics), NULL)
  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = c("lp__", "beta[1]"), sampler_diagnostics = "")
  expect_equal(dim(csv_output_1$post_warmup_draws), c(1000, 2, 2))
  expect_equal(dim(csv_output_1$post_warmup_sampler_diagnostics), NULL)
  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = c("lp__", "beta"), sampler_diagnostics = "")
  expect_equal(dim(csv_output_1$post_warmup_draws), c(1000, 2, 4))
  expect_equal(dim(csv_output_1$post_warmup_sampler_diagnostics), NULL)
  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = "", sampler_diagnostics = c("n_leapfrog__", "divergent__"))
  expect_equal(dim(csv_output_1$post_warmup_draws), NULL)
  expect_equal(dim(csv_output_1$post_warmup_sampler_diagnostics), c(1000, 2, 2))
  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = c("lp__", "alpha"), sampler_diagnostics = c("n_leapfrog__", "divergent__"))
  expect_equal(dim(csv_output_1$post_warmup_draws), c(1000, 2, 2))
  expect_equal(dim(csv_output_1$post_warmup_sampler_diagnostics), c(1000, 2, 2))
  expect_error(read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = c("NOPE"), sampler_diagnostics = list("n_leapfrog__", "divergent__")),
               "Can't find the following variable(s) in the output: NOPE",
               fixed = TRUE)
  expect_error(read_cmdstan_csv(fit_logistic_thin_1$output_files(), sampler_diagnostics = list("BAD_1", "BAD_2")),
               "Can't find the following sampler diagnostic(s) in the output: BAD_1, BAD_2",
               fixed = TRUE)
})

test_that("read_cmdstan_csv returned filtered variables in correct order", {
  csv_output_1 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = c("lp__", "beta[1]"), sampler_diagnostics = "")
  expect_equal(posterior::variables(csv_output_1$post_warmup_draws), c("lp__", "beta[1]"))
  csv_output_2 <- read_cmdstan_csv(fit_logistic_thin_1$output_files(), variables = c("beta[1]", "lp__"), sampler_diagnostics = "")
  expect_equal(posterior::variables(csv_output_2$post_warmup_draws), c("beta[1]", "lp__"))
})

test_that("read_cmdstan_csv() works with no samples", {
  skip_on_cran()

  csv_output_diag_e_0 <- read_cmdstan_csv(fit_bernoulli_diag_e_no_samples$output_files())
  expect_equal(csv_output_diag_e_0$post_warmup_draws, NULL)
  csv_output_dense_e_0 <- read_cmdstan_csv(fit_bernoulli_dense_e_no_samples$output_files())
  expect_equal(csv_output_dense_e_0$post_warmup_draws, NULL)
  csv_output_dense_e_0_w <- read_cmdstan_csv(fit_bernoulli_dense_e_no_samples_warmup$output_files())
  expect_equal(csv_output_dense_e_0_w$post_warmup_draws, NULL)
  csv_output_dense_e_0_w <- read_cmdstan_csv(fit_bernoulli_dense_e_no_samples_warmup$output_files())
  expect_equal(csv_output_dense_e_0_w$post_warmup_sampler_diagnostics, NULL)
  csv_output_dense_e_0_w <- read_cmdstan_csv(fit_bernoulli_dense_e_no_samples_warmup$output_files())
  expect_equal(dim(csv_output_dense_e_0_w$warmup_draws), c(100, 2, 2))
  csv_output_dense_e_0_w <- read_cmdstan_csv(fit_bernoulli_dense_e_no_samples_warmup$output_files())
  expect_equal(dim(csv_output_dense_e_0_w$warmup_sampler_diagnostics), c(100, 2, 6))
})

test_that("read_cmdstan_csv() reads values up to adaptation", {
  skip_on_cran()

  csv_files <- test_path("resources", "csv", "bernoulli-3-diff_params.csv")

  csv_out <- read_cmdstan_csv(csv_files)
  expect_equal(csv_out$metadata$pi, 3.14)
  expect_false(is.null(csv_out$metadata$pi_square))
})

test_that("remaining_columns_to_read() works", {
  expect_equal(remaining_columns_to_read(NULL, NULL, NULL), NULL)
  expect_equal(remaining_columns_to_read(NULL, c("a"), NULL), NULL)
  expect_equal(remaining_columns_to_read(NULL, NULL, c("a")), c("a"))
  expect_equal(remaining_columns_to_read(c("a"), c("a"), NULL), "")
  expect_equal(remaining_columns_to_read(c("a"), NULL, c("a")), c("a"))
  expect_equal(remaining_columns_to_read(c("a"), c("a", "b", "c"), NULL), "")
  expect_equal(remaining_columns_to_read(c("a"), NULL, c("a", "b", "c")), c("a"))
  expect_equal(remaining_columns_to_read(NULL, c("a", "b", "c"), NULL), NULL)
  expect_equal(remaining_columns_to_read(NULL, NULL, c("a", "b", "c")), c("a", "b", "c"))
  expect_equal(remaining_columns_to_read(c("a", "b", "c"), c("a", "b", "c"), NULL), "")
  expect_equal(remaining_columns_to_read(c(""), c("a", "b", "c"), NULL), "")
  expect_equal(remaining_columns_to_read(c("", "a"), c("a", "b", "c"), NULL), "")
  expect_equal(remaining_columns_to_read(c("a", "b", "c"), NULL, c("a", "b", "c")), c("a", "b", "c"))

  # with vector and matrix variables
  b_vec <- paste0("b[", 1:4, "]")
  c_mat <- paste0("c[", c(1,2,1,2), ",", c(1,1,2,2), "]")
  all_vars <- c("a", b_vec, c_mat)
  expect_equal(
    remaining_columns_to_read(c("a", "b[2]"), c("a", c_mat), all_vars),
    "b[2]"
  )
  expect_equal(
    remaining_columns_to_read(c("a", "b", "c"), c(paste0("b[", c(1,4), "]"), "c[1,2]"), all_vars),
    c("a", "b[2]", "b[3]", "c[1,1]", "c[2,1]", "c[2,2]")
  )
})

test_that("read_cmdstan_csv() reads adaptation step size correctly", {
  skip_on_cran()

  csv_files <- test_path("resources", "csv", "model1-2-no-warmup.csv")

  csv_out <- read_cmdstan_csv(csv_files)
  expect_equal(csv_out$step_size[[as.character(2)]], 0.672434)

  csv_files <- c(test_path("resources", "csv", "model1-1-dense_e_metric.csv"),
                 test_path("resources", "csv", "model1-2-dense_e_metric.csv"))

  csv_out <- read_cmdstan_csv(csv_files)
  expect_equal(csv_out$step_size[[as.character(1)]], 0.11757)
  expect_equal(csv_out$step_size[[as.character(2)]], 0.232778)
})

test_that("read_cmdstan_csv() works for optimize", {
  skip_on_cran()

  csv_output_1 <- read_cmdstan_csv(fit_bernoulli_optimize$output_files())
  csv_output_2 <- read_cmdstan_csv(fit_logistic_optimize$output_files())
  expect_equal(dim(csv_output_1$point_estimates), c(1, 2))
  expect_equal(dim(csv_output_2$point_estimates), c(1, 5))

  csv_file <- test_path("resources", "csv", "bernoulli-1-optimize.csv")
  csv_output_3 <- read_cmdstan_csv(csv_file)
  expect_equal(as.numeric(csv_output_3$point_estimates[1,"lp__"]), -12.2173)
  expect_equal(as.numeric(csv_output_3$point_estimates[1,"theta"]), 0.300001)

  # variable filtering
  csv_output_4 <- read_cmdstan_csv(fit_logistic_optimize$output_files(), variables = "beta")
  expect_equal(posterior::variables(csv_output_4$point_estimates), c("beta[1]", "beta[2]", "beta[3]"))
  csv_output_5 <- read_cmdstan_csv(fit_logistic_optimize$output_files(), variables = c("alpha", "lp__", "beta[2]"))
  expect_equal(posterior::variables(csv_output_5$point_estimates), c("alpha", "lp__", "beta[2]"))
})


test_that("read_cmdstan_csv() works for variational", {
  skip_on_cran()

  csv_output_1 <- read_cmdstan_csv(fit_bernoulli_variational$output_files())
  csv_output_2 <- read_cmdstan_csv(fit_logistic_variational$output_files())
  expect_equal(dim(csv_output_1$draws), c(1000, 3))
  expect_equal(dim(csv_output_2$draws), c(1000, 6))

  csv_file <- test_path("resources", "csv", "bernoulli-1-variational.csv")
  csv_output_3 <- read_cmdstan_csv(csv_file)
  expect_equal(as.numeric(csv_output_3$draws[1,"theta"]), 0.230751)
  expect_equal(dim(csv_output_3$draws), c(50, 3))
  expect_equal(csv_output_3$metadata$model_params, c("lp__", "lp_approx__", "theta"))

  # variable filtering
  csv_output_4 <- read_cmdstan_csv(fit_logistic_variational$output_files(), variables = "beta")
  expect_equal(posterior::variables(csv_output_4$draws), c("beta[1]", "beta[2]", "beta[3]"))
  csv_output_5 <- read_cmdstan_csv(fit_logistic_variational$output_files(), variables = c("alpha", "beta[2]"))
  expect_equal(posterior::variables(csv_output_5$draws), c("alpha", "beta[2]"))

  diff_samples_variational <- c(fit_logistic_variational$output_files(),
                                fit_logistic_variational_short$output_files())
  expect_error(
    read_cmdstan_csv(diff_samples_variational),
    "Supplied CSV files do not match in the number of output samples!"
  )
})

test_that("read_cmdstan_csv() works for generate_quantities", {
  skip_on_cran()

  csv_output_1 <- read_cmdstan_csv(fit_gq$output_files())
  expect_equal(dim(csv_output_1$generated_quantities), c(1000, 2, 11))
  y_rep_params <- c("y_rep[1]", "y_rep[2]", "y_rep[3]", "y_rep[4]", "y_rep[5]",
                    "y_rep[6]", "y_rep[7]", "y_rep[8]", "y_rep[9]", "y_rep[10]")
  csv_file <- test_path("resources", "csv", "bernoulli_ppc-1-gq.csv")
  csv_output_3 <- read_cmdstan_csv(csv_file)
  expect_equal(as.numeric(csv_output_3$generated_quantities[1,1,"y_rep[1]"]), 0)
  expect_equal(as.numeric(csv_output_3$generated_quantities[2,1,"y_rep[2]"]), 1)
  expect_equal(as.numeric(csv_output_3$generated_quantities[4,1,]), c(0,0,0,0,0,1,0,0,0,1))
  expect_equal(dim(csv_output_3$generated_quantities), c(5, 1, 10))
  expect_equal(csv_output_3$metadata$model_params, y_rep_params)

  # variable filtering
  csv_output_4 <- read_cmdstan_csv(fit_gq$output_files(), variables = "y_rep")
  expect_equal(posterior::variables(csv_output_4$generated_quantities), y_rep_params)
  csv_output_5 <- read_cmdstan_csv(fit_gq$output_files(), variables = c("sum_y", "y_rep"))
  expect_equal(posterior::variables(csv_output_5$generated_quantities), c("sum_y", y_rep_params))

  # metadata$fitted_params has correct number of files
  expect_length(csv_output_5$metadata$fitted_params, fit_gq$num_chains())
})

test_that("read_cmdstan_csv() errors for files from different methods", {
  skip_on_cran()
  files <- c(fit_bernoulli_variational$output_files(),fit_bernoulli_optimize$output_files())
  expect_error(
    read_cmdstan_csv(files),
    "Supplied CSV files were produced by different methods and need to be read in separately!"
  )
  files <- c(fit_bernoulli_thin_1$output_files(),fit_bernoulli_optimize$output_files())
  expect_error(
    read_cmdstan_csv(files),
    "Supplied CSV files were produced by different methods and need to be read in separately!"
  )
  files <- c(fit_bernoulli_variational$output_files(),fit_bernoulli_thin_1$output_files())
  expect_error(
    read_cmdstan_csv(files),
    "Supplied CSV files were produced by different methods and need to be read in separately!"
  )
  files <- c(fit_bernoulli_variational$output_files(),fit_bernoulli_optimize$output_files())
  expect_error(
    read_cmdstan_csv(files),
    "Supplied CSV files were produced by different methods and need to be read in separately!"
  )
})

test_that("stan_variables and stan_variable_sizes works in read_cdmstan_csv()", {
  skip_on_cran()
  bern_opt <- read_cmdstan_csv(fit_bernoulli_optimize$output_files())
  bern_vi <- read_cmdstan_csv(fit_bernoulli_variational$output_files())
  log_opt <- read_cmdstan_csv(fit_logistic_optimize$output_files())
  log_vi <- read_cmdstan_csv(fit_logistic_variational$output_files())
  bern_samp <- read_cmdstan_csv(fit_bernoulli_thin_1$output_files())
  log_samp <- read_cmdstan_csv(fit_logistic_thin_1$output_files())
  gq <- read_cmdstan_csv(fit_gq$output_files())

  expect_equal(bern_opt$metadata$stan_variables, c("lp__", "theta"))
  expect_equal(bern_vi$metadata$stan_variables, c("lp__", "lp_approx__", "theta"))
  expect_equal(bern_samp$metadata$stan_variables, c("lp__", "theta"))

  expect_equal(log_opt$metadata$stan_variables, c("lp__", "alpha", "beta"))
  expect_equal(log_vi$metadata$stan_variables, c("lp__", "lp_approx__", "alpha", "beta"))
  expect_equal(log_samp$metadata$stan_variables, c("lp__", "alpha", "beta"))

  expect_equal(gq$metadata$stan_variables, c("y_rep","sum_y"))

  expect_equal(bern_opt$metadata$stan_variable_sizes, list(lp__ = 1, theta = 1))
  expect_equal(bern_vi$metadata$stan_variable_sizes, list(lp__ = 1, lp_approx__ = 1, theta = 1))
  expect_equal(bern_samp$metadata$stan_variable_sizes, list(lp__ = 1, theta = 1))

  expect_equal(log_opt$metadata$stan_variable_sizes, list(lp__ = 1, alpha = 1, beta = 3))
  expect_equal(log_vi$metadata$stan_variable_sizes, list(lp__ = 1, lp_approx__ = 1, alpha = 1, beta = 3))
  expect_equal(log_samp$metadata$stan_variable_sizes, list(lp__ = 1, alpha = 1, beta = 3))

  expect_equal(gq$metadata$stan_variable_sizes, list(y_rep = 10, sum_y = 1))
})

test_that("returning time works for read_cmdstan_csv", {
  csv_files <- test_path("resources", "csv", "model1-2-no-warmup.csv")
  csv_data <- read_cmdstan_csv(csv_files)
  expect_equal(csv_data$time$total, NA_integer_)
  expect_equal(csv_data$time$chains, data.frame(
    chain_id = 2,
    warmup = 0.017041,
    sampling = 0.022068,
    total = 0.039109
  ))

  csv_files <- test_path("resources", "csv", "model1-3-diff_args.csv")
  csv_data <- read_cmdstan_csv(csv_files)
  expect_equal(csv_data$time$total, NA_integer_)
  expect_equal(csv_data$time$chains, data.frame(
    chain_id = 1,
    warmup = 0.038029,
    sampling = 0.030711,
    total = 0.06874
  ))

  csv_files <- c(
    test_path("resources", "csv", "model1-1-warmup.csv"),
    test_path("resources", "csv", "model1-2-warmup.csv")
  )
  csv_data <- read_cmdstan_csv(csv_files)
  expect_equal(csv_data$time$total, NA_integer_)
  expect_equal(csv_data$time$chains, data.frame(
    chain_id = c(1,2),
    warmup = c(0.038029, 0.017041),
    sampling = c(0.030711, 0.022068),
    total = c(0.06874, 0.039109)
  ))
  csv_files <- c(
    test_path("resources", "csv", "bernoulli-1-optimize.csv")
  )
  csv_data <- read_cmdstan_csv(csv_files)
  expect_null(csv_data$time$chains)
})

test_that("time from read_cmdstan_csv matches time from fit$time()", {
  fit <- fit_bernoulli_thin_1
  expect_equivalent(
    read_cmdstan_csv(fit$output_files())$time$chains,
    fit$time()$chains
  )
})

test_that("as_cmdstan_fit creates fitted model objects from csv", {
  fits <- list(
    mle = as_cmdstan_fit(fit_logistic_optimize$output_files()),
    vb = as_cmdstan_fit(fit_logistic_variational$output_files()),
    mcmc = as_cmdstan_fit(fit_logistic_thin_1$output_files())
  )
  for (class in names(fits)) {
    fit <- fits[[class]]
    checkmate::expect_r6(fit, classes = paste0("CmdStan", toupper(class), "_CSV"))
    expect_s3_class(fit$draws(), "draws")
    checkmate::expect_numeric(fit$lp())
    expect_output(fit$print(), "variable")
    expect_length(fit$output_files(), if (class == "mcmc") fit$num_chains() else 1)
    expect_s3_class(fit$summary(), "draws_summary")

    if (class == "mcmc") {
      expect_s3_class(fit$sampler_diagnostics(), "draws_array")
      expect_type(fit$inv_metric(), "list")
      expect_equal(fit$time()$total, NA_integer_)
      expect_s3_class(fit$time()$chains, "data.frame")
    }
    if (class == "mle") {
      checkmate::expect_numeric(fit$mle())
    }
    if (class == "vb") {
      checkmate::expect_numeric(fit$lp_approx())
    }

    for (method in unavailable_methods_CmdStanFit_CSV) {
      if (!(method == "time" && class == "mcmc")) {
        expect_error(fit[[method]](), "This method is not available")
      }
    }
  }
})

test_that("read_cmdstan_csv reads seed correctly", {
  opt <- read_cmdstan_csv(fit_bernoulli_optimize$output_files())
  vi <- read_cmdstan_csv(fit_bernoulli_variational$output_files())
  smp <- read_cmdstan_csv(fit_bernoulli_diag_e_no_samples$output_files())
  expect_equal(opt$metadata$seed, 1234)
  expect_equal(vi$metadata$seed, 123)
  expect_equal(smp$metadata$seed, 123)
})

test_that("read_cmdstan_csv works with sampling and draws_df format", {
  bern_samp_array <- read_cmdstan_csv(fit_bernoulli_thin_1$output_files())

  bern_samp_df <- read_cmdstan_csv(fit_bernoulli_thin_1$output_files(), format = "df")
  expect_equal(posterior::niterations(bern_samp_array$post_warmup_draws),
               posterior::niterations(bern_samp_df$post_warmup_draws))
  expect_equal(posterior::nchains(bern_samp_array$post_warmup_draws),
               posterior::nchains(bern_samp_df$post_warmup_draws))
  expect_equal(posterior::nvariables(bern_samp_array$post_warmup_draws),
               posterior::nvariables(bern_samp_df$post_warmup_draws))
  expect_equal(posterior::variables(bern_samp_array$post_warmup_draws),
               posterior::variables(bern_samp_df$post_warmup_draws))

  theta_array_chain_1 <- posterior::subset_draws(bern_samp_array$post_warmup_draws, variable = "theta", chain = 1)
  theta_df_chain_1 <- posterior::subset_draws(bern_samp_df$post_warmup_draws, variable = "theta", chain = 1)
  theta_array_chain_2 <- posterior::subset_draws(bern_samp_array$post_warmup_draws, variable = "theta", chain = 2)
  theta_df_chain_2 <- posterior::subset_draws(bern_samp_df$post_warmup_draws, variable = "theta", chain = 2)

  expect_true(all(theta_array_chain_1 == theta_df_chain_1$theta))
  expect_true(all(theta_array_chain_2 == theta_df_chain_2$theta))
})

test_that("read_cmdstan_csv works with sampling and draws_list format", {
  bern_samp_array <- read_cmdstan_csv(fit_bernoulli_thin_1$output_files())
  bern_samp_list <- read_cmdstan_csv(fit_bernoulli_thin_1$output_files(), format = "list")

  expect_equal(posterior::niterations(bern_samp_array$post_warmup_draws),
               posterior::niterations(bern_samp_list$post_warmup_draws))
  expect_equal(posterior::nchains(bern_samp_array$post_warmup_draws),
               posterior::nchains(bern_samp_list$post_warmup_draws))
  expect_equal(posterior::nvariables(bern_samp_array$post_warmup_draws),
               posterior::nvariables(bern_samp_list$post_warmup_draws))
  expect_equal(posterior::variables(bern_samp_array$post_warmup_draws),
               posterior::variables(bern_samp_list$post_warmup_draws))

  theta_array_chain_1 <- posterior::subset_draws(bern_samp_array$post_warmup_draws, variable = "theta", chain = 1)
  theta_list_chain_1 <- posterior::subset_draws(bern_samp_list$post_warmup_draws, variable = "theta", chain = 1)
  theta_array_chain_2 <- posterior::subset_draws(bern_samp_array$post_warmup_draws, variable = "theta", chain = 2)
  theta_list_chain_2 <- posterior::subset_draws(bern_samp_list$post_warmup_draws, variable = "theta", chain = 2)

  expect_true(all(theta_array_chain_1 == theta_list_chain_1[[1]]$theta))
  expect_true(all(theta_array_chain_2 == theta_list_chain_2[[1]]$theta))
})

test_that("read_cmdstan_csv works with optimization and draws_array format", {
  bern_opt <- read_cmdstan_csv(fit_bernoulli_optimize$output_files())
  bern_opt_array <- read_cmdstan_csv(fit_bernoulli_optimize$output_files(), format = "array")

  expect_equal(posterior::niterations(bern_opt$point_estimates),
               posterior::niterations(bern_opt_array$point_estimates))
  expect_equal(posterior::nvariables(bern_opt$point_estimates),
               posterior::nvariables(bern_opt_array$point_estimates))
  expect_equal(posterior::variables(bern_opt$point_estimates),
               posterior::variables(bern_opt_array$point_estimates))

  expect_equal(as.numeric(posterior::subset_draws(bern_opt$point_estimates, variable = "theta")),
               as.numeric(posterior::subset_draws(bern_opt_array$point_estimates, variable = "theta")))
})

test_that("read_cmdstan_csv works with optimization and draws_df format", {
  bern_opt <- read_cmdstan_csv(fit_bernoulli_optimize$output_files())

  bern_opt_df <- read_cmdstan_csv(fit_bernoulli_optimize$output_files(), format = "df")

  expect_equal(posterior::niterations(bern_opt$point_estimates),
               posterior::niterations(bern_opt_df$point_estimates))
  expect_equal(posterior::nchains(bern_opt$point_estimates),
               posterior::nchains(bern_opt_df$point_estimates))
  expect_equal(posterior::nvariables(bern_opt$point_estimates),
               posterior::nvariables(bern_opt_df$point_estimates))
  expect_equal(posterior::variables(bern_opt$point_estimates),
               posterior::variables(bern_opt_df$point_estimates))

  expect_equal(as.numeric(posterior::subset_draws(bern_opt$point_estimates, variable = "theta")),
               as.numeric(posterior::subset_draws(bern_opt_df$point_estimates, variable = "theta")$theta))
})

test_that("read_cmdstan_csv works with optimization and draws_list format", {
  bern_opt <- read_cmdstan_csv(fit_bernoulli_optimize$output_files())
  bern_opt_list <- read_cmdstan_csv(fit_bernoulli_optimize$output_files(), format = "list")

  expect_equal(posterior::niterations(bern_opt$point_estimates),
               posterior::niterations(bern_opt_list$point_estimates))
  expect_equal(posterior::nchains(bern_opt$point_estimates),
               posterior::nchains(bern_opt_list$point_estimates))
  expect_equal(posterior::nvariables(bern_opt$point_estimates),
               posterior::nvariables(bern_opt_list$point_estimates))
  expect_equal(posterior::variables(bern_opt$point_estimates),
               posterior::variables(bern_opt_list$point_estimates))

  expect_equal(as.numeric(posterior::subset_draws(bern_opt$point_estimates, variable = "theta")),
               as.numeric(posterior::subset_draws(bern_opt_list$point_estimates, variable = "theta")[[1]]$theta))

})

test_that("read_cmdstan_csv works with VI and draws_array format", {
    bern_vi <- read_cmdstan_csv(fit_bernoulli_variational$output_files())
    bern_vi_array <- read_cmdstan_csv(fit_bernoulli_variational$output_files(), format = "array")

    expect_equal(posterior::niterations(bern_vi$draws),
                 posterior::niterations(bern_vi_array$draws))
    expect_equal(posterior::nvariables(bern_vi$draws),
                 posterior::nvariables(bern_vi_array$draws))
    expect_equal(posterior::variables(bern_vi$draws),
                 posterior::variables(bern_vi_array$draws))

    expect_equal(as.numeric(posterior::subset_draws(bern_vi$draws, variable = "theta")),
                 as.numeric(posterior::subset_draws(bern_vi_array$draws, variable = "theta")))
  })

test_that("read_cmdstan_csv works with VI and draws_df format", {
  bern_vi <- read_cmdstan_csv(fit_bernoulli_variational$output_files())
  bern_vi_df <- read_cmdstan_csv(fit_bernoulli_variational$output_files(), format = "df")

  expect_equal(posterior::niterations(bern_vi$draws),
               posterior::niterations(bern_vi_df$draws))
  expect_equal(posterior::nchains(bern_vi$draws),
               posterior::nchains(bern_vi_df$draws))
  expect_equal(posterior::nvariables(bern_vi$draws),
               posterior::nvariables(bern_vi_df$draws))
  expect_equal(posterior::variables(bern_vi$draws),
               posterior::variables(bern_vi_df$draws))

  expect_equal(as.numeric(posterior::subset_draws(bern_vi$draws, variable = "theta")),
               as.numeric(posterior::subset_draws(bern_vi_df$draws, variable = "theta")$theta))
})

test_that("read_cmdstan_csv works with VI and draws_list format", {
  bern_vi <- read_cmdstan_csv(fit_bernoulli_variational$output_files())
  bern_vi_list <- read_cmdstan_csv(fit_bernoulli_variational$output_files(), format = "list")

  expect_equal(posterior::niterations(bern_vi$draws),
               posterior::niterations(bern_vi_list$draws))
  expect_equal(posterior::nchains(bern_vi$draws),
               posterior::nchains(bern_vi_list$draws))
  expect_equal(posterior::nvariables(bern_vi$draws),
               posterior::nvariables(bern_vi_list$draws))
  expect_equal(posterior::variables(bern_vi$draws),
               posterior::variables(bern_vi_list$draws))

  expect_equal(as.numeric(posterior::subset_draws(bern_vi$draws, variable = "theta")),
               as.numeric(posterior::subset_draws(bern_vi_list$draws, variable = "theta")[[1]]$theta))

})

test_that("read_cmdstan_csv works with GQ and draws_df format", {
  bern_gq <- read_cmdstan_csv(fit_gq$output_files())
  bern_gq_df <- read_cmdstan_csv(fit_gq$output_files(), format = "df")

  expect_equal(posterior::niterations(bern_gq$generated_quantities),
               posterior::niterations(bern_gq_df$generated_quantities))
  expect_equal(posterior::nchains(bern_gq$generated_quantities),
               posterior::nchains(bern_gq_df$generated_quantities))
  expect_equal(posterior::nvariables(bern_gq$generated_quantities),
               posterior::nvariables(bern_gq_df$generated_quantities))
  expect_equal(posterior::variables(bern_gq$generated_quantities),
               posterior::variables(bern_gq_df$generated_quantities))

  sum_y_array_chain_1 <- posterior::subset_draws(bern_gq$generated_quantities, variable = "sum_y", chain = 1)
  sum_y_df_chain_1 <- posterior::subset_draws(bern_gq_df$generated_quantities, variable = "sum_y", chain = 1)
  sum_y_array_chain_2 <- posterior::subset_draws(bern_gq$generated_quantities, variable = "sum_y", chain = 2)
  sum_y_df_chain_2 <- posterior::subset_draws(bern_gq_df$generated_quantities, variable = "sum_y", chain = 2)

  expect_true(all(sum_y_array_chain_1 == sum_y_df_chain_1$sum_y))
  expect_true(all(sum_y_array_chain_2 == sum_y_df_chain_2$sum_y))
})

test_that("read_cmdstan_csv works with GQ and draws_list format", {
  bern_gq <- read_cmdstan_csv(fit_gq$output_files())
  bern_gq_list <- read_cmdstan_csv(fit_gq$output_files(), format = "list")

  expect_equal(posterior::niterations(bern_gq$generated_quantities),
               posterior::niterations(bern_gq_list$generated_quantities))
  expect_equal(posterior::nchains(bern_gq$generated_quantities),
               posterior::nchains(bern_gq_list$generated_quantities))
  expect_equal(posterior::nvariables(bern_gq$generated_quantities),
               posterior::nvariables(bern_gq_list$generated_quantities))
  expect_equal(posterior::variables(bern_gq$generated_quantities),
               posterior::variables(bern_gq_list$generated_quantities))

  sum_y_array_chain_1 <- posterior::subset_draws(bern_gq$generated_quantities, variable = "sum_y", chain = 1)
  sum_y_list_chain_1 <- posterior::subset_draws(bern_gq_list$generated_quantities, variable = "sum_y", chain = 1)
  sum_y_array_chain_2 <- posterior::subset_draws(bern_gq$generated_quantities, variable = "sum_y", chain = 2)
  sum_y_list_chain_2 <- posterior::subset_draws(bern_gq_list$generated_quantities, variable = "sum_y", chain = 2)

  expect_true(all(sum_y_array_chain_1 == sum_y_list_chain_1[[1]]$sum_y))
  expect_true(all(sum_y_array_chain_2 == sum_y_list_chain_2[[1]]$sum_y))
})

test_that("read_cmdstan_csv errors if bad draws format", {
  expect_error(
    read_cmdstan_csv(fit_bernoulli_thin_1$output_files(), format = "bad_format"),
    "The supplied draws format is not valid"
  )
})

test_that("read_cmdstan_csv works with diagnose results", {
  csv_file <- test_path("resources", "csv", "logistic-diagnose.csv")

  diagnose_results <- read_cmdstan_csv(csv_file)
  expect_true(is.numeric(diagnose_results$lp))
  expect_true(is.data.frame(diagnose_results$gradients))
  expect_equal(diagnose_results$lp, -88.1497)
  expect_equal(diagnose_results$gradients$param_idx, c(0, 1, 2, 3))
  expect_equal(diagnose_results$gradients$value, c(0.037817, -1.26198, 1.16792, 0.933592))
  expect_equal(diagnose_results$gradients$model, c(8.83081, 4.07931, -25.7167, -4.11423))
  expect_equal(diagnose_results$gradients$finite_diff, c(8.83081, 4.07931, -25.7167, -4.11423))
  expect_equal(diagnose_results$gradients$error, c(9.919e-09, 3.13568e-08, -5.31186e-09, 5.87693e-09))
})

test_that("variable_dims() works", {
  expect_null(variable_dims(NULL))

  vars <- c("a", "b[1]", "b[2]", "b[3]", "c[1,1]", "c[1,2]")
  vars_dims <- list(a = 1, b = 3, c = c(1,2))
  expect_equal(variable_dims(vars), vars_dims)

  vars <- c("a", "b")
  vars_dims <- list(a = 1, b = 1)
  expect_equal(variable_dims(vars), vars_dims)

  vars <- c("c[1,1]", "c[1,2]", "c[1,3]", "c[2,1]", "c[2,2]", "c[2,3]", "b[1]", "b[2]", "b[3]", "b[4]")
  vars_dims <- list(c = c(2,3), b = 4)
  expect_equal(variable_dims(vars), vars_dims)

  # make sure not confused by one name being last substring of another name
  vars <- c("a[1]", "a[2]", "aa[1]", "aa[2]", "aa[3]")
  expect_equal(variable_dims(vars), list(a = 2, aa = 3))

  # wrong dimensions for descending order
  vars <- c("c[1,1]", "c[1,2]", "c[1,3]", "c[2,3]", "c[2,2]", "c[2,1]", "b[4]", "b[2]", "b[3]", "b[1]")
  vars_dims <- list(c = c(2,1), b = 1)
  expect_equal(variable_dims(vars), vars_dims)
})

test_that("read_cmdstan_csv works if no variables are specified", {
  expect_silent(
    read_cmdstan_csv(fit_bernoulli_optimize$output_files(), variables = "", sampler_diagnostics = "")
  )
  expect_silent(
    read_cmdstan_csv(fit_bernoulli_variational$output_files(), variables = "", sampler_diagnostics = "")
  )
  expect_silent(
    read_cmdstan_csv(fit_bernoulli_thin_1$output_files(), variables = "", sampler_diagnostics = "")
  )
})
