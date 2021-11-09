context("model-sample-metric")

set_cmdstan_path()
mod <- testing_model("bernoulli")
data_list <- testing_data("bernoulli")


test_that("sample() method works with provided inv_metrics", {
  inv_metric_vector <- array(1, dim = c(1))
  inv_metric_matrix <- matrix(1, nrow = 1, ncol = 1)

  inv_metric_vector_json <- test_path("resources", "metric", "bernoulli.inv_metric.diag_e.json")
  inv_metric_matrix_json <- test_path("resources", "metric", "bernoulli.inv_metric.dense_e.json")

  inv_metric_vector_r <- test_path("resources", "metric", "bernoulli.inv_metric.diag_e.data.R")
  inv_metric_matrix_r <- test_path("resources", "metric", "bernoulli.inv_metric.dense_e.data.R")

  expect_sample_output(fit_r <- mod$sample(data = data_list,
                                           chains = 1,
                                           metric = "diag_e",
                                           inv_metric = inv_metric_vector,
                                           seed = 123))

  expect_sample_output(fit_r <- mod$sample(data = data_list,
                                           chains = 1,
                                           metric = "dense_e",
                                           inv_metric = inv_metric_matrix,
                                           seed = 123))

  expect_sample_output(fit_json <- mod$sample(data = data_list,
                                              chains = 1,
                                              metric = "diag_e",
                                              metric_file = inv_metric_vector_json,
                                              seed = 123))

  expect_sample_output(fit_json <- mod$sample(data = data_list,
                                              chains = 1,
                                              metric = "dense_e",
                                              metric_file = inv_metric_matrix_json,
                                              seed = 123))

  expect_sample_output(fit_r <- mod$sample(data = data_list,
                                           chains = 1,
                                           metric = "diag_e",
                                           metric_file = inv_metric_vector_r,
                                           seed = 123))

  expect_sample_output(fit_r <- mod$sample(data = data_list,
                                           chains = 3,
                                           parallel_chains = 2,
                                           metric = "dense_e",
                                           metric_file = inv_metric_matrix_r,
                                           seed = 123))
})


test_that("sample() method works with lists of inv_metrics", {
  inv_metric_vector <- array(1, dim = c(1))
  inv_metric_vector_json <- test_path("resources", "metric", "bernoulli.inv_metric.diag_e.json")

  expect_sample_output(fit_r <- mod$sample(data = data_list,
                                           chains = 1,
                                           metric = "diag_e",
                                           inv_metric = list(inv_metric_vector),
                                           seed = 123))

  expect_sample_output(fit_r <- mod$sample(data = data_list,
                                           chains = 2,
                                           metric = "diag_e",
                                           inv_metric = list(inv_metric_vector, inv_metric_vector),
                                           seed = 123))

  expect_error(fit_r <- mod$sample(data = data_list,
                                   chains = 3,
                                   parallel_chains = 2,
                                   metric = "diag_e",
                                   inv_metric = list(inv_metric_vector, inv_metric_vector)),
               "2 metric\\(s\\) provided. Must provide 1 or 3 metric\\(s\\) for 3 chain\\(s\\)")

  expect_sample_output(fit_r <- mod$sample(data = data_list,
                                           chains = 1,
                                           metric = "diag_e",
                                           metric_file = list(inv_metric_vector_json)))

  expect_sample_output(fit_r <- mod$sample(data = data_list,
                                           chains = 2,
                                           metric = "diag_e",
                                           metric_file = list(inv_metric_vector_json, inv_metric_vector_json)))

  expect_sample_output(fit_r <- mod$sample(data = data_list,
                                           chains = 2,
                                           metric = "diag_e",
                                           metric_file = c(inv_metric_vector_json, inv_metric_vector_json)))

  expect_error(fit_r <- mod$sample(data = data_list,
                                   chains = 3,
                                   parallel_chains = 2,
                                   metric = "diag_e",
                                   metric_file = c(inv_metric_vector_json, inv_metric_vector_json)),
               "2 metric\\(s\\) provided. Must provide 1 or 3 metric\\(s\\) for 3 chain\\(s\\)")
})

test_that("sample() method fails if metric_file and inv_metric both provided", {
  inv_metric_vector <- array(1, dim = c(1))
  inv_metric_vector_json <- test_path("resources", "metric", "bernoulli.inv_metric.diag_e.json")

  expect_error(mod$sample(data = data_file_r,
                          chains = 1,
                          metric = "diag_e",
                          inv_metric = inv_metric_vector,
                          metric_file = inv_metric_vector_json),
               "Only one of inv_metric and metric_file can be specified")
})
