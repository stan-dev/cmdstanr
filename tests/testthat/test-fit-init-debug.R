context("fitted-inits")
set_cmdstan_path()




test_inits <- function() {
  fit_init <- function() {
    list(alpha = rnorm(1), beta = rnorm(3))
  }
  mod <- testing_model("logistic")
  data_list <- testing_data("logistic")
  fit_sample_multi <- mod$sample(data = data_list, chains = 2, init = fit_init,
                                 iter_sampling = 200, iter_warmup = 200, refresh = 1, seed = 1234)
  return(0)
}
while (1) {
  test_that("Fails when calling sample many times", {
    set.seed(1234)
    test_inits()
    expect_true(TRUE)
  })
}
