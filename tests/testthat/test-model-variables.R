context("model-compile")

if (not_on_cran()) {
  set_cmdstan_path()
}

test_that("$variables() work correctly with example models", {
  skip_on_cran()
  mod <- testing_model("bernoulli")
  expect_equal(names(mod$variables()$data), c("N", "y"))
  expect_equal(names(mod$variables()$parameters), c("theta"))
  expect_equal(mod$variables()$data$N$type, "int")
  expect_equal(mod$variables()$data$N$dimensions, 0)
  expect_equal(mod$variables()$data$y$type, "int")
  expect_equal(mod$variables()$data$y$dimensions, 1)
  expect_equal(mod$variables()$parameters$theta$type, "real")
  expect_equal(mod$variables()$parameters$theta$dimensions, 0)
  expect_equal(length(mod$variables()$transformed_parameters), 0)
  expect_equal(length(mod$variables()$generated_quantities), 0)
  expect_true(is.list(mod$variables()$transformed_parameters))
  expect_true(is.list(mod$variables()$generated_quantities))

  mod <- testing_model("bernoulli_log_lik")
  expect_equal(names(mod$variables()$data), c("N", "y"))
  expect_equal(names(mod$variables()$parameters), c("theta"))
  expect_equal(names(mod$variables()$generated_quantities), c("log_lik"))
  expect_equal(mod$variables()$generated_quantities$log_lik$type, "real")
  expect_equal(mod$variables()$generated_quantities$log_lik$dimensions, 1)

  mod <- testing_model("logistic")
  expect_equal(names(mod$variables()$data), c("N", "K", "y", "X"))
  expect_equal(names(mod$variables()$parameters), c("alpha", "beta"))
  expect_equal(mod$variables()$data$N$type, "int")
  expect_equal(mod$variables()$data$N$dimensions, 0)
  expect_equal(mod$variables()$data$K$type, "int")
  expect_equal(mod$variables()$data$K$dimensions, 0)
  expect_equal(mod$variables()$data$y$type, "int")
  expect_equal(mod$variables()$data$y$dimensions, 1)
  expect_equal(mod$variables()$data$X$type, "real")
  expect_equal(mod$variables()$data$X$dimensions, 2)
  expect_equal(mod$variables()$parameters$alpha$type, "real")
  expect_equal(mod$variables()$parameters$alpha$dimensions, 0)
  expect_equal(mod$variables()$parameters$beta$type, "real")
  expect_equal(mod$variables()$parameters$beta$dimensions, 1)
})

test_that("$variables() work correctly with example models", {
  skip_on_cran()
  code <- "
  data {
    array[1,2,3,4,5,6,7,8] int y;
    array[1,2,3,4] vector[4] x;
  }
  parameters {
    real z;
  }
  transformed parameters {
    array[1,2,3] real p;
    array[2] matrix[2,3] pp;
  }
  "
  stan_file <- write_stan_file(code)
  mod <- cmdstan_model(stan_file)
  expect_equal(names(mod$variables()$data), c("y", "x"))
  expect_equal(names(mod$variables()$parameters), c("z"))
  expect_equal(names(mod$variables()$transformed_parameters), c("p", "pp"))
  expect_equal(mod$variables()$data$y$type, "int")
  expect_equal(mod$variables()$data$y$dimensions, 8)
  expect_equal(mod$variables()$data$x$type, "real")
  expect_equal(mod$variables()$data$x$dimensions, 5)
  expect_equal(mod$variables()$parameters$z$type, "real")
  expect_equal(mod$variables()$parameters$z$dimensions, 0)
  expect_equal(mod$variables()$transformed_parameters$p$type, "real")
  expect_equal(mod$variables()$transformed_parameters$p$dimensions, 3)
  expect_equal(mod$variables()$transformed_parameters$pp$type, "real")
  expect_equal(mod$variables()$transformed_parameters$pp$dimensions, 3)
})
