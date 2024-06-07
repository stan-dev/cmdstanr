context("fitted-inits")
set_cmdstan_path()


test_that("Sample method works as init", {
  mod <- testing_model("issue_975")
  data <- list(N = 100, y = rnorm(100))
  pf <- mod$pathfinder(data = data)
  expect_no_error(fit <- mod$sample(data = data, init = pf))
})
