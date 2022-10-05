context("model-methods")

set_cmdstan_path()
mod <- cmdstan_model(testing_stan_file("bernoulli"), force_recompile = TRUE)
data_list <- testing_data("bernoulli")
fit <- mod$sample(data = data_list, chains = 1)

test_that("Methods error if not compiled", {
  expect_error(
    fit$log_prob(NULL),
    "The method has not been compiled, please call `init_model_methods()` first",
    fixed = TRUE
  )
  expect_error(
    fit$grad_log_prob(NULL),
    "The method has not been compiled, please call `init_model_methods()` first",
    fixed = TRUE
  )
  expect_error(
    fit$unconstrain_pars(NULL),
    "The method has not been compiled, please call `init_model_methods()` first",
    fixed = TRUE
  )
  expect_error(
    fit$constrain_pars(NULL),
    "The method has not been compiled, please call `init_model_methods()` first",
    fixed = TRUE
  )
})

fit$init_model_methods()

test_that("Methods return correct values", {
  expect_equal(fit$log_prob(upars=c(0.1)), -8.6327599208828509347)

  grad_lp <- -3.2997502497472801508
  attr(grad_lp, "log_prob") <- -8.6327599208828509347
  expect_equal(fit$grad_log_prob(upars=c(0.1)), grad_lp)

  cpars <- fit$constrain_pars(c(0.1))
  expect_equal(cpars, list(theta = 0.52497918747894001257))

  upars <- fit$unconstrain_pars(cpars)
  expect_equal(upars, c(0.1))
})

test_that("methods error for incorrect inputs", {
  expect_error(
    fit$log_prob(c(1,2)),
    "Model has 1 unconstrained parameter(s), but 2 were provided!",
    fixed = TRUE
  )
  expect_error(
    fit$grad_log_prob(c(1,2)),
    "Model has 1 unconstrained parameter(s), but 2 were provided!",
    fixed = TRUE
  )
  expect_error(
    fit$unconstrain_pars(list(theta = 0.5, dummy = 5)),
    "Provided parameter(s): dummy not present in model!",
    fixed = TRUE
  )
  expect_error(
    fit$constrain_pars(c(1,2)),
    "Model has 1 unconstrained parameter(s), but 2 were provided!",
    fixed = TRUE
  )

  logistic_mod <- cmdstan_model(testing_stan_file("logistic"), force_recompile = TRUE)
  logistic_data_list <- testing_data("logistic")
  logistic_fit <- logistic_mod$sample(data = logistic_data_list, chains = 1)
  logistic_fit$init_model_methods()

  expect_error(
    logistic_fit$unconstrain_pars(list(alpha = 0.5)),
    "Model parameter(s): beta not provided!",
    fixed = TRUE
  )
})

test_that("Methods error with already-compiled model", {
  mod <- testing_model("bernoulli")
  data_list <- testing_data("bernoulli")
  fit <- mod$sample(data = data_list, chains = 1)
  expect_error(
    fit$init_model_methods(),
    "Model methods cannot be used with a pre-compiled Stan executable, the model must be compiled again",
    fixed = TRUE
  )
})
