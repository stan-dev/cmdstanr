context("model-methods")

set_cmdstan_path()
mod <- cmdstan_model(testing_stan_file("bernoulli_log_lik"), force_recompile = TRUE)
data_list <- testing_data("bernoulli")
fit <- mod$sample(data = data_list, chains = 1)

test_that("Methods error if not compiled", {
  skip_if(os_is_wsl())
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
    fit$hessian(NULL),
    "The method has not been compiled, please call `init_model_methods()` first",
    fixed = TRUE
  )
  expect_error(
    fit$unconstrain_variables(NULL),
    "The method has not been compiled, please call `init_model_methods()` first",
    fixed = TRUE
  )
  expect_error(
    fit$constrain_variables(NULL),
    "The method has not been compiled, please call `init_model_methods()` first",
    fixed = TRUE
  )
})

test_that("User warned about higher-order autodiff with hessian", {
  skip_if(os_is_wsl())
  expect_message(
    fit$init_model_methods(hessian = TRUE, verbose = TRUE),
    "The hessian method relies on higher-order autodiff which is still experimental. Please report any compilation errors that you encounter",
    fixed = TRUE
    )
})

test_that("Methods return correct values", {
  skip_if(os_is_wsl())
  lp <- fit$log_prob(unconstrained_variables=c(0.1))
  expect_equal(lp, -8.6327599208828509347)

  grad_lp <- -3.2997502497472801508
  attr(grad_lp, "log_prob") <- lp
  expect_equal(fit$grad_log_prob(unconstrained_variables=c(0.1)), grad_lp)

  hessian <- list(
    log_prob = lp,
    grad_log_prob = -3.2997502497472801508,
    hessian = as.matrix(-2.9925124823147033482, nrow=1, ncol=1)
  )
  expect_equal(fit$hessian(unconstrained_variables=c(0.1)), hessian)

  hessian_noadj <- list(
    log_prob = -7.2439666007357095268,
    grad_log_prob = -3.2497918747894001257,
    hessian = as.matrix(-2.4937604019289194568, nrow=1, ncol=1)
  )

  expect_equal(fit$hessian(unconstrained_variables=c(0.1), jacobian_adjustment = FALSE),
               hessian_noadj)

  cpars <- fit$constrain_variables(c(0.1))
  cpars_true <- list(
    theta = 0.52497918747894001257,
    log_lik = rep(-7.2439666007357095268, data_list$N)
  )
  expect_equal(cpars, cpars_true)

  expect_equal(fit$constrain_variables(c(0.1), generated_quantities = FALSE),
               list(theta = 0.52497918747894001257))

  skeleton <- list(
    theta = array(0, dim = 1),
    log_lik = array(0, dim = data_list$N)
  )

  expect_equal(fit$variable_skeleton(), skeleton)

  unconstrained_variables <- fit$unconstrain_variables(cpars)
  expect_equal(unconstrained_variables, c(0.1))
})

test_that("Model methods environments are independent", {
  skip_if(os_is_wsl())
  data_list_2 <- data_list
  data_list_2$N <- 20
  data_list_2$y <- c(data_list$y, data_list$y)
  fit_2 <- mod$sample(data = data_list_2, chains = 1)
  fit_2$init_model_methods()

  expect_equal(fit$log_prob(unconstrained_variables=c(0.1)), -8.6327599208828509347)
  expect_equal(fit_2$log_prob(unconstrained_variables=c(0.1)), -15.87672652161856135)
})

test_that("methods error for incorrect inputs", {
  skip_if(os_is_wsl())
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
    fit$hessian(c(1,2)),
    "Model has 1 unconstrained parameter(s), but 2 were provided!",
    fixed = TRUE
  )
  expect_error(
    fit$constrain_variables(c(1,2)),
    "Model has 1 unconstrained parameter(s), but 2 were provided!",
    fixed = TRUE
  )

  logistic_mod <- cmdstan_model(testing_stan_file("logistic"), force_recompile = TRUE)
  logistic_data_list <- testing_data("logistic")
  logistic_fit <- logistic_mod$sample(data = logistic_data_list, chains = 1)
  # Init without Hessian, as bernoulli_logit_glm currently not fully fvar<var>
  # compatible
  logistic_fit$init_model_methods(verbose = TRUE)

  expect_error(
    logistic_fit$unconstrain_variables(list(alpha = 0.5)),
    "Model parameter(s): beta not provided!",
    fixed = TRUE
  )
})

test_that("Methods error with already-compiled model", {
  skip_if(os_is_wsl())
  precompile_mod <- testing_model("bernoulli")
  mod <- testing_model("bernoulli")
  data_list <- testing_data("bernoulli")
  fit <- mod$sample(data = data_list, chains = 1)
  expect_error(
    fit$init_model_methods(),
    "Model methods cannot be used with a pre-compiled Stan executable, the model must be compiled again",
    fixed = TRUE
  )
})

test_that("Methods can be compiled with model", {
  skip_if(os_is_wsl())
  mod <- cmdstan_model(testing_stan_file("bernoulli"),
                       force_recompile = TRUE,
                       compile_model_methods = TRUE,
                       compile_hessian_method = TRUE)
  fit <- mod$sample(data = data_list, chains = 1)

  lp <- fit$log_prob(unconstrained_variables=c(0.6))
  expect_equal(lp, -10.649855405830624733)

  grad_lp <- -4.7478756747095447466
  attr(grad_lp, "log_prob") <- lp
  expect_equal(fit$grad_log_prob(unconstrained_variables=c(0.6)), grad_lp)

  hessian <- list(
    log_prob = lp,
    grad_log_prob = -4.7478756747095447466,
    hessian = as.matrix(-2.7454108854798882078, nrow=1, ncol=1)
  )
  expect_equal(fit$hessian(unconstrained_variables=c(0.6)), hessian)

  cpars <- fit$constrain_variables(c(0.6))
  expect_equal(cpars, list(theta = 0.64565630622579539555))

  unconstrained_variables <- fit$unconstrain_variables(cpars)
  expect_equal(unconstrained_variables, c(0.6))
})

test_that("unconstrain_variables correctly handles zero-length containers", {
  skip_if(os_is_wsl())
  model_code <- "
  data {
    int N;
  }
  parameters {
    vector[N] y;
    real x;
  }
  model {
    x ~ std_normal();
    y ~ std_normal();
  }
  "
  mod <- cmdstan_model(write_stan_file(model_code),
                       compile_model_methods = TRUE)
  fit <- mod$sample(data = list(N = 0), chains = 1)
  unconstrained <- fit$unconstrain_variables(variables = list(x = 5))
  expect_equal(unconstrained, 5)
})

test_that("unconstrain_draws returns correct values", {
  skip_if(os_is_wsl())

  # With no constraints, the parameter draws should be the same as the
  # unconstrained draws
  model_code <- "
    data {
      int N;
    }
    parameters {
      real x;
    }
    model {
      x ~ std_normal();
    }
  "
  mod <- cmdstan_model(write_stan_file(model_code),
                       compile_model_methods = TRUE,
                       force_recompile = TRUE)
  fit <- mod$sample(data = list(N = 0), chains = 1)

  x_draws <- fit$draws(format = "draws_df")$x

  # Unconstrain all internal draws
  unconstrained_internal_draws <- fit$unconstrain_draws()[[1]]
  expect_equal(as.numeric(x_draws), as.numeric(unconstrained_internal_draws))

  # Unconstrain external CmdStan CSV files
  unconstrained_csv <- fit$unconstrain_draws(files = fit$output_files())[[1]]
  expect_equal(as.numeric(x_draws), as.numeric(unconstrained_csv))

  # Unconstrain existing draws object
  unconstrained_draws <- fit$unconstrain_draws(draws = fit$draws())[[1]]
  expect_equal(as.numeric(x_draws), as.numeric(unconstrained_draws))

  # With a lower-bounded constraint, the parameter draws should be the
  # exponentiation of the unconstrained draws
  model_code <- "
    data {
      int N;
    }
    parameters {
      real<lower = 0> x;
    }
    model {
      x ~ std_normal();
    }
  "
  mod <- cmdstan_model(write_stan_file(model_code),
                       compile_model_methods = TRUE,
                       force_recompile = TRUE)
  fit <- mod$sample(data = list(N = 0), chains = 1)

  x_draws <- fit$draws(format = "draws_df")$x

  unconstrained_internal_draws <- fit$unconstrain_draws()[[1]]
  expect_equal(as.numeric(x_draws), exp(as.numeric(unconstrained_internal_draws)))

  # Unconstrain external CmdStan CSV files
  unconstrained_csv <- fit$unconstrain_draws(files = fit$output_files())[[1]]
  expect_equal(as.numeric(x_draws), exp(as.numeric(unconstrained_csv)))

  # Unconstrain existing draws object
  unconstrained_draws <- fit$unconstrain_draws(draws = fit$draws())[[1]]
  expect_equal(as.numeric(x_draws), exp(as.numeric(unconstrained_draws)))
})

test_that("Model methods can be initialised for models with no data", {
  skip_if(os_is_wsl())

  stan_file <- write_stan_file("parameters { real x; } model { x ~ std_normal(); }")
  mod <- cmdstan_model(stan_file, compile_model_methods = TRUE, force_recompile = TRUE)
  expect_no_error(fit <- mod$sample())
  expect_equal(fit$log_prob(5), -12.5)
})
