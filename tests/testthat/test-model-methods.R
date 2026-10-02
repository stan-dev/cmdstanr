skip_on_cran()

skip_if(os_is_wsl())

set_cmdstan_path()
mod <- cmdstan_model(testing_stan_file("bernoulli_log_lik"), force_recompile = TRUE)
data_list <- testing_data("bernoulli")
utils::capture.output(
  fit <- mod$sample(data = data_list, chains = 1, refresh = 0)
)

# One program with every parameter shape the tests below need, built once.
# N = 0 and K = 0 give zero-length containers.
shapes_mod <- cmdstan_model(write_stan_file("
  data {
    int N;
    int K;
  }
  parameters {
    real x;
    real<lower = 0> y;
    vector[N] v;
    matrix[N, K] m;
    row_vector[K] rv;
  }
  model {
    x ~ std_normal();
    y ~ std_normal();
    v ~ std_normal();
    to_vector(m) ~ std_normal();
    rv ~ std_normal();
  }
"), force_recompile = TRUE)

test_that("Model methods automatically initialise when needed", {
  expect_no_error(fit$log_prob(unconstrained_variables=c(0.1)))
})

test_that("Methods return correct values", {
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

  expect_equal(fit$hessian(unconstrained_variables=c(0.1), jacobian = FALSE),
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
  data_list_2 <- data_list
  data_list_2$N <- 20
  data_list_2$y <- c(data_list$y, data_list$y)
  utils::capture.output(
    fit_2 <- mod$sample(data = data_list_2, chains = 1)
  )
  fit_2$init_model_methods()

  expect_equal(fit$log_prob(unconstrained_variables=c(0.1)), -8.6327599208828509347)
  expect_equal(fit_2$log_prob(unconstrained_variables=c(0.1)), -15.87672652161856135)
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
    fit$hessian(c(1,2)),
    "Model has 1 unconstrained parameter(s), but 2 were provided!",
    fixed = TRUE
  )
  expect_error(
    fit$constrain_variables(c(1,2)),
    "Model has 1 unconstrained parameter(s), but 2 were provided!",
    fixed = TRUE
  )

  utils::capture.output(
    shapes_fit <- shapes_mod$sample(data = list(N = 0, K = 0), chains = 1)
  )
  shapes_fit$init_model_methods()

  expect_error(
    shapes_fit$unconstrain_variables(list(x = 0.5)),
    "Model parameter(s): y not provided!",
    fixed = TRUE
  )
})

test_that("Methods work with a model built from a reused executable", {
  # the top of the file built this executable
  reused <- expect_no_recompilation(testing_model("bernoulli_log_lik"))
  utils::capture.output(
    fit <- reused$sample(data = data_list, chains = 1)
  )
  expect_no_error(fit$init_model_methods())
})

test_that("Reloaded models recompile model methods lazily after saveRDS/readRDS", {
  # Also tests that fitted model objects are returned without error after
  # saveRDS/readRDS when model methods are compiled: https://github.com/stan-dev/cmdstanr/issues/1157
  mod <- cmdstan_model(
    testing_stan_file("bernoulli_log_lik"),
    force_recompile = TRUE
  )
  temp_rds_file <- tempfile(fileext = ".RDS")
  saveRDS(mod, temp_rds_file)
  mod2 <- readRDS(temp_rds_file)

  expect_no_error(
    utils::capture.output(
      fit <- mod2$optimize(data = data_list)
    )
  )
  expect_equal(fit$log_prob(unconstrained_variables = c(0.1)),
               -8.6327599208828509347)
})

test_that("stale model-method bindings are detected and dropped", {
  mod <- cmdstan_model(
    testing_stan_file("bernoulli_log_lik"),
    force_recompile = TRUE
  )
  utils::capture.output(fit <- mod$optimize(data = data_list))
  fit$init_model_methods()
  temp_rds_file <- tempfile(fileext = ".RDS")
  saveRDS(fit, temp_rds_file)
  fit2 <- readRDS(temp_rds_file)
  model_methods_env <- fit2$.__enclos_env__$private$model_methods_env_

  expect_true(drop_stale_model_methods(model_methods_env))
  expect_equal(ls(model_methods_env, all.names = TRUE), "hpp_code_")
  expect_false(drop_stale_model_methods(model_methods_env))
})

test_that("model_methods_are_live() rejects pointers without an address", {
  # Serializing an external pointer discards its address, which is the state a
  # reloaded fit's model_ptr_ is in. No compilation needed to reproduce it.
  model_methods_env <- new.env()
  expect_false(model_methods_are_live(model_methods_env))

  model_methods_env$model_ptr_ <- new_null_external_pointer()
  expect_false(model_methods_are_live(model_methods_env))

  # A decorated pointer is not a shape this can decide, so it must not be
  # reported as live either.
  decorated <- new_null_external_pointer()
  class(decorated) <- "SomethingElse"
  model_methods_env$model_ptr_ <- decorated
  expect_false(model_methods_are_live(model_methods_env))
})

test_that("source_cpp_native_symbol_is_null() only flags null .Call symbols", {
  null_symbol <- new_null_external_pointer()
  class(null_symbol) <- "NativeSymbol"
  null_symbol_fun <- function(x) NULL
  body(null_symbol_fun) <- as.call(list(as.name(".Call"), null_symbol, quote(x)))
  expect_true(source_cpp_native_symbol_is_null(null_symbol_fun))

  expect_false(source_cpp_native_symbol_is_null(function(x) x + 1))
  expect_false(source_cpp_native_symbol_is_null(quote(.Call)))
  expect_false(source_cpp_native_symbol_is_null(NULL))
})

test_that("cmdstanr does not modify the shared externalptr prototype", {
  # methods::new("externalptr") returns the S4 prototype itself, and external
  # pointers are not duplicated on assignment, so attaching attributes to the
  # result would corrupt new("externalptr") for every caller in the session.
  expect_null(attributes(methods::new("externalptr")))
  expect_null(attributes(.cmdstanr$NULL_EXTERNAL_POINTER))
})

test_that("unconstrain_variables correctly handles zero-length containers", {
  utils::capture.output(
    fit <- shapes_mod$sample(data = list(N = 0, K = 0), chains = 1)
  )
  unconstrained <- fit$unconstrain_variables(variables = list(x = 5, y = 1))
  expect_equal(unconstrained, c(5, 0))
})

test_that("unconstrain_draws returns correct values", {
  utils::capture.output({
    fit <- shapes_mod$sample(data = list(N = 0, K = 0), chains = 2,
                             save_warmup = TRUE)
    fit_no_warmup <- shapes_mod$sample(data = list(N = 0, K = 0), chains = 2)
  })

  # x has no constraint, so its draws are its unconstrained draws, and y has
  # a lower bound of zero, so its draws are the exponential of its
  expect_unconstrained <- function(unconstrained, inc_warmup = FALSE) {
    draws <- fit$draws(format = "draws_df", inc_warmup = inc_warmup)
    unconstrained <- posterior::as_draws_df(unconstrained)
    expect_equal(unconstrained$x, draws$x)
    expect_equal(exp(unconstrained$y), draws$y)
  }

  # Unconstrain all internal draws
  expect_unconstrained(fit$unconstrain_draws())
  expect_unconstrained(fit$unconstrain_draws(inc_warmup = TRUE),
                       inc_warmup = TRUE)

  expect_error(
    fit_no_warmup$unconstrain_draws(inc_warmup = TRUE),
    "Warmup draws were requested from a fit object without them!"
  )

  # Unconstrain external CmdStan CSV files
  expect_unconstrained(fit$unconstrain_draws(files = fit$output_files()))
  expect_unconstrained(
    fit$unconstrain_draws(files = fit$output_files(), inc_warmup = TRUE),
    inc_warmup = TRUE
  )

  # Unconstrain existing draws object
  expect_unconstrained(fit$unconstrain_draws(draws = fit$draws()))

  expect_message(fit$unconstrain_draws(draws = fit$draws(), inc_warmup = TRUE),
                 "`inc_warmup` cannot be used with a draws object. Ignoring.")

  expect_error(
    fit$unconstrain_draws(files = fit$output_files(), draws = fit$draws()),
    "not both"
  )
  expect_true(posterior::is_draws_df(fit$unconstrain_draws(format = "df")))
  expect_true(
    posterior::is_draws_df(fit$unconstrain_draws(format = "data.frame"))
  )
  expect_true(posterior::is_draws_list(fit$unconstrain_draws(format = "list")))
  expect_error(
    fit$unconstrain_draws(format = "rvars"),
    "convert after extracting the draws"
  )
})

test_that("Model methods can be initialised for models with no data", {
  stan_file <- write_stan_file("parameters { real x; } model { x ~ std_normal(); }")
  mod <- cmdstan_model(stan_file, force_recompile = TRUE)
  expect_no_error(
    utils::capture.output(
      fit <- mod$sample()
    )
  )
  expect_equal(fit$log_prob(5), -12.5)
})

test_that("Variable skeleton returns correct dimensions for matrices", {
  N <- 4
  K <- 3
  utils::capture.output(
    fit <- shapes_mod$sample(data = list(N = N, K = K), chains = 1,
                             iter_warmup = 1, iter_sampling = 5)
  )

  target_skeleton <- list(
    x = array(0, dim = 1),
    y = array(0, dim = 1),
    v = array(0, dim = N),
    m = array(0, dim = c(N, K)),
    rv = array(0, dim = K)
  )

  expect_equal(fit$variable_skeleton(), target_skeleton)
})

test_that("model methods refuse a fit from an executable alone", {
  adopted <- cmdstan_model(exe_file = mod$exe_file())
  utils::capture.output(
    fit_adopted <- adopted$sample(
      data = data_list, chains = 1, iter_warmup = 10, iter_sampling = 10,
      refresh = 0
    )
  )
  expect_error(
    fit_adopted$log_prob(unconstrained_variables = c(0.1)),
    "created from an executable alone", fixed = TRUE
  )
})

test_that("init_model_methods(quiet = TRUE) suppresses the message", {
  rlang::local_interactive(TRUE)
  # A fit read back from disk has no live bindings, so init_model_methods()
  # compiles them. Both compile steps are mocked: only the message matters.
  local_mocked_bindings(
    rcpp_source_stan = function(...) invisible(NULL),
    initialize_model_pointer = function(...) invisible(NULL)
  )
  temp_rds_file <- tempfile(fileext = ".RDS")
  saveRDS(fit, temp_rds_file)
  expect_message(
    readRDS(temp_rds_file)$init_model_methods(),
    "Compiling additional model methods"
  )
  expect_no_message(readRDS(temp_rds_file)$init_model_methods(quiet = TRUE))
})
