context("model-init")

set_cmdstan_path()
mod <- testing_model("bernoulli")
data_list <- testing_data("bernoulli")

mod_logistic <- testing_model("logistic")
data_list_logistic <- testing_data("logistic")

# these create _relative_ paths to init files
init_json_1 <- test_path("resources", "init", "bernoulli.init-1.json")
init_json_2 <- test_path("resources", "init", "bernoulli.init-2.json")


test_that("all fitting methods work with provided init files", {
  expect_sample_output(
    mod$sample(data = data_list, chains = 1, init = init_json_1, seed = 123)
  )
  expect_optim_output(
    mod$optimize(data = data_list, init = init_json_1, seed = 123)
  )
  expect_vb_output(
    mod$variational(data = data_list, init = init_json_1, seed = 1234)
  )
  expect_laplace_output(
    mod$laplace(data = data_list, init = init_json_1, seed = 123)
  )

  # broadcasting
  expect_sample_output(
    fit <- mod$sample(data = data_list, chains = 2, init = init_json_1, seed = 123)
  )
  expect_identical(
    fit$init(),
    list(
      jsonlite::read_json(init_json_1, simplifyVector = TRUE),
      jsonlite::read_json(init_json_1, simplifyVector = TRUE)
    )
  )
})

test_that("sample method works with valid numeric init values", {
  expect_sample_output(
    mod$sample(data = data_list, chains = 1, init = 0, seed = 123)
  )

  expect_sample_output(
    mod$sample(data = data_list, chains = 1, init = 2, seed = 123)
  )
})

test_that("sample method throws error for invalid init argument", {
  expect_error(
    mod$sample(data = data_list, chains = 2, init = -10, seed = 123),
    "If 'init' is numeric it must be a single real number >= 0",
    fixed = TRUE
  )

  expect_error(
    mod$sample(data = data_list, init = data.frame(x = 10)),
    "Invalid 'init' specification",
    fixed = TRUE
  )

  expect_error(
    mod$sample(data = data_list, chains = 1, init = "NOT_A_FILE"),
    "File does not exist"
  )

  expect_error(
    mod$sample(data = data_list, chains = 2, init = c("NOT_A_FILE", "ALSO_NOT_A_FILE")),
    "File does not exist"
  )

  expect_error(
    mod$sample(data = data_list, chains = 3, init = c(init_json_1, init_json_2)),
    "length 1 or number of chains"
  )
})

test_that("init can be a list of lists", {
  init_list <- list(
    list(
      alpha = 1,
      beta = c(-1, 0, 1)
    ),
    list(
      alpha = 0,
      beta = c(-2, 1, 2)
    )
  )
  expect_optim_output(
    fit <- mod_logistic$optimize(data = data_list_logistic, init = init_list[1], seed = 123)
  )
  expect_length(fit$metadata()$init, 1)

  expect_laplace_output(
    fit <- mod_logistic$laplace(data = data_list_logistic, init = init_list[1], seed = 123)
  )
  expect_length(fit$metadata()$init, 1)

  expect_sample_output(
    fit <- mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_list, seed = 123),
    num_chains = 2
  )

  expect_length(fit$init(), 2)
  expect_identical(
    fit$init(),
    list(
      jsonlite::read_json(fit$metadata()$init[1], simplifyVector = TRUE),
      jsonlite::read_json(fit$metadata()$init[2], simplifyVector = TRUE)
    )
  )

  # partial inits ok
  init_list <- list(list(alpha = 0))
  expect_sample_output(
    fit <- mod_logistic$sample(data = data_list_logistic, chains = 1, init = init_list),
    num_chains = 1
  )
  expect_length(fit$init(), 1)
  expect_identical(
    fit$init(),
    list(jsonlite::read_json(fit$metadata()$init[1], simplifyVector = TRUE))
  )
})

test_that("error if init list is specified incorrectly", {
  init_list <- list(alpha = 1, beta = c(1,1))
  expect_error(
    mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_list),
    "If 'init' is a list it must be a list of lists"
  )

  init_list <- list(init_list)
  expect_error(
    mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_list),
    "'init' has the wrong length"
  )

  init_list <- list(
    list(alpha = 1, beta = 1:3),
    list(alpha = 1, beta = 1:3)
  )
  expect_error(
    mod_logistic$optimize(data = data_list_logistic, init = init_list, seed = 123),
    "'init' has the wrong length"
  )

  init_list <- list(list(), list())
  expect_error(
    mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_list),
    "'init' contains empty lists."
  )

  init_list <- list()
  init_list[[1]] = list()
  init_list[[1]]['alpha'] = 1
  init_list[[1]]['beta[1]'] = -1
  init_list[[1]]['beta[2]'] = 0
  init_list[[1]]['beta[3]'] = 1
  init_list[[2]] = init_list[[1]]
  expect_error(
    mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_list),
    "'init' contains entries with parameter names that include square-brackets, which is not permitted."
  )

})

test_that("init can be a function", {
  init_fun <- function() {
    list(alpha = 0, beta = 1:3)
  }
  expect_optim_output(
    fit <- mod_logistic$optimize(data = data_list_logistic, init = init_fun, seed = 123)
  )
  expect_sample_output(
    fit <- mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_fun),
    num_chains = 2
  )
  expect_length(fit$init(), 2)
  expect_identical(
    fit$init(),
    list(
      jsonlite::read_json(fit$metadata()$init[1], simplifyVector = TRUE),
      jsonlite::read_json(fit$metadata()$init[2], simplifyVector = TRUE)
    )
  )

  # check that chain_id argument is allowed
  init_fun <- function(chain_id) {
    list(alpha = 0, beta = 1:3)
  }
  expect_optim_output(
    fit <- mod_logistic$optimize(data = data_list_logistic, init = init_fun, seed = 123)
  )
  expect_sample_output(
    fit <- mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_fun),
    num_chains = 2
  )
  expect_length(fit$init(), 2)
  expect_identical(
    fit$init(),
    list(
      jsonlite::read_json(fit$metadata()$init[1], simplifyVector = TRUE),
      jsonlite::read_json(fit$metadata()$init[2], simplifyVector = TRUE)
    )
  )
})

test_that("error if init function specified incorrectly", {
  init_fun <- function(a, b) list(a, b)
  expect_error(
    mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_fun),
    "If 'init' is a function it must have zero arguments or only argument 'chain_id'"
  )

  init_fun <- function() {
    c(a = 1, b = 1:3)
  }
  expect_error(
    mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_fun),
    "If 'init' is a function it must return a single list"
  )

  init_fun <- function() {
    data.frame(a = 1, b = 1:3)
  }
  expect_error(
    mod_logistic$sample(data = data_list_logistic, chains = 2, init = init_fun),
    "If 'init' is a function it must return a single list"
  )

  init_fun <- function() list()
  expect_error(
    mod_logistic$sample(data = data_list_logistic, chains = 1, init = init_fun),
    "'init' contains empty lists."
  )
})

test_that("print message if not all parameters are initialized", {
  options(cmdstanr_warn_inits = NULL) # should default to TRUE
  init_list <- list(
    list(
      alpha = 1
    )
  )
  expect_message(
    utils::capture.output(mod_logistic$optimize(data = data_list_logistic, init = init_list, seed = 123)),
    "beta",
    fixed = TRUE
  )
  expect_message(
    utils::capture.output(mod_logistic$optimize(data = data_list_logistic, init = list(list(a = 0)), seed = 123)),
    "alpha, beta",
    fixed = TRUE
  )

  init_list <- list(list(alpha = 1),list(alpha = 1))
  expect_message(
    utils::capture.output(mod_logistic$sample(data = data_list_logistic, init = init_list, seed = 123, chains = 2)),
    "- chain 2: beta",
    fixed = TRUE
  )

  init_list <- list(list(alpha = 1),list(a = 1))
  expect_message(
    utils::capture.output(mod_logistic$sample(data = data_list_logistic, init = init_list, seed = 123, chains = 2)),
    "- chain 2: alpha, beta",
    fixed = TRUE
  )
})

test_that("No message printed if options(cmdstanr_warn_inits=FALSE)", {
  options(cmdstanr_warn_inits = FALSE)
  expect_message(
    utils::capture.output(mod_logistic$optimize(data = data_list_logistic, init = list(list(a = 0)), seed = 123)),
    regexp = NA
  )
  expect_message(
    utils::capture.output(mod_logistic$optimize(data = data_list_logistic, init = list(list(alpha = 1)), seed = 123)),
    regexp = NA
  )
  expect_message(
    utils::capture.output(mod_logistic$sample(data = data_list_logistic, init = list(list(alpha = 1),list(alpha = 1)), chains = 2, seed = 123)),
    regexp = NA
  )
  options(cmdstanr_warn_inits = TRUE)
})

test_that("Initial values for single-element containers treated correctly", {
  modcode <- "
  data {
    real y_mean;
  }
  parameters {
    vector[1] y;
  }
  model {
    y_mean ~ normal(y[1], 1);
  }
  "
  mod <- cmdstan_model(write_stan_file(modcode), force_recompile = TRUE)
  expect_no_error(
    utils::capture.output(
      fit <- mod$sample(
        data = list(y_mean = 0),
        init = list(list(y = c(0))),
        chains = 1
      )
    )
  )
})

test_that("Pathfinder inits do not drop dimensions", {
  modcode <- "
  data {
    int N;
    vector[N] y;
  }

  parameters {
    matrix[N, 1] mu;
    matrix[1, N] mu_2;
    vector<lower=0>[N] sigma;
  }

  model {
    target += normal_lupdf(y | mu[:, 1], sigma);
    target += normal_lupdf(y | mu_2[1], sigma);
  }
  "
  mod <- cmdstan_model(write_stan_file(modcode), force_recompile = TRUE)
  data <- list(N = 100, y = rnorm(100))
  utils::capture.output(
    pf <- mod$pathfinder(data = data, psis_resample = FALSE)
  )
  expect_no_error(
    utils::capture.output(
      fit <- mod$sample(data = data, init = pf, chains = 1,
                        iter_warmup = 100, iter_sampling = 100)
    )
  )
})
