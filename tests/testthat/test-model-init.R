context("model-init")

if (not_on_cran()) {
  set_cmdstan_path()
  mod <- testing_model("bernoulli")
  data_list <- testing_data("bernoulli")

  mod_logistic <- testing_model("logistic")
  data_list_logistic <- testing_data("logistic")
}

# these create _relative_ paths to init files
init_json_1 <- test_path("resources", "init", "bernoulli.init-1.json")
init_json_2 <- test_path("resources", "init", "bernoulli.init-2.json")

test_that("all fitting methods work with provided init files", {
  skip_on_cran()

  expect_sample_output(
    mod$sample(data = data_list, chains = 1, init = init_json_1, seed = 123)
  )
  expect_optim_output(
    mod$optimize(data = data_list, init = init_json_1, seed = 123)
  )
  expect_vb_output(
    mod$variational(data = data_list, init = init_json_1, seed = 123)
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
  skip_on_cran()

  expect_sample_output(
    mod$sample(data = data_list, chains = 1, init = 0, seed = 123)
  )

  expect_sample_output(
    mod$sample(data = data_list, chains = 1, init = 2, seed = 123)
  )
})

test_that("sample method throws error for invalid init argument", {
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()
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
  skip_on_cran()

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
