context("model-init")
NOT_CRAN <- identical(Sys.getenv("NOT_CRAN"), "true")

if (NOT_CRAN) {
  set_cmdstan_path()
  stan_program <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  mod <- cmdstan_model(stan_file = stan_program)
  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
}

# these create _relative_ paths to init files
init_json_1 <- test_path("resources", "init", "bernoulli.init-1.json")
init_json_2 <- test_path("resources", "init", "bernoulli.init-2.json")

test_that("fitting methods work with provided init files", {
  skip_on_cran()

  expect_sample_output(
    mod$sample(data = data_list, num_chains = 1, init = init_json_1, seed = 123)
  )
  expect_optim_output(
    mod$optimize(data = data_list, init = init_json_1, seed = 123)
  )
  expect_vb_output(
    mod$variational(data = data_list, init = init_json_1, seed = 123)
  )

  # broadcasting
  expect_sample_output(
    mod$sample(data = data_list, num_chains = 2, init = init_json_1)
  )
})

test_that("sample method works with valid numeric init values", {
  skip_on_cran()

  expect_sample_output(
    mod$sample(data = data_list, num_chains = 1, init = 0)
  )

  expect_sample_output(
    mod$sample(data = data_list, num_chains = 1, init = 2)
  )
})

test_that("sample method throws error for invalid init argument", {
  skip_on_cran()

  expect_error(
    mod$sample(data = data_list, num_chains = 2, init = -10),
    "If 'init' is numeric it must be a single real number >= 0",
    fixed = TRUE
  )

  expect_error(
    mod$sample(data = data_list, num_chains = 1, init = "NOT_A_FILE"),
    "File does not exist"
  )

  expect_error(
    mod$sample(data = data_list, num_chains = 2, init = c("NOT_A_FILE", "ALSO_NOT_A_FILE")),
    "File does not exist"
  )

  expect_error(
    mod$sample(data = data_list, num_chains = 3, init = c(init_json_1, init_json_2)),
    "length 1 or length 'num_chains'"
  )
})
