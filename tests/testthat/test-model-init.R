context("model-init")

if (not_on_cran()) {
  set_cmdstan_path()
  mod <- cmdstan_model(stan_file = beroulli_example_file())
  data_list <- bernoulli_example_data()
}

# these create _relative_ paths to init files
init_json_1 <- test_path("resources", "init", "bernoulli.init-1.json")
init_json_2 <- test_path("resources", "init", "bernoulli.init-2.json")

test_that("all fitting methods work with provided init files", {
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
    mod$sample(data = data_list, init = data.frame(x = 10)),
    "If specified 'init' must be numeric or a character vector",
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
