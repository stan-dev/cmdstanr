context("model-expose-functions")

set_cmdstan_path()

function_decl <- "functions { vector retvec(vector x) { return x; } }"
stan_prog <- paste(function_decl,
                   paste(readLines(testing_stan_file("bernoulli")),
                         collapse = "\n"),
                   collapse = "\n")
model <- write_stan_file(stan_prog)
data_list <- testing_data("bernoulli")
mod <- cmdstan_model(model, force_recompile = TRUE)
fit <- mod$sample(data = data_list)


test_that("Functions can be exposed in model object", {
  skip_if(os_is_wsl())
  mod$expose_functions(verbose = TRUE)

  expect_equal(
    mod$functions$retvec(c(1,2,3,4)),
    c(1,2,3,4)
  )

  mod$expose_functions(global = TRUE, verbose = TRUE)

  expect_equal(
    retvec(c(1,2,3,4)),
    c(1,2,3,4)
  )
})

test_that("Functions can be exposed in fit object", {
  skip_if(os_is_wsl())
  fit$expose_functions(verbose = TRUE)

  expect_equal(
    fit$functions$retvec(c(1,2,3,4)),
    c(1,2,3,4)
  )
})

test_that("Compiled functions can be copied to global environment", {
  skip_if(os_is_wsl())
  expect_message(
    fit$expose_functions(global = TRUE),
    "Functions already compiled, copying to global environment",
    fixed = TRUE
  )

  expect_equal(
    retvec(c(1,2,3,4)),
    c(1,2,3,4)
  )
})


test_that("Functions can be compiled with model", {
  skip_if(os_is_wsl())
  mod <- cmdstan_model(model, force_recompile = TRUE, compile_standalone = TRUE)
  fit <- mod$sample(data = data_list)

  expect_message(
    fit$expose_functions(),
    "Functions already compiled, nothing to do!",
    fixed = TRUE
  )

  expect_equal(
    fit$functions$retvec(c(1,2,3,4)),
    c(1,2,3,4)
  )

  expect_message(
    fit$expose_functions(global = TRUE),
    "Functions already compiled, copying to global environment",
    fixed = TRUE
  )

  expect_equal(
    retvec(c(1,2,3,4)),
    c(1,2,3,4)
  )
})

test_that("rng functions can be exposed", {
  skip_if(os_is_wsl())
  function_decl <- "functions { real normal_rng(real mu) { return normal_rng(mu, 1); } }"
  stan_prog <- paste(function_decl,
                     paste(readLines(testing_stan_file("bernoulli")),
                           collapse = "\n"),
                     collapse = "\n")
  model <- write_stan_file(stan_prog)
  data_list <- testing_data("bernoulli")
  mod <- cmdstan_model(model, force_recompile = TRUE)
  fit <- mod$sample(data = data_list)

  fit$expose_functions(verbose = TRUE)

  expect_equal(
    fit$functions$normal_rng(5, seed = 10),
    3.8269637967017344771
  )
})
