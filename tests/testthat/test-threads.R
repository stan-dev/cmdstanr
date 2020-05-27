context("model-sample")

if (not_on_cran()) {
  set_cmdstan_path()
  stan_program <- testing_stan_file("bernoulli")
  data_file_json <- test_path("resources", "data", "bernoulli.data.json")
}

test_that("using threads_per_chain without stan_threads set in compile() warns", {
  skip_on_cran()
  mod <- cmdstan_model(stan_program, force_recompile = TRUE)

  expect_warning(
    expect_output(
      mod$sample(data = data_file_json, threads_per_chain = 4),
      "Running MCMC with 4 chain(s) on 1 core(s)",
      fixed = TRUE
    ),
    "'threads_per_chain' is set but the model was not compiled with 'cpp_options = list(stan_threads = TRUE)' so 'threads_per_chain' will have no effect!",
    fixed = TRUE)
})

test_that("threading works", {
  skip_on_cran()
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)

  expect_warning(
    expect_output(
      mod$sample(data = data_file_json),
      "Running MCMC with 4 chain(s) on 1 core(s)",
      fixed = TRUE
    ),
    "The model was compiled with 'cpp_options = list(stan_threads = TRUE)' but the number of threads per chain was not set! The sampling with run with one thread per chain. Use 'threads_per_chain' to set the number of threads.",
    fixed = TRUE)

  expect_output(
    mod$sample(data = data_file_json, cores = 4, threads_per_chain = 1),
    "Running MCMC with 4 chain(s) on 4 core(s) with 1  thread(s) per chain..",
    fixed = TRUE
  )
  expect_output(
    mod$sample(data = data_file_json,  cores = 4, threads_per_chain = 2),
    "Running MCMC with 4 chain(s) on 4 core(s) with 2  thread(s) per chain..",
    fixed = TRUE
  )
  expect_output(
    mod$sample(data = data_file_json,  cores = 4, threads_per_chain = 4),
    "Running MCMC with 4 chain(s) on 4 core(s) with 4  thread(s) per chain..",
    fixed = TRUE
  )


  #recompile bernoulli with no threads to be used in other tests
  mod <- cmdstan_model(stan_program, force_recompile = TRUE)
})

