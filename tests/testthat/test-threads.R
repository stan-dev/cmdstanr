context("model-sample")

if (not_on_cran()) {
  set_cmdstan_path()
  stan_program <- testing_stan_file("bernoulli")
  data_file_json <- test_path("resources", "data", "bernoulli.data.json")
}

test_that("using threads_per_chain without stan_threads set in compile() warns", {
  skip_on_cran()
  mod <- cmdstan_model(stan_program)
  expect_warning(
    expect_output(
      mod$sample(data = data_file_json, threads_per_chain = 4),
      "Running MCMC with 4 sequential chains",
      fixed = TRUE
    ),
    "'threads_per_chain' is set but the model was not compiled with 'cpp_options = list(stan_threads = TRUE)' so 'threads_per_chain' will have no effect!",
    fixed = TRUE)
})

test_that("threading works", {
  skip_on_cran()
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE))

  expect_error(
    mod$sample(data = data_file_json),
    "The model was compiled with 'cpp_options = list(stan_threads = TRUE)' but 'threads_per_chain' was not set!",
    fixed = TRUE
  )

  expect_output(
    mod$sample(data = data_file_json, parallel_chains = 4, threads_per_chain = 1),
    "Running MCMC with 4 parallel chains, with 1 thread(s) per chain..",
    fixed = TRUE
  )
  num_threads <- as.integer(Sys.getenv("STAN_NUM_THREADS"))
  expect_equal(num_threads, 1)
  expect_output(
    mod$sample(data = data_file_json,  parallel_chains = 4, threads_per_chain = 2),
    "Running MCMC with 4 parallel chains, with 2 thread(s) per chain..",
    fixed = TRUE
  )
  num_threads <- as.integer(Sys.getenv("STAN_NUM_THREADS"))
  expect_equal(num_threads, 2)
  expect_output(
    mod$sample(data = data_file_json,  parallel_chains = 4, threads_per_chain = 4),
    "Running MCMC with 4 parallel chains, with 4 thread(s) per chain..",
    fixed = TRUE
  )
  num_threads <- as.integer(Sys.getenv("STAN_NUM_THREADS"))
  expect_equal(num_threads, 4)
})

