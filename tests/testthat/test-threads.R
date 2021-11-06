context("threading")

set_cmdstan_path()
stan_program <- testing_stan_file("bernoulli")
stan_gq_program <- testing_stan_file("bernoulli_ppc")
data_file_gq_json <- testing_data("bernoulli_ppc")
data_file_json <- test_path("resources", "data", "bernoulli.data.json")


test_that("using threads_per_chain without stan_threads set in compile() warns", {
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

test_that("threading works with sample()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)

  expect_error(
    mod$sample(data = data_file_json),
    "The model was compiled with 'cpp_options = list(stan_threads = TRUE)' but 'threads_per_chain' was not set!",
    fixed = TRUE
  )

  expect_output(
    f <- mod$sample(data = data_file_json, parallel_chains = 4, threads_per_chain = 1),
    "Running MCMC with 4 parallel chains, with 1 thread(s) per chain..",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 1)
  expect_equal(f$metadata()$threads_per_chain, 1)

  expect_output(
    f <- mod$sample(data = data_file_json,  parallel_chains = 4, threads_per_chain = 2),
    "Running MCMC with 4 parallel chains, with 2 thread(s) per chain..",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 2)
  expect_equal(f$metadata()$threads_per_chain, 2)
  expect_output(
    f <- mod$sample(data = data_file_json,  parallel_chains = 4, threads_per_chain = 4),
    "Running MCMC with 4 parallel chains, with 4 thread(s) per chain..",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 4)
  expect_equal(f$metadata()$threads_per_chain, 4)
})

test_that("threading works with optimize()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)

  expect_error(
    mod$optimize(data = data_file_json),
    "The model was compiled with 'cpp_options = list(stan_threads = TRUE)' but 'threads' was not set!",
    fixed = TRUE
  )

  expect_output(
    f <- mod$optimize(data = data_file_json, threads = 1, seed = 123),
    "Optimization terminated normally",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 1)
  expect_equal(f$metadata()$threads, 1)

  expect_output(
    f <- mod$optimize(data = data_file_json, threads = 2, seed = 123),
    "Optimization terminated normally",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 2)
  expect_equal(f$metadata()$threads, 2)

  expect_output(
    f <- mod$optimize(data = data_file_json, threads = 4, seed = 123),
    "Optimization terminated normally",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 4)
  expect_equal(f$metadata()$threads, 4)
})

test_that("threading works with variational()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)

  expect_error(
    mod$variational(data = data_file_json),
    "The model was compiled with 'cpp_options = list(stan_threads = TRUE)' but 'threads' was not set!",
    fixed = TRUE
  )

  expect_output(
    f <- mod$variational(data = data_file_json, threads = 1, seed = 123),
    "EXPERIMENTAL ALGORITHM",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 1)
  expect_equal(f$metadata()$threads, 1)

  expect_output(
    f <- mod$variational(data = data_file_json, threads = 2, seed = 123),
    "EXPERIMENTAL ALGORITHM",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 2)
  expect_equal(f$metadata()$threads, 2)

  expect_output(
    f <- mod$variational(data = data_file_json, threads = 4, seed = 123),
    "EXPERIMENTAL ALGORITHM",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 4)
  expect_equal(f$metadata()$threads, 4)
})

test_that("threading works with generate_quantities()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)
  mod_gq <- cmdstan_model(stan_gq_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)
  expect_output(
    f <- mod$sample(data = data_file_json, parallel_chains = 4, threads_per_chain = 1),
    "Running MCMC with 4 parallel chains, with 1 thread(s) per chain..",
    fixed = TRUE
  )
  expect_error(
    mod_gq$generate_quantities(fitted_params = f, data = data_file_json),
    "The model was compiled with 'cpp_options = list(stan_threads = TRUE)' but 'threads_per_chain' was not set!",
    fixed = TRUE
  )
  expect_output(
    f_gq <- mod_gq$generate_quantities(fitted_params = f, data = data_file_gq_json, threads_per_chain = 1, seed = 123),
    "Running standalone generated quantities after 4 MCMC chains",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 1)
  expect_equal(f_gq$metadata()$threads_per_chain, 1)

  expect_output(
    f_gq <- mod_gq$generate_quantities(fitted_params = f, data = data_file_gq_json, threads_per_chain = 2, seed = 123),
    "Running standalone generated quantities after 4 MCMC chains",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 2)
  expect_equal(f_gq$metadata()$threads_per_chain, 2)

  expect_output(
    f_gq <- mod_gq$generate_quantities(fitted_params = f, data = data_file_gq_json, threads_per_chain = 4, seed = 123),
    "Running standalone generated quantities after 4 MCMC chains",
    fixed = TRUE
  )
  expect_equal(as.integer(Sys.getenv("STAN_NUM_THREADS")), 4)
  expect_equal(f_gq$metadata()$threads_per_chain, 4)
})

test_that("correct output when stan_threads not TRUE", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = FALSE), force_recompile = TRUE)
  expect_output(
    mod$sample(data = data_file_json),
    "Running MCMC with 4 sequential chains",
    fixed = TRUE
  )
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = "dummy string"), force_recompile = TRUE)
  expect_output(
    mod$sample(data = data_file_json),
    "Running MCMC with 4 sequential chains",
    fixed = TRUE
  )
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = FALSE), force_recompile = TRUE)
  expect_warning(
    mod$sample(data = data_file_json, threads_per_chain = 4),
    "'threads_per_chain' is set but the model was not compiled with 'cpp_options = list(stan_threads = TRUE)' so 'threads_per_chain' will have no effect!",
    fixed = TRUE
  )
})
