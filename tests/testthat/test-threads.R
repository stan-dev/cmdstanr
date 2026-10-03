skip_on_cran()

set_cmdstan_path()
stan_program <- testing_stan_file("bernoulli")
stan_gq_program <- testing_stan_file("bernoulli_ppc")
data_file_gq_json <- testing_data("bernoulli_ppc")
data_file_json <- test_path("resources", "data", "bernoulli.data.json")


test_that("threads_per_chain on an executable without threading errors", {
  mod <- cmdstan_model(stan_program)
  expect_error(
    mod$sample(data = data_file_json, threads_per_chain = 4),
    "does not report threading as enabled",
    fixed = TRUE
  )
  expect_output(
    mod$sample(data = data_file_json, chains = 1, threads_per_chain = 1),
    "Running MCMC with 1 chain",
    fixed = TRUE
  )
})

test_that("threading works with sample()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)

  expect_output(
    f <- mod$sample(data = data_file_json, parallel_chains = 4, threads_per_chain = 1),
    "Running MCMC with 4 parallel chains, with 1 thread(s) per chain..",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads_per_chain, 1)

  expect_output(
    f <- mod$sample(data = data_file_json,  parallel_chains = 4, threads_per_chain = 2),
    "Running MCMC with 4 parallel chains, with 2 thread(s) per chain..",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads_per_chain, 2)
  expect_output(
    f <- mod$sample(data = data_file_json,  parallel_chains = 4, threads_per_chain = 4),
    "Running MCMC with 4 parallel chains, with 4 thread(s) per chain..",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads_per_chain, 4)
})

test_that("the thread count reaches the child process and not the session", {
  withr::local_envvar(STAN_NUM_THREADS = NA)
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE))
  utils::capture.output({
    with_threads <- mod$sample(
      data = data_file_json, chains = 1, threads_per_chain = 4
    )
    without <- mod$sample(data = data_file_json, chains = 1)
  })
  expect_equal(with_threads$metadata()$threads_per_chain, 4)
  expect_equal(without$metadata()$threads_per_chain, 1)
  expect_true(is.na(Sys.getenv("STAN_NUM_THREADS", unset = NA)))
})

test_that("WSLENV keeps the entries the session already exports", {
  skip_if(!os_is_wsl())

  withr::local_envvar(WSLENV = "A/u:STAN_NUM_THREADS/u:B/p")
  expect_equal(cmdstan_process_env(4)[["WSLENV"]], "A/u:B/p:STAN_NUM_THREADS/u")

  withr::local_envvar(WSLENV = NA)
  expect_equal(cmdstan_process_env(4)[["WSLENV"]], "STAN_NUM_THREADS/u")
})

test_that("threading works with optimize()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)

  expect_output(
    f <- mod$optimize(data = data_file_json, threads = 1, seed = 123),
    "Optimization terminated normally",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads, 1)

  expect_output(
    f <- mod$optimize(data = data_file_json, threads = 2, seed = 123),
    "Optimization terminated normally",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads, 2)

  expect_output(
    f <- mod$optimize(data = data_file_json, threads = 4, seed = 123),
    "Optimization terminated normally",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads, 4)
})

test_that("threading works with variational()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)

  expect_output(
    f <- mod$variational(data = data_file_json, threads = 1, seed = 123),
    "EXPERIMENTAL ALGORITHM",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads, 1)

  expect_output(
    f <- mod$variational(data = data_file_json, threads = 2, seed = 123),
    "EXPERIMENTAL ALGORITHM",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads, 2)

  expect_output(
    f <- mod$variational(data = data_file_json, threads = 4, seed = 123),
    "EXPERIMENTAL ALGORITHM",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads, 4)
})

test_that("threading works with pathfinder()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE),
                       force_recompile = TRUE)
  pathfinder_args <- list(
    data = data_file_json,
    seed = 123,
    refresh = 0,
    draws = 10,
    single_path_draws = 10,
    num_paths = 1,
    num_elbo_draws = 10,
    max_lbfgs_iters = 10
  )

  pathfinder_args$threads <- 2
  expect_output(
    f <- do.call(mod$pathfinder, pathfinder_args),
    "Finished in",
    fixed = TRUE
  )
  expect_equal(f$metadata()$threads, 2)

  pathfinder_args$num_threads <- 2
  expect_error(
    do.call(mod$pathfinder, pathfinder_args),
    "Cannot specify both `threads` and deprecated `num_threads`"
  )
  pathfinder_args$threads <- NULL
  pathfinder_args$show_messages <- FALSE
  expect_snapshot(
    invisible(do.call(mod$pathfinder, pathfinder_args))
  )
})

test_that("threading works with generate_quantities()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)
  mod_gq <- cmdstan_model(stan_gq_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)
  expect_output(
    f <- mod$sample(data = data_file_json, parallel_chains = 4, threads_per_chain = 1),
    "Running MCMC with 4 parallel chains, with 1 thread(s) per chain..",
    fixed = TRUE
  )
  expect_output(
    f_gq <- mod_gq$generate_quantities(fitted_params = f, data = data_file_gq_json, threads_per_chain = 1, seed = 123),
    "Running standalone generated quantities after 4 MCMC chains",
    fixed = TRUE
  )
  expect_equal(f_gq$metadata()$threads_per_chain, 1)

  expect_output(
    f_gq <- mod_gq$generate_quantities(fitted_params = f, data = data_file_gq_json, threads_per_chain = 2, seed = 123),
    "Running standalone generated quantities after 4 MCMC chains",
    fixed = TRUE
  )
  expect_equal(f_gq$metadata()$threads_per_chain, 2)

  expect_output(
    f_gq <- mod_gq$generate_quantities(fitted_params = f, data = data_file_gq_json, threads_per_chain = 4, seed = 123),
    "Running standalone generated quantities after 4 MCMC chains",
    fixed = TRUE
  )
  expect_equal(f_gq$metadata()$threads_per_chain, 4)
})

test_that("stan_threads = FALSE builds an executable without threading", {
  mod <- cmdstan_model(
    stan_program,
    cpp_options = list(stan_threads = FALSE),
    force_recompile = TRUE
  )
  # FALSE reaches make as an empty assignment, and is reported that way
  expect_equal(mod$cpp_options()$STAN_THREADS, "")
  expect_output(
    fit <- mod$sample(data = data_file_json, chains = 1),
    "Running MCMC with 1 chain",
    fixed = TRUE
  )
  expect_equal(fit$metadata()$threads_per_chain, 1)
  expect_error(
    mod$sample(data = data_file_json, chains = 1, threads_per_chain = 2),
    "does not report threading as enabled",
    fixed = TRUE
  )
})
