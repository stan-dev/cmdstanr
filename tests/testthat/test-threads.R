set_cmdstan_path()
stan_program <- testing_stan_file("bernoulli")
stan_gq_program <- testing_stan_file("bernoulli_ppc")
data_file_gq_json <- testing_data("bernoulli_ppc")
data_file_json <- test_path("resources", "data", "bernoulli.data.json")


test_that("using threads_per_chain without stan_threads set in compile() warns", {
  mod <- cmdstan_model(
    stan_program,
    cpp_options = list(stan_threads = NULL),
    force_recompile = TRUE
  )
  if (isTRUE(mod$exe_info()$stan_threads)) {
    skip("The local CmdStan make configuration forces STAN_THREADS=true.")
  }
  expect_warning(
    expect_output(
      mod$sample(
        data = data_file_json,
        threads_per_chain = 4,
        iter_warmup = 2,
        iter_sampling = 2,
        refresh = 0,
        diagnostics = ""
      ),
      "Running 4 chains in 4 CmdStan invocations with 1 total thread",
      fixed = TRUE
    ),
    "'threads_per_chain=4' was supplied for an unthreaded executable and has no effect.",
    fixed = TRUE)
})

test_that("threading works with sample()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)
  withr::local_envvar(c(STAN_NUM_THREADS = "17"))

  expect_error(
    mod$sample(data = data_file_json, threads = 2, threads_per_chain = 1),
    "cannot both be supplied"
  )

  expect_output(
    f <- mod$sample(
      data = data_file_json,
      parallel_chains = 4,
      threads_per_chain = 2,
      iter_warmup = 2,
      iter_sampling = 2,
      refresh = 0,
      diagnostics = ""
    ),
    "Running 4 chains in 1 CmdStan invocation with 8 total threads",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$num_chains(), 4)
  expect_equal(f$num_procs(), 1)
  expect_length(f$return_codes(), 1)
  expect_length(f$output_files(), 4)
  expect_equal(f$metadata()$num_threads, 8)
  expect_equal(f$metadata()$threads_per_chain, 2)

  expect_output(
    f <- mod$sample(
      data = data_file_json,
      parallel_chains = 4,
      threads = 3,
      seed = 123,
      iter_warmup = 2,
      iter_sampling = 2,
      refresh = 0,
      diagnostics = ""
    ),
    "Running 4 chains in 1 CmdStan invocation with 3 total threads",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$metadata()$num_threads, 3)
  expect_null(f$metadata()$threads_per_chain)

  utils::capture.output(
    f_repeated <- mod$sample(
      data = data_file_json,
      parallel_chains = 4,
      threads = 3,
      seed = 123,
      iter_warmup = 2,
      iter_sampling = 2,
      refresh = 0,
      diagnostics = ""
    )
  )
  expect_equal(f_repeated$draws(), f$draws())

  scalarization_warnings <- character()
  utils::capture.output(
    f_scalarized <- withCallingHandlers(
      mod$sample(
        data = data_file_json,
        chains = 2,
        parallel_chains = 2,
        chain_ids = c(10, 20),
        threads = 2,
        seed = c(101, 202),
        step_size = c(0.1, 0.2),
        iter_warmup = 2,
        iter_sampling = 2,
        refresh = 0,
        diagnostics = ""
      ),
      warning = function(warning) {
        scalarization_warnings <<- c(
          scalarization_warnings,
          conditionMessage(warning)
        )
        invokeRestart("muffleWarning")
      }
    )
  )
  warning_text <- paste(scalarization_warnings, collapse = " ")
  expect_match(warning_text, "seed=101.*remaining values were ignored")
  expect_match(warning_text, "step_size=0.1.*remaining values were ignored")
  expect_match(warning_text, "effective chain IDs are 10, 11")
  expect_equal(f_scalarized$runset$chain_ids(), 10:11)
})

test_that("threading works with optimize()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)
  withr::local_envvar(c(STAN_NUM_THREADS = "17"))

  expect_output(
    f <- mod$optimize(data = data_file_json, seed = 123),
    "Optimization terminated normally",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$metadata()$threads, 1)

  expect_output(
    f <- mod$optimize(data = data_file_json, threads = 2, seed = 123),
    "Optimization terminated normally",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$metadata()$threads, 2)

  expect_output(
    f <- mod$optimize(data = data_file_json, threads = 4, seed = 123),
    "Optimization terminated normally",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$metadata()$threads, 4)
})

test_that("threading works with variational()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)
  withr::local_envvar(c(STAN_NUM_THREADS = "17"))

  expect_output(
    f <- mod$variational(data = data_file_json, seed = 123),
    "EXPERIMENTAL ALGORITHM",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$metadata()$threads, 1)

  expect_output(
    f <- mod$variational(data = data_file_json, threads = 2, seed = 123),
    "EXPERIMENTAL ALGORITHM",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$metadata()$threads, 2)

  expect_output(
    f <- mod$variational(data = data_file_json, threads = 4, seed = 123),
    "EXPERIMENTAL ALGORITHM",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$metadata()$threads, 4)
})

test_that("threading works with pathfinder()", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = TRUE),
                       force_recompile = TRUE)
  withr::local_envvar(c(STAN_NUM_THREADS = "17"))
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

  expect_output(
    f <- do.call(mod$pathfinder, pathfinder_args),
    "Finished in",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$metadata()$threads, 1)

  pathfinder_args$threads <- 2
  expect_output(
    f <- do.call(mod$pathfinder, pathfinder_args),
    "Finished in",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f$metadata()$threads, 2)

  pathfinder_args$num_threads <- 2
  expect_error(
    do.call(mod$pathfinder, pathfinder_args),
    "Cannot specify both 'threads' and deprecated 'num_threads'"
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
  withr::local_envvar(c(STAN_NUM_THREADS = "17"))
  expect_output(
    f <- mod$sample(
      data = data_file_json,
      parallel_chains = 4,
      threads = 2,
      iter_warmup = 2,
      iter_sampling = 2,
      refresh = 0,
      diagnostics = ""
    ),
    "Running 4 chains in 1 CmdStan invocation with 2 total threads",
    fixed = TRUE
  )
  expect_output(
    f_gq <- mod_gq$generate_quantities(
      fitted_params = f,
      data = data_file_gq_json,
      parallel_chains = 4,
      threads_per_chain = 2,
      seed = 123
    ),
    "Running standalone generated quantities for 4 chains in 1 CmdStan invocation",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f_gq$num_chains(), 4)
  expect_equal(f_gq$num_procs(), 1)
  expect_length(f_gq$return_codes(), 1)
  expect_length(f_gq$output_files(), 4)
  expect_equal(f_gq$metadata()$num_threads, 8)
  expect_equal(f_gq$metadata()$threads_per_chain, 2)

  expect_output(
    f_gq <- mod_gq$generate_quantities(
      fitted_params = f,
      data = data_file_gq_json,
      threads = 3,
      seed = 123
    ),
    "Running standalone generated quantities for 4 chains in 1 CmdStan invocation",
    fixed = TRUE
  )
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
  expect_equal(f_gq$metadata()$num_threads, 3)
  expect_null(f_gq$metadata()$threads_per_chain)
})

test_that("correct output when stan_threads not TRUE", {
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = NULL), force_recompile = TRUE)
  if (isTRUE(mod$exe_info()$stan_threads)) {
    skip("The local CmdStan make configuration forces STAN_THREADS=true.")
  }
  expect_output(
    mod$sample(data = data_file_json, iter_warmup = 2, iter_sampling = 2,
               refresh = 0, diagnostics = ""),
    "Running 4 chains in 4 CmdStan invocations with 1 total thread",
    fixed = TRUE
  )
  mod <- cmdstan_model(stan_program, cpp_options = list(stan_threads = NULL), force_recompile = TRUE)
  expect_output(
    expect_warning(
      mod$sample(data = data_file_json, threads_per_chain = 4,
                 iter_warmup = 2, iter_sampling = 2, refresh = 0,
                 diagnostics = ""),
      "'threads_per_chain=4' was supplied for an unthreaded executable and has no effect.",
      fixed = TRUE
    ),
    "Running 4 chains in 4 CmdStan invocations with 1 total thread",
    fixed = TRUE
  )
})

test_that("executable metadata takes precedence over compile options", {
  mod <- cmdstan_model(
    stan_program,
    cpp_options = list(stan_threads = FALSE),
    force_recompile = TRUE
  )
  if (!isTRUE(mod$exe_info()$stan_threads)) {
    skip("The local CmdStan make configuration does not force STAN_THREADS=true.")
  }
  expect_output(
    fit <- mod$sample(
      data = data_file_json,
      chains = 1,
      threads_per_chain = 2,
      iter_warmup = 2,
      iter_sampling = 2,
      refresh = 0,
      diagnostics = ""
    ),
    "Running 1 chain in 1 CmdStan invocation with 2 total threads",
    fixed = TRUE
  )
  expect_equal(fit$metadata()$num_threads, 2)
  expect_equal(fit$metadata()$threads_per_chain, 2)
})
