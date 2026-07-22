test_that("CmdStanRunPlan separates logical chains from invocations", {
  threaded_invocation <- new_cmdstan_invocation_plan(
    invocation_id = 1,
    chain_indices = 1:4,
    chain_ids = 10:13,
    num_threads = 8,
    command = "model",
    command_args = c("sample", "num_chains=4"),
    env = c(STAN_NUM_THREADS = "8")
  )
  threaded_plan <- CmdStanRunPlan$new(
    method = "sample",
    chain_ids = 10:13,
    invocations = list(threaded_invocation),
    chain_artifacts = list(
      output = paste0("output-", 10:13, ".csv"),
      diagnostic = paste0("diagnostic-", 10:13, ".csv"),
      metric = paste0("metric-", 10:13, ".json")
    ),
    invocation_artifacts = list(
      profile = "profile-1.csv",
      config = "config-1.json"
    ),
    requested_parallel_chains = 4,
    requested_threads_per_chain = 2
  )

  expect_equal(threaded_plan$num_chains(), 4L)
  expect_equal(threaded_plan$num_invocations(), 1L)
  expect_equal(threaded_plan$invocation_ids(), 1L)
  expect_equal(threaded_plan$chain_ids, 10:13)
  expect_equal(threaded_plan$invocations[[1]]$base_chain_id, 10L)
  expect_equal(threaded_plan$invocations[[1]]$num_chains, 4L)
  expect_equal(threaded_plan$invocations[[1]]$chain_indices, 1:4)

  unthreaded_invocations <- lapply(seq_len(4), function(i) {
    new_cmdstan_invocation_plan(
      invocation_id = i,
      chain_indices = i,
      chain_ids = c(9L, 2L, 20L, 7L)[i],
      num_threads = 1,
      command = "model",
      command_args = "sample"
    )
  })
  unthreaded_plan <- CmdStanRunPlan$new(
    method = "sample",
    chain_ids = c(9L, 2L, 20L, 7L),
    invocations = unthreaded_invocations,
    chain_artifacts = list(output = paste0("output-", seq_len(4), ".csv")),
    invocation_artifacts = list(profile = paste0("profile-", seq_len(4), ".csv")),
    requested_parallel_chains = 2
  )

  expect_equal(unthreaded_plan$num_chains(), 4L)
  expect_equal(unthreaded_plan$num_invocations(), 4L)
  expect_equal(unthreaded_plan$invocation_ids(), 1:4)
  expect_equal(
    vapply(unthreaded_plan$invocations, `[[`, integer(1), "chain_indices"),
    1:4
  )
})

test_that("CmdStanRunPlan rejects invalid mappings and artifact cardinality", {
  invocation <- new_cmdstan_invocation_plan(
    invocation_id = 1,
    chain_indices = 1:2,
    chain_ids = 1:2,
    num_threads = 2,
    command = "model"
  )

  expect_error(
    CmdStanRunPlan$new(
      method = "sample",
      chain_ids = 1:3,
      invocations = list(invocation),
      chain_artifacts = list(output = paste0("output-", 1:3, ".csv")),
      invocation_artifacts = list(profile = "profile.csv")
    ),
    "exactly once"
  )
  expect_error(
    CmdStanRunPlan$new(
      method = "sample",
      chain_ids = 1:2,
      invocations = list(invocation),
      chain_artifacts = list(output = "output.csv"),
      invocation_artifacts = list(profile = "profile.csv")
    ),
    "chain artifact 'output'"
  )
  expect_error(
    CmdStanRunPlan$new(
      method = "sample",
      chain_ids = 1:2,
      invocations = list(invocation),
      chain_artifacts = list(output = paste0("output-", 1:2, ".csv")),
      invocation_artifacts = list(profile = c("profile-1.csv", "profile-2.csv"))
    ),
    "invocation artifact 'profile'"
  )

  expect_error(
    new_cmdstan_invocation_plan(
      invocation_id = 1,
      chain_indices = 1:2,
      chain_ids = c(1, 3),
      num_threads = 2,
      command = "model"
    ),
    "consecutive"
  )
  expect_error(
    new_cmdstan_invocation_plan(
      invocation_id = 1,
      chain_indices = 1,
      chain_ids = 1,
      num_threads = 0,
      command = "model"
    ),
    "num_threads"
  )
})

test_that("resolve_num_threads uses canonical totals and heuristic pool sizing", {
  expect_equal(
    resolve_num_threads(
      threads = 7,
      threads_per_chain = NULL,
      chains = 4,
      parallel_chains = 2,
      stan_threads = TRUE
    ),
    7L
  )
  expect_equal(
    resolve_num_threads(
      threads = NULL,
      threads_per_chain = 3,
      chains = 4,
      parallel_chains = 2,
      stan_threads = TRUE
    ),
    6L
  )
  expect_equal(
    resolve_num_threads(
      threads = NULL,
      threads_per_chain = NULL,
      chains = 3,
      parallel_chains = NULL,
      stan_threads = TRUE
    ),
    3L
  )
  expect_equal(
    resolve_num_threads(
      threads = NULL,
      threads_per_chain = 2,
      chains = 2,
      parallel_chains = 8,
      stan_threads = TRUE
    ),
    4L
  )
  expect_error(
    resolve_num_threads(
      threads = 4,
      threads_per_chain = 2,
      chains = 4,
      parallel_chains = 4,
      stan_threads = TRUE
    ),
    "cannot both be supplied"
  )
  expect_warning(
    expect_equal(
      resolve_num_threads(
        threads = 4,
        threads_per_chain = NULL,
        chains = 4,
        parallel_chains = 4,
        stan_threads = FALSE
      ),
      1L
    ),
    "threads=4.*unthreaded executable.*no effect"
  )
})

test_that("normalize_chain_scalar_args scalarizes only threaded invocations", {
  expect_equal(
    normalize_chain_scalar_args(
      seed = 123,
      step_size = 0.5,
      chain_ids = 10:12,
      chains = 3,
      stan_threads = TRUE
    ),
    list(seed = 123L, step_size = 0.5, chain_ids = 10:12)
  )

  expect_warning(
    threaded <- normalize_chain_scalar_args(
      seed = c(123, 456, 789),
      step_size = c(0.1, 0.2, 0.3),
      chain_ids = c(10, 20, 30),
      chains = 3,
      stan_threads = TRUE
    ),
    "seed.*123.*remaining values.*ignored"
  )
  expect_equal(threaded$seed, 123L)
  expect_equal(threaded$step_size, 0.1)
  expect_equal(threaded$chain_ids, 10:12)

  expect_warning(
    normalize_chain_scalar_args(
      seed = 123,
      step_size = c(0.1, 0.2, 0.3),
      chain_ids = 1:3,
      chains = 3,
      stan_threads = TRUE
    ),
    "step_size.*0.1.*remaining values.*ignored"
  )
  expect_warning(
    normalize_chain_scalar_args(
      seed = 123,
      step_size = 0.1,
      chain_ids = c(10, 20, 30),
      chains = 3,
      stan_threads = TRUE
    ),
    "effective chain IDs are 10, 11, 12"
  )

  unthreaded <- normalize_chain_scalar_args(
    seed = c(123, 456, 789),
    step_size = c(0.1, 0.2, 0.3),
    chain_ids = c(10, 20, 30),
    chains = 3,
    stan_threads = FALSE
  )
  expect_equal(unthreaded$seed, c(123L, 456L, 789L))
  expect_equal(unthreaded$step_size, c(0.1, 0.2, 0.3))
  expect_equal(unthreaded$chain_ids, c(10L, 20L, 30L))
})

test_that("CmdStan file lists stage comma paths and finalize idempotently", {
  staging_dir <- file.path(tempdir(), paste0("cmdstan-stage-", Sys.getpid()))
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  input_path <- file.path(staging_dir, "input,one.json")
  writeLines("input", input_path)

  input_stage <- stage_cmdstan_file_list(
    input_path,
    direction = "input",
    staging_dir = staging_dir
  )
  expect_equal(input_stage$public_paths(), input_path)
  expect_false(any(grepl(",", basename(input_stage$cmdstan_paths()), fixed = TRUE)))
  expect_true(file.exists(input_stage$cmdstan_paths()))
  expect_equal(readLines(input_stage$cmdstan_paths()), "input")

  output_path <- file.path(staging_dir, "output,one.csv")
  output_stage <- stage_cmdstan_file_list(
    output_path,
    direction = "output",
    staging_dir = staging_dir
  )
  expect_false(file.exists(output_stage$cmdstan_paths()))
  writeLines("partial output", output_stage$cmdstan_paths())
  expect_true(output_stage$restore_outputs())
  expect_false(output_stage$restore_outputs())
  expect_equal(readLines(output_path), "partial output")
})

test_that("compose_cmdstan_file_list converts elements before joining", {
  local_mocked_bindings(
    wsl_safe_path = function(path, ...) paste0("/wsl/", basename(path))
  )
  expect_equal(
    compose_cmdstan_file_list(c("first output.csv", "second output.csv")),
    "/wsl/first output.csv,/wsl/second output.csv"
  )
  expect_error(
    compose_cmdstan_file_list(c("valid.csv", "has,comma.csv")),
    "contains a comma after path conversion"
  )
})

test_that("compose_invocation_args emits exact threaded sample file lists", {
  exe_file <- tempfile("cmdstan-model-")
  file.create(exe_file)
  Sys.chmod(exe_file, mode = "0755")
  args <- CmdStanArgs$new(
    model_name = "model",
    exe_file = exe_file,
    proc_ids = 10:11,
    method_args = SampleArgs$new(step_size = c(0.1, 0.2)),
    seed = 123,
    output_dir = tempdir()
  )
  invocation <- new_cmdstan_invocation_plan(
    invocation_id = 1,
    chain_indices = 1:2,
    chain_ids = 10:11,
    num_threads = 4,
    command = "model",
    env = c(STAN_NUM_THREADS = "4")
  )
  output_files <- file.path(tempdir(), paste0("output-", 10:11, ".csv"))
  diagnostic_files <- file.path(tempdir(), paste0("diagnostic-", 10:11, ".csv"))
  profile_file <- file.path(tempdir(), "profile-1.csv")

  command_args <- compose_invocation_args(
    cmdstan_args = args,
    invocation = invocation,
    chain_artifacts = list(
      output = output_files,
      diagnostic = diagnostic_files
    ),
    invocation_artifacts = list(profile = profile_file)
  )

  expect_true("id=10" %in% command_args)
  expect_true("num_threads=4" %in% command_args)
  expect_true("num_chains=2" %in% command_args)
  expect_true("stepsize=0.1" %in% command_args)
  expect_true(paste0("file=", paste(output_files, collapse = ",")) %in% command_args)
  expect_true(
    paste0("diagnostic_file=", paste(diagnostic_files, collapse = ",")) %in%
      command_args
  )
  expect_true(paste0("profile_file=", profile_file) %in% command_args)
})

test_that("compose_invocation_args emits exact threaded GQ inputs", {
  exe_file <- tempfile("cmdstan-model-")
  file.create(exe_file)
  Sys.chmod(exe_file, mode = "0755")
  fitted_params <- file.path(tempdir(), paste0("fitted-", 1:2, ".csv"))
  file.create(fitted_params)
  args <- CmdStanArgs$new(
    model_name = "model",
    exe_file = exe_file,
    proc_ids = 1:2,
    method_args = GenerateQuantitiesArgs$new(fitted_params = fitted_params),
    seed = 123,
    output_dir = tempdir()
  )
  invocation <- new_cmdstan_invocation_plan(
    invocation_id = 1,
    chain_indices = 1:2,
    chain_ids = 1:2,
    num_threads = 2,
    command = "model",
    env = c(STAN_NUM_THREADS = "2")
  )
  output_files <- file.path(tempdir(), paste0("gq-", 1:2, ".csv"))

  command_args <- compose_invocation_args(
    cmdstan_args = args,
    invocation = invocation,
    chain_artifacts = list(output = output_files),
    invocation_artifacts = list(),
    input_files = list(fitted_params = fitted_params)
  )

  expect_true("num_chains=2" %in% command_args)
  expect_true(
    paste0("fitted_params=", paste(fitted_params, collapse = ",")) %in%
      command_args
  )
})

test_that("build_cmdstan_run_plan derives threaded and unthreaded cardinality", {
  exe_file <- tempfile("cmdstan-model-")
  file.create(exe_file)
  Sys.chmod(exe_file, mode = "0755")
  threaded_args <- CmdStanArgs$new(
    model_name = "model",
    exe_file = exe_file,
    proc_ids = 10:13,
    method_args = SampleArgs$new(step_size = 0.1),
    seed = 123,
    output_dir = tempdir(),
    save_latent_dynamics = TRUE,
    save_cmdstan_config = TRUE
  )
  threaded <- build_cmdstan_run_plan(
    cmdstan_args = threaded_args,
    stan_threads = TRUE,
    num_threads = 8,
    requested_parallel_chains = 4,
    requested_threads_per_chain = 2
  )
  expect_equal(threaded$plan$num_chains(), 4L)
  expect_equal(threaded$plan$num_invocations(), 1L)
  expect_length(threaded$plan$chain_artifacts$output, 4)
  expect_length(threaded$plan$chain_artifacts$diagnostic, 4)
  expect_length(threaded$plan$invocation_artifacts$profile, 1)
  expect_length(threaded$plan$invocation_artifacts$config, 1)
  expect_equal(threaded$plan$invocations[[1]]$num_threads, 8L)
  expect_equal(threaded$plan$invocations[[1]]$env[["STAN_NUM_THREADS"]], "8")

  unthreaded_args <- CmdStanArgs$new(
    model_name = "model",
    exe_file = exe_file,
    proc_ids = c(9L, 2L, 20L, 7L),
    method_args = SampleArgs$new(step_size = 1:4),
    seed = 1:4,
    output_dir = tempdir()
  )
  unthreaded <- build_cmdstan_run_plan(
    cmdstan_args = unthreaded_args,
    stan_threads = FALSE,
    num_threads = 1,
    requested_parallel_chains = 2
  )
  expect_equal(unthreaded$plan$num_chains(), 4L)
  expect_equal(unthreaded$plan$num_invocations(), 4L)
  expect_equal(
    vapply(unthreaded$plan$invocations, `[[`, integer(1), "chain_ids"),
    c(9L, 2L, 20L, 7L)
  )
  expect_true(all(vapply(
    unthreaded$plan$invocations,
    function(invocation) length(invocation$env) == 0L,
    logical(1)
  )))
})

test_that("CmdStanRun exposes logical chains and physical invocations separately", {
  exe_file <- tempfile("cmdstan-model-")
  file.create(exe_file)
  Sys.chmod(exe_file, mode = "0755")
  args <- CmdStanArgs$new(
    model_name = "model",
    exe_file = exe_file,
    proc_ids = 10:13,
    method_args = SampleArgs$new(),
    seed = 123,
    output_dir = tempdir()
  )
  planned <- build_cmdstan_run_plan(
    cmdstan_args = args,
    stan_threads = TRUE,
    num_threads = 8,
    requested_parallel_chains = 4,
    requested_threads_per_chain = 2
  )
  procs <- CmdStanMCMCProcs$new(
    num_procs = planned$plan$num_invocations(),
    invocation_chain_ids = lapply(
      planned$plan$invocations,
      function(invocation) invocation$chain_ids
    ),
    parallel_procs = 1,
    show_stderr_messages = FALSE,
    show_stdout_messages = FALSE
  )
  runset <- CmdStanRun$new(
    args,
    procs,
    run_plan = planned$plan,
    file_stages = planned$stages
  )

  expect_equal(runset$num_chains(), 4L)
  expect_equal(runset$chain_ids(), 10:13)
  expect_equal(runset$num_procs(), 1L)
  expect_equal(runset$proc_ids(), 1L)
  expect_equal(runset$output_files(include_failed = TRUE), planned$plan$chain_artifacts$output)
  expect_equal(runset$profile_files(include_failed = TRUE), planned$plan$invocation_artifacts$profile)
  expect_equal(runset$command_args(), list(planned$plan$invocations[[1]]$command_args))
  expect_equal(runset$invocation_env(1), c(STAN_NUM_THREADS = "8"))
  procs$record_chain_output("shared invocation metadata")
  procs$record_chain_output("Chain [11] Iteration: 1 / 2")
  expect_equal(runset$chain_output(2), "Chain [11] Iteration: 1 / 2")
})

test_that("CmdStanProcs preserves PATH in the invocation environment", {
  captured_env <- NULL
  withr::local_envvar(c(
    PATH = "/cmdstanr-test-bin",
    STAN_NUM_THREADS = "17"
  ))
  local_mocked_bindings(
    toolchain_PATH_env_var = function() character(),
    tbb_path = function() character(),
    wsl_compatible_process_new = function(..., env = character()) {
      captured_env <<- env
      structure(list(), class = "fake_process")
    }
  )
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stderr_messages = FALSE,
    show_stdout_messages = FALSE
  )

  procs$new_proc(
    id = 1,
    command = "model",
    args = "sample",
    wd = tempdir(),
    env = c(STAN_NUM_THREADS = "6")
  )

  expect_equal(captured_env[["STAN_NUM_THREADS"]], "6")
  expect_equal(captured_env[["PATH"]], "/cmdstanr-test-bin")
  expect_equal(Sys.getenv("STAN_NUM_THREADS"), "17")
})

test_that("CmdStanProcs preserves an omitted thread count as NULL", {
  procs <- CmdStanProcs$new(num_procs = 1)

  expect_null(procs$threads_per_proc())
})

test_that("single logical-chain failures retain chain-specific warnings", {
  for (procs_class in list(CmdStanMCMCProcs, CmdStanGQProcs)) {
    procs <- procs_class$new(
      num_procs = 1,
      invocation_chain_ids = list(1L),
      show_stderr_messages = FALSE,
      show_stdout_messages = TRUE
    )
    procs$set_proc_state(1L, 7L)
    expect_warning(
      procs$report_time(1L),
      "Chain 1 finished unexpectedly!",
      fixed = TRUE
    )
  }
})

test_that("invocation cleanup terminates the complete launcher process tree", {
  killed_tree <- FALSE
  local_mocked_bindings(
    toolchain_PATH_env_var = function() character(),
    tbb_path = function() character(),
    wsl_compatible_process_new = function(...) {
      list(kill_tree = function() killed_tree <<- TRUE)
    }
  )
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stderr_messages = FALSE,
    show_stdout_messages = FALSE
  )
  procs$new_proc(
    id = 1,
    command = "model",
    args = "sample",
    wd = tempdir(),
    mpi_cmd = "mpiexec",
    mpi_args = list(n = 2, exe = "model")
  )

  procs$cleanup()

  expect_true(killed_tree)
})

test_that("fixed parameter, OpenCL, and MPI use the same topology plan", {
  exe_file <- tempfile("cmdstan-model-")
  file.create(exe_file)
  Sys.chmod(exe_file, mode = "0755")
  args <- CmdStanArgs$new(
    model_name = "model",
    exe_file = exe_file,
    proc_ids = 5:7,
    method_args = SampleArgs$new(fixed_param = TRUE),
    seed = 123,
    opencl_ids = c(0, 1),
    output_dir = tempdir()
  )
  threaded <- build_cmdstan_run_plan(
    cmdstan_args = args,
    stan_threads = TRUE,
    num_threads = 6,
    mpi_cmd = "mpiexec",
    mpi_args = list(n = 3)
  )

  expect_equal(threaded$plan$num_chains(), 3L)
  expect_equal(threaded$plan$num_invocations(), 1L)
  expect_true("algorithm=fixed_param" %in% threaded$plan$invocations[[1]]$command_args)
  expect_true("num_chains=3" %in% threaded$plan$invocations[[1]]$command_args)
  expect_true("platform=0" %in% threaded$plan$invocations[[1]]$command_args)
  expect_true("device=1" %in% threaded$plan$invocations[[1]]$command_args)
  expect_equal(threaded$plan$invocations[[1]]$mpi_cmd, "mpiexec")
  expect_equal(threaded$plan$invocations[[1]]$mpi_args, list(n = 3))

  unthreaded <- build_cmdstan_run_plan(
    cmdstan_args = args,
    stan_threads = FALSE,
    num_threads = 1,
    mpi_cmd = "mpiexec",
    mpi_args = list(n = 3)
  )
  expect_equal(unthreaded$plan$num_invocations(), 3L)
  expect_true(all(vapply(
    unthreaded$plan$invocations,
    function(invocation) identical(invocation$mpi_cmd, "mpiexec"),
    logical(1)
  )))
})

test_that("chain output filtering retains only attributed lines", {
  output <- c(
    "shared invocation metadata",
    "Chain [10] Iteration: 1 / 2",
    "Chain [11] Iteration: 1 / 2",
    "Chain [10] Iteration: 2 / 2",
    "unattributed stderr"
  )

  expect_equal(
    filter_cmdstan_chain_output(output, 10),
    output[c(2, 4)]
  )
  expect_equal(
    filter_cmdstan_chain_output(output, 11),
    output[3]
  )
})

test_that("chain output IDs and progress are tracked independently of invocations", {
  expect_equal(parse_chain_output_id("Chain [10] Iteration: 1 / 2"), 10L)
  expect_true(is.na(parse_chain_output_id("shared invocation metadata")))
  expect_true(is.na(parse_chain_output_id("Chain 10 Iteration: 1 / 2")))

  procs <- CmdStanMCMCProcs$new(
    num_procs = 1,
    invocation_chain_ids = list(10:11),
    show_stderr_messages = FALSE,
    show_stdout_messages = TRUE
  )
  procs$record_chain_output("shared invocation metadata")
  procs$record_chain_output("Chain [10] Iteration: 1 / 2 (Warmup)")
  procs$record_chain_output("Chain [11] Iteration: 1 / 2 (Sampling)")

  expect_equal(
    procs$chain_output(10),
    "Chain [10] Iteration: 1 / 2 (Warmup)"
  )
  expect_equal(
    procs$chain_output(11),
    "Chain [11] Iteration: 1 / 2 (Sampling)"
  )
  expect_equal(unname(procs$chain_progress()), c(3L, 4L))

  procs$mark_proc_start(1)
  procs$set_proc_state(1, 4)
  procs$mark_proc_stop(1)
  expect_equal(unname(procs$chain_progress()), c(7L, 7L))
  expect_warning(procs$report_time(1), "All chains finished unexpectedly")
})

test_that("WSL thread environment merges inherited entries without parent mutation", {
  local_mocked_bindings(os_is_wsl = function() TRUE)
  withr::local_envvar(WSLENV = "EXISTING/u:OTHER/p")
  before <- Sys.getenv(c("STAN_NUM_THREADS", "WSLENV"), unset = NA_character_)

  child_env <- cmdstan_thread_env(6)

  expect_equal(child_env[["STAN_NUM_THREADS"]], "6")
  expect_equal(child_env[["WSLENV"]], "EXISTING/u:OTHER/p:STAN_NUM_THREADS/u")
  expect_equal(
    Sys.getenv(c("STAN_NUM_THREADS", "WSLENV"), unset = NA_character_),
    before
  )
})
