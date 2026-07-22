# CmdStan run planning ----------------------------------------------------

#' Validate executable metadata required by chain run planning
#'
#' @noRd
cmdstan_chain_run_capability <- function(exe_info) {
  checkmate::assert_list(exe_info)
  if (!is.character(exe_info$stan_version) ||
      length(exe_info$stan_version) != 1L ||
      cmdstan_version_compare(exe_info$stan_version, "2.38.0") < 0) {
    stop(
      "Internal chain run planning requires CmdStan 2.38 or newer.",
      call. = FALSE
    )
  }
  if (!is.logical(exe_info$stan_threads) ||
      length(exe_info$stan_threads) != 1L ||
      is.na(exe_info$stan_threads)) {
    stop(
      "CmdStan executable metadata does not report 'stan_threads'.",
      call. = FALSE
    )
  }
  isTRUE(exe_info$stan_threads)
}

#' Parse an effective CmdStan chain ID from one output line
#'
#' @noRd
parse_chain_output_id <- function(line) {
  checkmate::assert_string(line)
  match <- regexec("^Chain \\[([0-9]+)\\](?: |$)", line, perl = TRUE)
  values <- regmatches(line, match)[[1]]
  if (length(values) != 2L) {
    return(NA_integer_)
  }
  as.integer(values[[2]])
}

#' Filter an invocation transcript for one effective CmdStan chain ID
#'
#' @noRd
filter_cmdstan_chain_output <- function(output, chain_id) {
  checkmate::assert_character(output, any.missing = FALSE)
  checkmate::assert_integerish(
    chain_id,
    lower = 1,
    len = 1,
    any.missing = FALSE
  )
  prefix <- paste0("^Chain \\[", as.integer(chain_id), "\\](?: |$)")
  output[grepl(prefix, output, perl = TRUE)]
}

#' Resolve total CmdStan threads for a chain-oriented run
#'
#' @noRd
resolve_num_threads <- function(threads,
                                threads_per_chain,
                                chains,
                                parallel_chains,
                                stan_threads) {
  if (!is.null(threads) && !is.null(threads_per_chain)) {
    stop("'threads' and 'threads_per_chain' cannot both be supplied.", call. = FALSE)
  }
  checkmate::assert_integerish(
    threads,
    lower = 1,
    len = 1,
    null.ok = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    threads_per_chain,
    lower = 1,
    len = 1,
    null.ok = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_integerish(chains, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(
    parallel_chains,
    lower = 1,
    len = 1,
    null.ok = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_flag(stan_threads)

  chains <- as.integer(chains)
  parallel_chains <- as.integer(parallel_chains %||% chains)
  if (!stan_threads) {
    if (!is.null(threads)) {
      warning(
        "'threads=", threads, "' was supplied for an unthreaded executable and has no effect.",
        call. = FALSE
      )
    } else if (!is.null(threads_per_chain)) {
      warning(
        "'threads_per_chain=", threads_per_chain,
        "' was supplied for an unthreaded executable and has no effect.",
        call. = FALSE
      )
    }
    return(1L)
  }

  if (!is.null(threads)) {
    return(as.integer(threads))
  }
  threads_per_chain <- as.integer(threads_per_chain %||% 1L)
  as.integer(min(chains, parallel_chains) * threads_per_chain)
}

#' Normalize chain-level scalar arguments at the topology boundary
#'
#' @noRd
normalize_chain_scalar_args <- function(seed,
                                        step_size,
                                        chain_ids,
                                        chains,
                                        stan_threads) {
  checkmate::assert_integerish(chains, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assert_flag(stan_threads)
  chains <- as.integer(chains)
  checkmate::assert_integerish(
    chain_ids,
    lower = 1,
    len = chains,
    unique = TRUE,
    any.missing = FALSE
  )
  chain_ids <- as.integer(chain_ids)
  validate_seed(seed, chains)
  if (!is.null(step_size)) {
    checkmate::assert_numeric(
      step_size,
      lower = .Machine$double.eps,
      len = if (length(step_size) == 1L) 1L else chains,
      any.missing = FALSE
    )
  }

  if (!stan_threads) {
    return(list(
      seed = if (is.null(seed)) NULL else as.integer(seed),
      step_size = step_size,
      chain_ids = chain_ids
    ))
  }

  if (is.null(seed)) {
    seed <- maybe_generate_seed(NULL, 1L)
  } else {
    seed <- as.integer(seed)
    if (length(seed) > 1L) {
      warning(
        "Threaded multi-chain execution uses seed=", seed[[1]],
        " for all chains; the remaining values were ignored.",
        call. = FALSE
      )
      seed <- seed[[1]]
    }
  }
  if (length(step_size) > 1L) {
    warning(
      "Threaded multi-chain execution uses step_size=", step_size[[1]],
      " for all chains; the remaining values were ignored.",
      call. = FALSE
    )
    step_size <- step_size[[1]]
  }
  effective_chain_ids <- seq.int(chain_ids[[1]], length.out = chains)
  if (!identical(chain_ids, effective_chain_ids)) {
    warning(
      "Threaded multi-chain execution requires consecutive chain IDs; ",
      "the effective chain IDs are ", paste(effective_chain_ids, collapse = ", "),
      ".",
      call. = FALSE
    )
    chain_ids <- effective_chain_ids
  }

  list(seed = as.integer(seed), step_size = step_size, chain_ids = chain_ids)
}

#' Compose arguments for one physical CmdStan invocation
#'
#' @noRd
compose_invocation_args <- function(cmdstan_args,
                                    invocation,
                                    chain_artifacts,
                                    invocation_artifacts,
                                    input_files = list()) {
  checkmate::assert_r6(cmdstan_args, classes = "CmdStanArgs")
  checkmate::assert_list(invocation)
  checkmate::assert_list(chain_artifacts)
  checkmate::assert_list(invocation_artifacts)
  checkmate::assert_list(input_files)
  chain_indices <- invocation$chain_indices
  invocation_id <- invocation$invocation_id

  select_chain_values <- function(values) {
    if (is.null(values) || length(values) == 0L) {
      return(NULL)
    }
    if (length(values) == 1L) {
      return(values)
    }
    values[chain_indices]
  }
  select_invocation_value <- function(values) {
    if (is.null(values) || length(values) == 0L) {
      return(NULL)
    }
    values[[invocation_id]]
  }

  args <- paste0("id=", invocation$base_chain_id)
  if ("STAN_NUM_THREADS" %in% names(invocation$env)) {
    args <- c(args, paste0("num_threads=", invocation$num_threads))
  }

  seed <- select_chain_values(cmdstan_args$seed)
  if (!is.null(seed)) {
    args <- c(args, "random", paste0("seed=", seed[[1]]))
  }

  init <- input_files$init
  if (is.null(init)) {
    init <- cmdstan_args$init
  }
  init <- select_chain_values(init)
  if (!is.null(init)) {
    if (is.character(init)) {
      init <- compose_cmdstan_file_list(init)
    } else {
      init <- init[[1]]
    }
    args <- c(args, paste0("init=", init))
  }

  if (!is.null(cmdstan_args$data_file)) {
    args <- c(
      args,
      "data",
      paste0("file=", wsl_safe_path(cmdstan_args$data_file))
    )
  }

  output_files <- select_chain_values(chain_artifacts$output)
  if (is.null(output_files)) {
    stop("Every CmdStan invocation requires planned output files.", call. = FALSE)
  }
  output_args <- c(
    "output",
    paste0("file=", compose_cmdstan_file_list(output_files))
  )
  diagnostic_files <- select_chain_values(chain_artifacts$diagnostic)
  if (!is.null(diagnostic_files)) {
    output_args <- c(
      output_args,
      paste0("diagnostic_file=", compose_cmdstan_file_list(diagnostic_files))
    )
  }
  if (!is.null(cmdstan_args$refresh)) {
    output_args <- c(output_args, paste0("refresh=", cmdstan_args$refresh))
  }
  if (!is.null(cmdstan_args$sig_figs)) {
    output_args <- c(output_args, paste0("sig_figs=", cmdstan_args$sig_figs))
  }
  profile_file <- select_invocation_value(invocation_artifacts$profile)
  if (!is.null(profile_file)) {
    output_args <- c(
      output_args,
      paste0("profile_file=", wsl_safe_path(profile_file))
    )
  }
  if (!is.null(cmdstan_args$save_cmdstan_config)) {
    output_args <- c(
      output_args,
      paste0("save_cmdstan_config=", cmdstan_args$save_cmdstan_config)
    )
  }
  args <- c(args, output_args)

  if (!is.null(cmdstan_args$opencl_ids)) {
    args <- c(
      args,
      "opencl",
      paste0("platform=", cmdstan_args$opencl_ids[[1]]),
      paste0("device=", cmdstan_args$opencl_ids[[2]])
    )
  }

  method_args <- cmdstan_args$method_args$compose(chain_indices[[1]])
  method_args <- append(
    method_args,
    paste0("num_chains=", invocation$num_chains),
    after = 1L
  )
  if (inherits(cmdstan_args$method_args, "SampleArgs")) {
    metric_files <- input_files$metric_file
    if (is.null(metric_files)) {
      metric_files <- cmdstan_args$method_args$metric_file
    }
    metric_files <- select_chain_values(metric_files)
    if (!is.null(metric_files)) {
      metric_arg <- paste0(
        "metric_file=",
        compose_cmdstan_file_list(metric_files)
      )
      metric_arg_index <- which(startsWith(method_args, "metric_file="))
      if (!length(metric_arg_index)) {
        algorithm_index <- which(method_args == "algorithm=hmc")
        method_args <- append(method_args, metric_arg, after = algorithm_index)
      } else {
        method_args[[metric_arg_index[[1]]]] <- metric_arg
        if (length(metric_arg_index) > 1L) {
          method_args <- method_args[-metric_arg_index[-1L]]
        }
      }
    }
  } else if (inherits(cmdstan_args$method_args, "GenerateQuantitiesArgs")) {
    fitted_params <- input_files$fitted_params
    if (is.null(fitted_params)) {
      fitted_params <- cmdstan_args$method_args$fitted_params
    }
    fitted_params <- select_chain_values(fitted_params)
    method_args <- method_args[!startsWith(method_args, "fitted_params=")]
    if (!is.null(fitted_params)) {
      method_args <- c(
        method_args,
        paste0("fitted_params=", compose_cmdstan_file_list(fitted_params))
      )
    }
  }
  c(args, method_args)
}

#' Construct the child environment for a threaded CmdStan invocation
#'
#' @noRd
cmdstan_thread_env <- function(num_threads) {
  checkmate::assert_integerish(
    num_threads,
    lower = 1,
    len = 1,
    any.missing = FALSE
  )
  env <- c(STAN_NUM_THREADS = as.character(as.integer(num_threads)))
  if (os_is_wsl()) {
    current <- Sys.getenv("WSLENV", unset = "")
    entries <- strsplit(current, ":", fixed = TRUE)[[1]]
    entries <- entries[nzchar(entries)]
    entries <- entries[!startsWith(entries, "STAN_NUM_THREADS")]
    env <- c(
      env,
      WSLENV = paste(c(entries, "STAN_NUM_THREADS/u"), collapse = ":")
    )
  }
  env
}

#' Build the logical-chain to physical-invocation plan
#'
#' @noRd
build_cmdstan_run_plan <- function(cmdstan_args,
                                   stan_threads,
                                   num_threads,
                                   requested_parallel_chains = NULL,
                                   requested_threads = NULL,
                                   requested_threads_per_chain = NULL,
                                   mpi_cmd = NULL,
                                   mpi_args = NULL) {
  checkmate::assert_r6(cmdstan_args, classes = "CmdStanArgs")
  checkmate::assert_flag(stan_threads)
  checkmate::assert_integerish(
    num_threads,
    lower = 1,
    len = 1,
    any.missing = FALSE
  )
  checkmate::assert_string(mpi_cmd, null.ok = TRUE)
  checkmate::assert_list(mpi_args, null.ok = TRUE)
  num_threads <- as.integer(num_threads)
  if (!stan_threads && num_threads != 1L) {
    stop("An unthreaded CmdStan executable must use one thread.", call. = FALSE)
  }

  chain_ids <- as.integer(cmdstan_args$proc_ids)
  num_chains <- length(chain_ids)
  chain_groups <- if (stan_threads) {
    list(seq_len(num_chains))
  } else {
    lapply(seq_len(num_chains), function(i) i)
  }
  num_invocations <- length(chain_groups)

  staging_dir <- cmdstan_tempdir()
  if (is.null(staging_dir) || !dir.exists(staging_dir)) {
    staging_dir <- tempdir(check = TRUE)
  }
  staging_dir <- absolute_path(staging_dir)
  if (grepl(",", staging_dir, fixed = TRUE)) {
    stop("The CmdStan staging directory must not contain a comma.", call. = FALSE)
  }

  output_public <- cmdstan_args$new_files(type = "output")
  output_stage <- stage_cmdstan_file_list(
    output_public,
    direction = "output",
    staging_dir = staging_dir
  )
  output_cmdstan <- output_stage$cmdstan_paths()

  diagnostic_public <- NULL
  diagnostic_cmdstan <- NULL
  diagnostic_stage <- NULL
  if (isTRUE(cmdstan_args$save_latent_dynamics)) {
    diagnostic_public <- cmdstan_args$new_files(type = "diagnostic")
    diagnostic_stage <- stage_cmdstan_file_list(
      diagnostic_public,
      direction = "output",
      staging_dir = staging_dir
    )
    diagnostic_cmdstan <- diagnostic_stage$cmdstan_paths()
  }

  profile_candidates <- cmdstan_args$new_files(type = "profile")
  first_chain_indices <- vapply(chain_groups, `[[`, integer(1), 1L)
  profile_public <- profile_candidates[first_chain_indices]
  profile_stage <- stage_cmdstan_file_list(
    profile_public,
    direction = "output",
    staging_dir = staging_dir
  )
  profile_cmdstan <- profile_stage$cmdstan_paths()

  metric_public <- NULL
  metric_stage <- NULL
  if (!is.null(cmdstan_args$method_args$save_metric) &&
      isTRUE(as.logical(cmdstan_args$method_args$save_metric))) {
    metric_public <- paste0(
      tools::file_path_sans_ext(output_public),
      "_metric.json"
    )
    metric_cmdstan <- paste0(
      tools::file_path_sans_ext(output_cmdstan),
      "_metric.json"
    )
    metric_stage <- new_cmdstan_file_stage_mapping(
      metric_public,
      metric_cmdstan,
      direction = "output",
      staging_dir = staging_dir
    )
  }

  config_public <- NULL
  config_stage <- NULL
  if (!is.null(cmdstan_args$save_cmdstan_config) &&
      isTRUE(as.logical(cmdstan_args$save_cmdstan_config))) {
    config_public <- paste0(
      tools::file_path_sans_ext(output_public[first_chain_indices]),
      "_config.json"
    )
    config_cmdstan <- paste0(
      tools::file_path_sans_ext(output_cmdstan[first_chain_indices]),
      "_config.json"
    )
    config_stage <- new_cmdstan_file_stage_mapping(
      config_public,
      config_cmdstan,
      direction = "output",
      staging_dir = staging_dir
    )
  }

  input_files <- list()
  input_stages <- list()
  add_input_stage <- function(name, paths) {
    if (is.null(paths)) {
      return(NULL)
    }
    stage <- stage_cmdstan_file_list(
      paths,
      direction = "input",
      staging_dir = staging_dir
    )
    input_stages[[name]] <<- stage
    input_files[[name]] <<- stage$cmdstan_paths()
    invisible(NULL)
  }
  if (is.character(cmdstan_args$init)) {
    add_input_stage("init", cmdstan_args$init)
  }
  if (inherits(cmdstan_args$method_args, "SampleArgs")) {
    add_input_stage("metric_file", cmdstan_args$method_args$metric_file)
  } else if (inherits(cmdstan_args$method_args, "GenerateQuantitiesArgs")) {
    add_input_stage(
      "fitted_params",
      cmdstan_args$method_args$fitted_params
    )
  }

  child_env <- if (stan_threads) {
    cmdstan_thread_env(num_threads)
  } else {
    character()
  }
  chain_artifacts_cmdstan <- list(
    output = output_cmdstan,
    diagnostic = diagnostic_cmdstan
  )
  invocation_artifacts_cmdstan <- list(profile = profile_cmdstan)
  invocations <- lapply(seq_along(chain_groups), function(invocation_id) {
    chain_indices <- chain_groups[[invocation_id]]
    invocation <- list(
      invocation_id = as.integer(invocation_id),
      chain_indices = as.integer(chain_indices),
      chain_ids = chain_ids[chain_indices],
      base_chain_id = chain_ids[chain_indices][[1]],
      num_chains = as.integer(length(chain_indices)),
      num_threads = if (stan_threads) num_threads else 1L,
      command = cmdstan_args$command(),
      command_args = character(),
      env = child_env,
      mpi_cmd = mpi_cmd,
      mpi_args = mpi_args
    )
    command_args <- compose_invocation_args(
      cmdstan_args = cmdstan_args,
      invocation = invocation,
      chain_artifacts = chain_artifacts_cmdstan,
      invocation_artifacts = invocation_artifacts_cmdstan,
      input_files = input_files
    )
    new_cmdstan_invocation_plan(
      invocation_id = invocation_id,
      chain_indices = chain_indices,
      chain_ids = chain_ids[chain_indices],
      num_threads = invocation$num_threads,
      command = invocation$command,
      command_args = command_args,
      env = child_env,
      mpi_cmd = mpi_cmd,
      mpi_args = mpi_args
    )
  })

  plan <- CmdStanRunPlan$new(
    method = cmdstan_args$method,
    chain_ids = chain_ids,
    invocations = invocations,
    chain_artifacts = list(
      output = output_public,
      diagnostic = diagnostic_public,
      metric = metric_public
    ),
    invocation_artifacts = list(
      profile = profile_public,
      config = config_public
    ),
    requested_parallel_chains = requested_parallel_chains,
    requested_threads = requested_threads,
    requested_threads_per_chain = requested_threads_per_chain
  )
  stages <- c(
    list(
      output = output_stage,
      diagnostic = diagnostic_stage,
      profile = profile_stage,
      metric = metric_stage,
      config = config_stage
    ),
    input_stages
  )
  stages <- stages[!vapply(stages, is.null, logical(1))]
  list(plan = plan, stages = stages)
}

#' Validate one physical CmdStan invocation
#'
#' @noRd
new_cmdstan_invocation_plan <- function(invocation_id,
                                        chain_indices,
                                        chain_ids,
                                        num_threads,
                                        command,
                                        command_args = character(),
                                        env = character(),
                                        mpi_cmd = NULL,
                                        mpi_args = NULL) {
  checkmate::assert_integerish(
    invocation_id,
    lower = 1,
    len = 1,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    chain_indices,
    lower = 1,
    min.len = 1,
    unique = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    chain_ids,
    lower = 1,
    len = length(chain_indices),
    unique = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    num_threads,
    lower = 1,
    len = 1,
    any.missing = FALSE
  )
  checkmate::assert_string(command)
  checkmate::assert_character(command_args, any.missing = FALSE)
  checkmate::assert_character(env, any.missing = FALSE)
  checkmate::assert_string(mpi_cmd, null.ok = TRUE)
  checkmate::assert_list(mpi_args, null.ok = TRUE)

  invocation_id <- as.integer(invocation_id)
  chain_indices <- as.integer(chain_indices)
  chain_ids <- as.integer(chain_ids)
  num_threads <- as.integer(num_threads)
  num_chains <- length(chain_indices)

  if (num_chains > 1L && !identical(chain_ids, seq.int(chain_ids[[1]], length.out = num_chains))) {
    stop("Chain IDs in a multi-chain invocation must be consecutive.", call. = FALSE)
  }
  list(
    invocation_id = invocation_id,
    chain_indices = chain_indices,
    chain_ids = chain_ids,
    base_chain_id = chain_ids[[1]],
    num_chains = as.integer(num_chains),
    num_threads = num_threads,
    command = command,
    command_args = command_args,
    env = env,
    mpi_cmd = mpi_cmd,
    mpi_args = mpi_args
  )
}

#' Logical-chain to physical-invocation mapping
#'
#' @noRd
CmdStanRunPlan <- R6::R6Class(
  classname = "CmdStanRunPlan",
  public = list(
    method = NULL,
    chain_ids = NULL,
    invocations = NULL,
    chain_artifacts = NULL,
    invocation_artifacts = NULL,
    requested_parallel_chains = NULL,
    requested_threads = NULL,
    requested_threads_per_chain = NULL,

    initialize = function(method,
                          chain_ids,
                          invocations,
                          chain_artifacts = list(),
                          invocation_artifacts = list(),
                          requested_parallel_chains = NULL,
                          requested_threads = NULL,
                          requested_threads_per_chain = NULL) {
      checkmate::assert_string(method)
      checkmate::assert_integerish(
        chain_ids,
        lower = 1,
        min.len = 1,
        unique = TRUE,
        any.missing = FALSE
      )
      checkmate::assert_list(invocations, min.len = 1)
      checkmate::assert_list(chain_artifacts, names = "unique")
      checkmate::assert_list(invocation_artifacts, names = "unique")
      checkmate::assert_integerish(
        requested_parallel_chains,
        lower = 1,
        len = 1,
        null.ok = TRUE,
        any.missing = FALSE
      )
      checkmate::assert_integerish(
        requested_threads,
        lower = 1,
        len = 1,
        null.ok = TRUE,
        any.missing = FALSE
      )
      checkmate::assert_integerish(
        requested_threads_per_chain,
        lower = 1,
        len = 1,
        null.ok = TRUE,
        any.missing = FALSE
      )

      chain_ids <- as.integer(chain_ids)
      num_chains <- length(chain_ids)
      num_invocations <- length(invocations)
      invocation_ids <- vapply(
        invocations,
        function(invocation) invocation$invocation_id,
        integer(1)
      )
      if (!identical(invocation_ids, seq_len(num_invocations))) {
        stop("Invocation IDs must be sequential from 1.", call. = FALSE)
      }

      mapped_indices <- unlist(
        lapply(invocations, function(invocation) invocation$chain_indices),
        use.names = FALSE
      )
      if (!identical(sort(mapped_indices), seq_len(num_chains))) {
        stop("Every logical chain index must occur exactly once across invocations.", call. = FALSE)
      }
      for (invocation in invocations) {
        expected_chain_ids <- chain_ids[invocation$chain_indices]
        if (!identical(invocation$chain_ids, expected_chain_ids)) {
          stop("Invocation chain IDs must match the run plan's logical chain registry.", call. = FALSE)
        }
        if (invocation$num_chains != length(invocation$chain_indices) ||
            invocation$num_chains != length(invocation$chain_ids)) {
          stop("Invocation chain cardinality is inconsistent.", call. = FALSE)
        }
      }

      validate_artifacts <- function(artifacts, expected_length, scope) {
        for (artifact_name in names(artifacts)) {
          artifact <- artifacts[[artifact_name]]
          if (!is.null(artifact)) {
            checkmate::assert_character(artifact, any.missing = FALSE)
            if (length(artifact) != expected_length) {
              stop(
                scope,
                " artifact '", artifact_name, "' must have length ",
                expected_length, ".",
                call. = FALSE
              )
            }
          }
        }
      }
      validate_artifacts(chain_artifacts, num_chains, "chain")
      validate_artifacts(invocation_artifacts, num_invocations, "invocation")

      self$method <- method
      self$chain_ids <- chain_ids
      self$invocations <- invocations
      self$chain_artifacts <- chain_artifacts
      self$invocation_artifacts <- invocation_artifacts
      self$requested_parallel_chains <- if (is.null(requested_parallel_chains)) {
        NULL
      } else {
        as.integer(requested_parallel_chains)
      }
      self$requested_threads <- if (is.null(requested_threads)) {
        NULL
      } else {
        as.integer(requested_threads)
      }
      self$requested_threads_per_chain <- if (is.null(requested_threads_per_chain)) {
        NULL
      } else {
        as.integer(requested_threads_per_chain)
      }
      invisible(self)
    },

    num_chains = function() {
      length(self$chain_ids)
    },

    num_invocations = function() {
      length(self$invocations)
    },

    invocation_ids = function() {
      vapply(
        self$invocations,
        function(invocation) invocation$invocation_id,
        integer(1)
      )
    }
  )
)
