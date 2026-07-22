# CmdStanRun --------------------------------------------------------------

#' Run CmdStan using a specified configuration
#'
#' The internal `CmdStanRun` R6 class handles preparing the call to CmdStan
#' (using the `CmdStanArgs` object), setting up the external processes (using
#' the `CmdStanProcs` object), and provides methods for running CmdStan's
#' multiple methods/algorithms, running CmdStan utilities (e.g. `stansummary`),
#' and saving the output files.
#'
#' @noRd
#' @param args A `CmdStanArgs` object.
#' @param procs A `CmdStanProcs` object.
#' @param run_plan An optional `CmdStanRunPlan` object.
#' @param file_stages File staging records owned by the run.
#'
CmdStanRun <- R6::R6Class(
  classname = "CmdStanRun",
  public = list(
    args = NULL,
    procs = NULL,
    plan = NULL,
    initialize = function(args, procs, run_plan = NULL, file_stages = list()) {
      checkmate::assert_r6(args, classes = "CmdStanArgs")
      checkmate::assert_r6(procs, classes = "CmdStanProcs")
      checkmate::assert_r6(
        run_plan,
        classes = "CmdStanRunPlan",
        null.ok = TRUE
      )
      checkmate::assert_list(file_stages)
      self$args <- args
      self$procs <- procs
      self$plan <- run_plan
      private$file_stages_ <- file_stages
      if (is.null(self$plan)) {
        private$output_files_ <- self$new_output_files()
        private$profile_files_ <- self$new_profile_files()
        if (!is.null(self$args$save_cmdstan_config) &&
            as.logical(self$args$save_cmdstan_config)) {
          private$config_files_ <- self$new_config_files()
        }
        if (!is.null(self$args$method_args$save_metric) &&
            as.logical(self$args$method_args$save_metric)) {
          private$metric_files_ <- self$new_metric_files()
        }
        if (self$args$save_latent_dynamics) {
          private$latent_dynamics_files_ <- self$new_latent_dynamics_files()
        }
      } else {
        if (self$plan$num_invocations() != self$procs$num_procs()) {
          stop(
            "The run plan and process registry have different invocation counts.",
            call. = FALSE
          )
        }
        planned_chain_ids <- lapply(
          self$plan$invocations,
          function(invocation) invocation$chain_ids
        )
        if (!identical(self$procs$invocation_chain_ids(), planned_chain_ids)) {
          stop(
            "The run plan and process registry have different logical-chain mappings.",
            call. = FALSE
          )
        }
        private$output_files_ <- self$plan$chain_artifacts$output
        private$latent_dynamics_files_ <- self$plan$chain_artifacts$diagnostic
        private$metric_files_ <- self$plan$chain_artifacts$metric
        private$profile_files_ <- self$plan$invocation_artifacts$profile
        private$config_files_ <- self$plan$invocation_artifacts$config
      }
      if (os_is_wsl()) {
        # While the executable built under WSL will be stored in the Windows
        # filesystem alongside the model code, we place a copy in a WSL temp
        # directory prior to execution to avoid IO perfomance impacts
        wsl_tmpdir <- wsl_tempdir()
        file.copy(from = args$exe_file,
                  to = file.path(wsl_dir_prefix(), wsl_tmpdir))
        args$exe_file <- file.path(wsl_tmpdir, basename(args$exe_file))
        processx::run("wsl", args = c("chmod", "+x", args$exe_file),
                      error_on_status = FALSE)
      }
      invisible(self)
    },
    num_procs = function() self$procs$num_procs(),
    proc_ids = function() self$procs$proc_ids(),
    num_chains = function() {
      if (is.null(self$plan)) self$num_procs() else self$plan$num_chains()
    },
    chain_ids = function() {
      if (is.null(self$plan)) self$args$proc_ids else self$plan$chain_ids
    },
    invocation_env = function(id) {
      if (is.null(self$plan)) {
        return(character())
      }
      self$plan$invocations[[id]]$env
    },
    chain_output = function(id) {
      checkmate::assert_integerish(
        id,
        lower = 1,
        upper = self$num_chains(),
        len = 1,
        any.missing = FALSE
      )
      id <- as.integer(id)
      if (is.null(self$plan)) {
        return(self$procs$proc_output(id))
      }
      effective_id <- self$plan$chain_ids[[id]]
      self$procs$chain_output(effective_id)
    },
    add_live_run_metadata = function(metadata) {
      if (is.null(self$plan)) {
        return(metadata)
      }
      if (length(metadata$id) == self$plan$num_chains()) {
        metadata$id <- self$plan$chain_ids
      }
      if (is.data.frame(metadata$time) &&
          nrow(metadata$time) == self$plan$num_chains()) {
        metadata$time$chain_id <- self$plan$chain_ids
      }
      if (is.character(self$args$init)) {
        metadata$init <- self$args$init
      }
      metadata$parallel_chains <- self$plan$requested_parallel_chains
      metadata$threads_per_chain <- if (is.null(self$plan$requested_threads)) {
        self$plan$requested_threads_per_chain %||% 1L
      } else {
        NULL
      }
      metadata
    },
    exe_file = function() self$args$exe_file,
    stan_code = function() self$args$stan_code,
    model_methods_env = function() self$args$model_methods_env,
    standalone_env = function() self$args$standalone_env,
    model_name = function() self$args$model_name,
    method = function() self$args$method,
    data_file = function() self$args$data_file,
    new_output_files = function() {
      self$args$new_files(type = "output")
    },
    new_latent_dynamics_files = function() {
      self$args$new_files(type = "diagnostic")
    },
    new_profile_files = function() {
      self$args$new_files(type = "profile")
    },
    new_config_files = function() {
      # the output_file name is used as the base for the config file
      paste0(tools::file_path_sans_ext(private$output_files_), "_config.json")
    },
    new_metric_files = function() {
      # the output_file name is used as the base for the metric file
      paste0(tools::file_path_sans_ext(private$output_files_), "_metric.json")
    },
    config_files = function(include_failed = FALSE) {
      files <- private$config_files_
      if (include_failed) {
        files
      } else {
        ok <- private$successful_invocations_()
        files[ok]
      }
    },
    metric_files = function(include_failed = FALSE) {
      files <- private$metric_files_
      files_win_path <- sapply(private$metric_files_, wsl_safe_path, revert = TRUE)
      if (include_failed) {
        files
      } else {
        ok <- private$successful_chains_()
        files[ok]
      }
    },
    latent_dynamics_files = function(include_failed = FALSE) {
      if (!length(private$latent_dynamics_files_)) {
        stop(
          "No latent dynamics files found. ",
          "Set 'save_latent_dynamics=TRUE' when fitting the model.",
          call. = FALSE
        )
      }
      if (include_failed) {
        private$latent_dynamics_files_
      } else {
        ok <- private$successful_chains_()
        private$latent_dynamics_files_[ok]
      }
    },
    output_files = function(include_failed = FALSE) {
      if (include_failed) {
        private$output_files_
      } else {
        ok <- private$successful_chains_()
        private$output_files_[ok]
      }
    },
    profile_files = function(include_failed = FALSE) {
      files <- private$profile_files_
      if (include_failed) {
        return(files)
      }
      if (!length(files) || !any(file.exists(files))) {
        stop(
          "No profile files found. ",
          "The model that produced the fit did not use any profiling.",
          call. = FALSE
        )
      }
      files[private$successful_invocations_()]
    },
    save_output_files = function(dir = ".",
                                 basename = NULL,
                                 timestamp = TRUE,
                                 random = TRUE) {
      current_files <- self$output_files(include_failed = TRUE)
      new_paths <- copy_temp_files(
        current_paths = current_files,
        new_dir = dir,
        new_basename = basename %||% self$model_name(),
        ids = self$chain_ids(),
        ext = ".csv",
        timestamp = timestamp,
        random = random
      )
      file.remove(current_files[!current_files %in% new_paths])
      private$output_files_ <- new_paths
      message(
        "Moved ",
        length(current_files),
        " files and set internal paths to new locations:\n",
        paste("-", new_paths, collapse = "\n")
      )
      private$output_files_saved_ <- TRUE
      invisible(new_paths)
    },
    save_latent_dynamics_files = function(dir = ".",
                                          basename = NULL,
                                          timestamp = TRUE,
                                          random = TRUE) {
      current_files <- self$latent_dynamics_files(include_failed = TRUE) # used so we get error if 0 files
      new_paths <- copy_temp_files(
        current_paths = current_files,
        new_dir = dir,
        new_basename = paste0(basename %||% self$model_name(), "-diagnostic"),
        ids = self$chain_ids(),
        ext = ".csv",
        timestamp = timestamp,
        random = random
      )
      file.remove(current_files[!current_files %in% new_paths])
      private$latent_dynamics_files_ <- new_paths
      message(
        "Moved ",
        length(current_files),
        " files and set internal paths to new locations:\n",
        paste("-", new_paths, collapse = "\n")
      )
      private$latent_dynamics_files_saved_ <- TRUE
      invisible(new_paths)
    },
    save_profile_files = function(dir = ".",
                                  basename = NULL,
                                  timestamp = TRUE,
                                  random = TRUE) {
      current_files <- self$profile_files()
      new_paths <- copy_temp_files(
        current_paths = current_files,
        new_dir = dir,
        new_basename = paste0(basename %||% self$model_name(), "-profile"),
        ids = self$proc_ids(),
        ext = ".csv",
        timestamp = timestamp,
        random = random
      )
      file.remove(current_files[!current_files %in% new_paths])
      private$profile_files_ <- new_paths
      message(
        "Moved ",
        length(current_files),
        " files and set internal paths to new locations:\n",
        paste("-", new_paths, collapse = "\n")
      )
      private$profile_files_saved_ <- TRUE
      invisible(new_paths)
    },
    save_data_file = function(dir = ".",
                              basename = NULL,
                              timestamp = TRUE,
                              random = TRUE) {
      new_path <- copy_temp_files(
        current_paths = self$data_file(),
        new_dir = dir,
        new_basename = basename %||% self$model_name(),
        ids = NULL,
        ext = tools::file_ext(self$data_file()),
        timestamp = timestamp,
        random = random
      )
      if (new_path != self$data_file()) {
        file.remove(self$data_file())
      }
      self$args$data_file <- new_path
      message("Moved data file and set internal path to new location:\n",
              "- ", new_path)
      invisible(new_path)
    },
    save_config_files = function(dir = ".",
                                    basename = NULL,
                                    timestamp = TRUE,
                                    random = TRUE) {
      current_files <- self$config_files(include_failed = TRUE) # used so we get error if 0 files
      new_paths <- copy_temp_files(
        current_paths = current_files,
        new_dir = dir,
        new_basename = paste0(basename %||% self$model_name(), "-config"),
        ids = self$proc_ids(),
        ext = ".json",
        timestamp = timestamp,
        random = random
      )
      file.remove(current_files[!current_files %in% new_paths])
      private$config_files_ <- new_paths
      message(
        "Moved ",
        length(current_files),
        " files and set internal paths to new locations:\n",
        paste("-", new_paths, collapse = "\n")
      )
      private$config_files_saved_ <- TRUE
      invisible(new_paths)
    },
    save_metric_files = function(dir = ".",
                                 basename = NULL,
                                 timestamp = TRUE,
                                 random = TRUE) {
      current_files <- self$metric_files(include_failed = TRUE) # used so we get error if 0 files
      new_paths <- copy_temp_files(
        current_paths = current_files,
        new_dir = dir,
        new_basename = paste0(basename %||% self$model_name(), "-metric"),
        ids = self$chain_ids(),
        ext = ".json",
        timestamp = timestamp,
        random = random
      )
      file.remove(current_files[!current_files %in% new_paths])
      private$metric_files_ <- new_paths
      message(
        "Moved ",
        length(current_files),
        " files and set internal paths to new locations:\n",
        paste("-", new_paths, collapse = "\n")
      )
      private$metric_files_saved_ <- TRUE
      invisible(new_paths)
    },

    command = function() self$args$command(),
    command_args = function() {
      if (!is.null(self$plan)) {
        return(lapply(self$plan$invocations, `[[`, "command_args"))
      }
      if (!length(private$command_args_)) {
        # create a list of character vectors (one per run/chain) of cmdstan arguments
        private$command_args_ <- lapply(self$procs$proc_ids(), function(j) {
          self$args$compose_all_args(
            idx = j,
            output_file = private$output_files_[j],
            profile_file = private$profile_files_[j],
            latent_dynamics_file = private$latent_dynamics_files_[j] # maybe NULL
          )
        })
      }
      private$command_args_
    },

    run_cmdstan = function() {
      on.exit(private$restore_file_stages_(), add = TRUE)
      run_method <- paste0("run_", self$method(), "_")
      private[[run_method]]()
      private$restore_file_stages_()
      private$validate_invocation_outputs_()
      if (self$method() %in% c("sample", "generate_quantities")) {
        if (self$num_chains() == 1L) {
          self$procs$report_time(1L)
        } else {
          self$procs$report_time()
        }
      }
    },

    run_cmdstan_mpi = function(mpi_cmd, mpi_args) {
      on.exit(private$restore_file_stages_(), add = TRUE)
      private$run_sample_(mpi_cmd, mpi_args)
      private$restore_file_stages_()
      private$validate_invocation_outputs_()
      if (self$num_chains() == 1L) {
        self$procs$report_time(1L)
      } else {
        self$procs$report_time()
      }
    },

    #' Run `bin/stansummary` or `bin/diagnose`
    #' @param tool The name of the tool in `bin/` to run.
    #' @param flags An optional character vector of flags (e.g. `c("--sig_figs=1")`).
    #' @noRd
    run_cmdstan_tool = function(tool = c("stansummary", "diagnose"), flags = NULL) {
      if (self$method() == "optimize") {
        stop("Not available for optimize method.", call. = FALSE)
      }
      if (self$method() == "laplace") {
        stop("Not available for laplace method.", call. = FALSE)
      }
      if (self$method() == "generate_quantities") {
        stop("Not available for generate_quantities method.", call. = FALSE)
      }
      tool <- match.arg(tool)
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("No CmdStan runs finished successfully. ",
             "Unable to run bin/", tool, ".", call. = FALSE)
      }
      target_exe <- file.path("bin", cmdstan_ext(tool))
      check_target_exe(target_exe)
      withr::with_path(
        c(
          toolchain_PATH_env_var(),
          tbb_path()
        ),
        run_log <- wsl_compatible_run(
          command = target_exe,
          args = c(
            sapply(self$output_files(include_failed = FALSE),
                   wsl_safe_path),
            flags),
          wd = cmdstan_path(),
          echo = TRUE,
          echo_cmd = is_verbose_mode(),
          error_on_status = TRUE
        )
      )
    },

    time = function() {
      if (self$method() %in% c("laplace", "optimize", "variational", "pathfinder")) {
        time <- list(total = self$procs$total_time())
      } else if (!is.null(self$plan) && self$method() == "sample") {
        metadata <- lapply(
          self$output_files(include_failed = FALSE),
          read_csv_metadata
        )
        chain_time <- if (length(metadata)) {
          do.call(rbind, lapply(metadata, `[[`, "time"))
        } else {
          data.frame(
            chain_id = integer(),
            warmup = numeric(),
            sampling = numeric(),
            total = numeric()
          )
        }
        if (nrow(chain_time) == self$plan$num_chains()) {
          chain_time$chain_id <- self$plan$chain_ids
        }
        time <- list(total = self$procs$total_time(), chains = chain_time)
      } else if (!is.null(self$plan) &&
                 self$method() == "generate_quantities") {
        metadata <- lapply(
          self$output_files(include_failed = FALSE),
          read_csv_metadata
        )
        chain_time <- do.call(
          rbind,
          lapply(metadata, function(x) x$time %||% NULL)
        )
        if (is.null(chain_time)) {
          chain_time <- data.frame(chain_id = integer(), total = numeric())
        }
        if (nrow(chain_time) == self$plan$num_chains()) {
          chain_time$chain_id <- self$plan$chain_ids
        }
        time <- list(total = self$procs$total_time(), chains = chain_time)
      } else {
        chain_ids <- names(self$procs$is_finished())
        chain_time <- data.frame(
          chain_id = as.vector(self$procs$proc_ids()),
          warmup = as.vector(self$procs$proc_section_time("warmup")),
          sampling = as.vector(self$procs$proc_section_time("sampling")),
          total = as.vector(self$procs$proc_total_time()[chain_ids])
        )
        time <- list(total = self$procs$total_time(), chains = chain_time)
      }
      time
    }
  ),
  private = list(
    output_files_ = character(),
    profile_files_ = NULL,
    output_files_saved_ = FALSE,
    latent_dynamics_files_ = NULL,
    latent_dynamics_files_saved_ = FALSE,
    profile_files_saved_ = FALSE,
    config_files_ = NULL,
    metric_files_ = NULL,
    config_files_saved_ = FALSE,
    metric_files_saved_ = FALSE,
    command_args_ = list(),
    file_stages_ = list(),

    successful_invocations_ = function() {
      self$procs$is_finished() | self$procs$is_queued()
    },

    successful_chains_ = function() {
      if (is.null(self$plan)) {
        return(private$successful_invocations_())
      }
      ok <- rep(FALSE, self$plan$num_chains())
      invocation_ok <- private$successful_invocations_()
      for (invocation in self$plan$invocations) {
        ok[invocation$chain_indices] <- invocation_ok[[invocation$invocation_id]]
      }
      ok
    },

    restore_file_stages_ = function() {
      for (stage in private$file_stages_) {
        stage$restore_outputs()
      }
      for (stage in private$file_stages_) {
        stage$cleanup()
      }
      invisible(NULL)
    },

    validate_invocation_outputs_ = function() {
      if (is.null(self$plan) ||
          !self$method() %in% c("sample", "generate_quantities")) {
        return(invisible(NULL))
      }
      for (invocation in self$plan$invocations) {
        invocation_id <- invocation$invocation_id
        if (!self$procs$is_finished(invocation_id)) {
          next
        }
        files <- private$output_files_[invocation$chain_indices]
        valid <- all(file.exists(files)) && all(vapply(
          files,
          function(file) {
            !inherits(try(read_csv_metadata(file), silent = TRUE), "try-error")
          },
          logical(1)
        ))
        if (!valid) {
          self$procs$set_proc_state(invocation_id, 7)
        }
      }
      invisible(NULL)
    },

    finalize = function() {
      private$restore_file_stages_()
      if (self$args$using_tempdir) {
        temp_files <- c(
          if (!private$output_files_saved_)
            self$output_files(include_failed = TRUE),
          if (self$args$save_latent_dynamics && !private$latent_dynamics_files_saved_)
            self$latent_dynamics_files(include_failed = TRUE),
          if (!private$profile_files_saved_)
            private$profile_files_,
          if (!is.null(self$args$save_cmdstan_config) &&
              as.logical(self$args$save_cmdstan_config) &&
              !private$config_files_saved_)
            self$config_files(include_failed = TRUE),
          if (!(is.null(self$args$method_args$save_metric)) &&
              as.logical(self$args$method_args$save_metric) &&
              !private$metric_files_saved_)
            self$metric_files(include_failed = TRUE)
        )
        unlink(temp_files)
      }
    }
  )
)


# run helpers -------------------------------------------------
check_target_exe <- function(exe) {
  exe_path <- file.path(cmdstan_path(), exe)
  if (!file.exists(exe_path)) {
    withr::with_envvar(
      c("HOME" = short_path(Sys.getenv("HOME"))),
      withr::with_path(
        c(
          toolchain_PATH_env_var(),
          tbb_path()
        ),
        run_log <- wsl_compatible_run(
          command = make_cmd(),
          args = exe,
          wd = cmdstan_path(),
          echo_cmd = TRUE,
          echo = TRUE,
          error_on_status = TRUE
        )
      )
    )
  }
}

.run_invocations <- function(mpi_cmd = NULL, mpi_args = NULL) {
  procs <- self$procs
  on.exit(procs$cleanup(), add = TRUE)
  start_time <- Sys.time()
  invocation_ids <- procs$proc_ids()
  invocation_index <- 1L
  while (!all(procs$is_finished() | procs$is_failed())) {
    while (procs$active_procs() != procs$parallel_procs() && procs$any_queued()) {
      invocation_id <- invocation_ids[[invocation_index]]
      if (is.null(self$plan)) {
        command <- self$command()
        command_args <- self$command_args()[[invocation_id]]
        env <- if (is.null(procs$threads_per_proc())) {
          NULL
        } else {
          cmdstan_thread_env(procs$threads_per_proc())
        }
        invocation_mpi_cmd <- mpi_cmd
        invocation_mpi_args <- mpi_args
      } else {
        invocation <- self$plan$invocations[[invocation_id]]
        command <- invocation$command
        command_args <- invocation$command_args
        env <- if (length(invocation$env)) invocation$env else NULL
        invocation_mpi_cmd <- invocation$mpi_cmd %||% mpi_cmd
        invocation_mpi_args <- invocation$mpi_args %||% mpi_args
      }
      if (!is.null(invocation_mpi_cmd)) {
        invocation_mpi_args <- invocation_mpi_args %||% list()
        invocation_mpi_args[["exe"]] <- wsl_safe_path(self$exe_file())
      }
      procs$new_proc(
        id = invocation_id,
        command = command,
        args = command_args,
        wd = dirname(self$exe_file()),
        env = env,
        mpi_cmd = invocation_mpi_cmd,
        mpi_args = invocation_mpi_args
      )
      if (is_verbose_mode() && length(env)) {
        message(
          "CmdStan child environment: ",
          paste(paste0(names(env), "=", unname(env)), collapse = " ")
        )
      }
      procs$mark_proc_start(invocation_id)
      procs$set_active_procs(procs$active_procs() + 1)
      invocation_index <- invocation_index + 1L
    }
    start_active_procs <- procs$active_procs()

    while (procs$active_procs() == start_active_procs &&
           procs$active_procs() > 0) {
      procs$wait(0.1)
      procs$poll(0)
      for (invocation_id in invocation_ids) {
        if (!procs$is_queued(invocation_id)) {
          procs$process_output(invocation_id)
          procs$process_error_output(invocation_id)
        }
      }
      procs$set_active_procs(procs$num_alive())
    }
    procs$check_finished()
  }
  procs$set_total_time(as.double((Sys.time() - start_time), units = "secs"))
}
CmdStanRun$set("private", name = "run_invocations_", value = .run_invocations)

.run_sample <- function(mpi_cmd = NULL, mpi_args = NULL) {
  procs <- self$procs
  num_chains <- self$num_chains()
  num_invocations <- self$num_procs()
  if (procs$show_stdout_messages()) {
    start_msg <- paste0(
      "Running ", num_chains, " chain", if (num_chains == 1L) "" else "s",
      " in ", num_invocations, " CmdStan invocation",
      if (num_invocations == 1L) "" else "s"
    )
    is_mpi <- !is.null(mpi_cmd) || (!is.null(self$plan) &&
      any(vapply(self$plan$invocations, function(x) !is.null(x$mpi_cmd), logical(1))))
    if (!is.null(self$plan)) {
      total_threads <- if (length(self$plan$invocations[[1]]$env)) {
        self$plan$invocations[[1]]$num_threads
      } else {
        procs$parallel_procs()
      }
    }
    if (is_mpi) {
      invocation_mpi_args <- if (!is.null(self$plan)) {
        self$plan$invocations[[1]]$mpi_args
      } else {
        mpi_args
      }
      ranks <- invocation_mpi_args[["n"]] %||% invocation_mpi_args[["np"]]
      start_msg <- paste0(start_msg, " using MPI")
      if (!is.null(ranks)) {
        start_msg <- paste0(start_msg, " with ", ranks, " ranks")
      }
      if (!is.null(self$plan) && length(self$plan$invocations[[1]]$env)) {
        start_msg <- paste0(
          start_msg,
          if (is.null(ranks)) " with " else " and ",
          total_threads, " thread", if (total_threads == 1L) "" else "s",
          " per rank"
        )
      }
    } else if (!is.null(self$plan)) {
      start_msg <- paste0(start_msg, " with ", total_threads, " total thread",
                          if (total_threads == 1L) "" else "s")
    }
    cat(start_msg, "...\n\n", sep = "")
  }
  private$run_invocations_(mpi_cmd, mpi_args)
}
CmdStanRun$set("private", name = "run_sample_", value = .run_sample)

.run_generate_quantities <- function() {
  procs <- self$procs
  if (procs$show_stdout_messages()) {
    start_msg <- paste0(
      "Running standalone generated quantities for ", self$num_chains(),
      " chain", if (self$num_chains() == 1L) "" else "s",
      " in ", self$num_procs(), " CmdStan invocation",
      if (self$num_procs() == 1L) "" else "s"
    )
    if (!is.null(self$plan)) {
      total_threads <- if (length(self$plan$invocations[[1]]$env)) {
        self$plan$invocations[[1]]$num_threads
      } else {
        procs$parallel_procs()
      }
      start_msg <- paste0(start_msg, " with ", total_threads, " total thread",
                          if (total_threads == 1L) "" else "s")
    }
    cat(start_msg, "...\n\n", sep = "")
  }
  private$run_invocations_()
}
CmdStanRun$set("private", name = "run_generate_quantities_", value = .run_generate_quantities)

.run_other <- function() {
  procs <- self$procs
  child_env <- if (is.null(procs$threads_per_proc())) {
    NULL
  } else {
    cmdstan_thread_env(procs$threads_per_proc())
  }
  start_time <- Sys.time()
  id <- 1
  procs$new_proc(
    id = id,
    command = self$command(),
    args = self$command_args()[[id]],
    wd = dirname(self$exe_file()),
    env = child_env
  )
  procs$set_active_procs(1)
  procs$mark_proc_start(id)
  procs$set_proc_state(id = id, new_state = 2) # active process
  while (procs$active_procs() == 1) {
    procs$wait(0.1)
    procs$poll(0)
    if (!procs$is_queued(id)) {
      procs$process_output(id)
      procs$process_error_output(id)
    }
    procs$set_active_procs(procs$num_alive())
  }
  procs$process_output(id)
  procs$process_error_output(id)
  successful_fit <- FALSE
  if (self$method() %in% "optimize") { # QUESTION: should this include laplace?
    if (procs$proc_state(id = id) > 3) {
      successful_fit <- TRUE
    }
  } else if (self$method() == "pathfinder") {
    if (procs$proc_state(id = id) > 3 | procs$get_proc(id)$get_exit_status() == 0) {
      successful_fit <- TRUE
    }
  } else if (procs$get_proc(id)$get_exit_status() == 0) {
    successful_fit <- TRUE
  }
  if (successful_fit) {
    procs$set_proc_state(id = id, new_state = 5) # mark_proc_stop will mark this process successful
  } else {
    procs$set_proc_state(id = id, new_state = 4) # mark_proc_stop will mark this process unsuccessful
  }
  procs$mark_proc_stop(id)
  procs$set_total_time(as.double((Sys.time() - start_time), units = "secs"))
  procs$report_time()
}
CmdStanRun$set("private", name = "run_optimize_", value = .run_other)
CmdStanRun$set("private", name = "run_laplace_", value = .run_other)
CmdStanRun$set("private", name = "run_variational_", value = .run_other)
CmdStanRun$set("private", name = "run_pathfinder_", value = .run_other)

.run_diagnose <- function() {
  procs <- self$procs
  child_env <- if (is.null(procs$threads_per_proc())) {
    NULL
  } else {
    cmdstan_thread_env(procs$threads_per_proc())
  }
  stdout_file <- tempfile()
  stderr_file <- tempfile()

  withr::with_path(
    c(
      toolchain_PATH_env_var(),
      tbb_path()
    ),
    ret <- wsl_compatible_run(
      command = self$command(),
      args = self$command_args()[[1]],
      wd = dirname(self$exe_file()),
      stderr = stderr_file,
      stdout = stdout_file,
      env = child_env,
      error_on_status = FALSE
    )
  )
  if (is.na(ret$status) || ret$status != 0) {
    if (file.exists(stdout_file)) {
      cat(readLines(stdout_file), sep = "\n")
    }
    if (file.exists(stderr_file)) {
      cat(readLines(stderr_file), sep = "\n")
    }
    stop(
      "Diagnose failed with the status code ", ret$status, "!\n",
      "See the output above for more information.",
      call. = FALSE
    )
  }
}
CmdStanRun$set("private", name = "run_diagnose_", value = .run_diagnose)


# CmdStanProcs ------------------------------------------------------------

#' System processes for running CmdStan using the `processx::process` R6 class
#'
#' The internal `CmdStanProcs` R6 class provides methods for setting up the
#' system processes for running CmdStan, monitoring the status of the processes,
#' and handling stdout and stderr.
#'
#' @noRd
#' @param num_procs The number of CmdStan processes to start for a run. For MCMC
#'   this is the number of chains. Currently for other methods this must be set
#'   to 1.
#' @param parallel_procs The maximum number of processes to run in parallel.
#'   Currently for non-sampling this must be set to 1.
#' @param threads_per_proc The number of threads to use per process to run
#'   parallel sections of model.
#'
CmdStanProcs <- R6::R6Class(
  classname = "CmdStanProcs",
  public = list(
    initialize = function(num_procs,
                          parallel_procs = NULL,
                          threads_per_proc = NULL,
                          invocation_chain_ids = NULL,
                          show_stderr_messages = TRUE,
                          show_stdout_messages = TRUE) {
      checkmate::assert_integerish(num_procs, lower = 1, len = 1, any.missing = FALSE)
      checkmate::assert_integerish(parallel_procs, lower = 1, len = 1, any.missing = FALSE, null.ok = TRUE)
      checkmate::assert_integerish(threads_per_proc, lower = 1, len = 1, null.ok = TRUE)
      if (is.null(invocation_chain_ids)) {
        invocation_chain_ids <- lapply(seq_len(num_procs), as.integer)
      }
      checkmate::assert_list(invocation_chain_ids, len = num_procs)
      for (chain_ids in invocation_chain_ids) {
        checkmate::assert_integerish(
          chain_ids,
          lower = 1,
          min.len = 1,
          unique = TRUE,
          any.missing = FALSE
        )
      }
      all_chain_ids <- unlist(invocation_chain_ids, use.names = FALSE)
      if (anyDuplicated(all_chain_ids)) {
        stop("Every logical chain ID must belong to exactly one invocation.", call. = FALSE)
      }
      private$num_procs_ <- as.integer(num_procs)
      if (is.null(parallel_procs)) {
        private$parallel_procs_ <- private$num_procs_
      } else {
        private$parallel_procs_ <- as.integer(parallel_procs)
      }
      private$threads_per_proc_ <- if (is.null(threads_per_proc)) {
        NULL
      } else {
        as.integer(threads_per_proc)
      }
      private$active_procs_ <- 0
      private$proc_ids_ <- seq_len(num_procs)
      zeros <- rep(0, num_procs)
      names(zeros) <- private$proc_ids_
      private$proc_state_ <- zeros
      private$proc_start_time_ <- zeros
      private$proc_total_time_ <- zeros
      private$invocation_chain_ids_ <- lapply(invocation_chain_ids, as.integer)
      private$chain_output_ <- stats::setNames(
        vector("list", length(all_chain_ids)),
        as.character(all_chain_ids)
      )
      private$chain_progress_ <- stats::setNames(
        rep(0L, length(all_chain_ids)),
        as.character(all_chain_ids)
      )
      private$show_stderr_messages_ <- show_stderr_messages
      private$show_stdout_messages_ <- show_stdout_messages
      invisible(self)
    },
    show_stdout_messages = function () {
      private$show_stdout_messages_
    },
    show_stderr_messages = function () {
      private$show_stderr_messages_
    },
    num_procs = function() {
      private$num_procs_
    },
    parallel_procs = function() {
      private$parallel_procs_
    },
    threads_per_proc = function() {
      private$threads_per_proc_
    },
    proc_ids = function() {
      private$proc_ids_
    },
    invocation_chain_ids = function(id = NULL) {
      if (is.null(id)) {
        return(private$invocation_chain_ids_)
      }
      private$invocation_chain_ids_[[id]]
    },
    num_chains = function() {
      sum(lengths(private$invocation_chain_ids_))
    },
    num_failed_chains = function() {
      failed <- self$is_failed()
      if (!any(failed)) {
        return(0L)
      }
      as.integer(sum(lengths(private$invocation_chain_ids_[failed])))
    },
    record_chain_output = function(line) {
      chain_id <- parse_chain_output_id(line)
      if (is.na(chain_id) ||
          !as.character(chain_id) %in% names(private$chain_output_)) {
        return(invisible(FALSE))
      }
      key <- as.character(chain_id)
      private$chain_output_[[key]] <- c(private$chain_output_[[key]], line)
      parsed_line <- sub("^Chain \\[[0-9]+\\](?: |$)", "", line, perl = TRUE)
      progress <- private$chain_progress_[[key]]
      if (grepl("Rejecting initial value:|Exception:", parsed_line, perl = TRUE)) {
        progress <- max(progress, 2L)
      }
      if (grepl("Iteration:", parsed_line, fixed = TRUE)) {
        if (grepl("Sampling", parsed_line, fixed = TRUE)) {
          progress <- max(progress, 4L)
        } else {
          progress <- max(progress, 3L)
        }
      }
      if (grepl("\\[100%\\]|Elapsed Time:", parsed_line, perl = TRUE)) {
        progress <- max(progress, 5L)
      }
      private$chain_progress_[[key]] <- as.integer(progress)
      invisible(TRUE)
    },
    chain_output = function(chain_id) {
      checkmate::assert_integerish(
        chain_id,
        lower = 1,
        len = 1,
        any.missing = FALSE
      )
      key <- as.character(as.integer(chain_id))
      if (!key %in% names(private$chain_output_)) {
        stop("Unknown logical chain ID ", chain_id, ".", call. = FALSE)
      }
      private$chain_output_[[key]]
    },
    chain_progress = function(chain_id = NULL) {
      if (is.null(chain_id)) {
        return(private$chain_progress_)
      }
      key <- as.character(as.integer(chain_id))
      if (!key %in% names(private$chain_progress_)) {
        stop("Unknown logical chain ID ", chain_id, ".", call. = FALSE)
      }
      private$chain_progress_[[key]]
    },
    cleanup = function() {
      lapply(private$processes_, function(p) {
        try(p$kill_tree(), silent = TRUE)
      })
      invisible(self)
    },
    poll = function(ms) { # time in milliseconds
      processx::poll(private$processes_, ms)
    },
    wait = function(s) { # time in seconds
      Sys.sleep(s)
    },
    get_proc = function(id) {
      private$processes_[[id]]
    },
    new_proc = function(id,
                        command,
                        args,
                        wd,
                        env = NULL,
                        mpi_cmd = NULL,
                        mpi_args = NULL) {
      if (!is.null(mpi_cmd)) {
        exe_name <- mpi_args[["exe"]]
        mpi_args[["exe"]] <- NULL
        mpi_args_vector <- c()
        for (i in names(mpi_args)) {
          mpi_args_vector <- c(paste0("-", i), mpi_args[[i]], mpi_args_vector)
        }
        args <- c(mpi_args_vector, exe_name, args)
        command <- mpi_cmd
      }
      withr::with_path(
        c(
          toolchain_PATH_env_var(),
          tbb_path()
        ),
        {
          child_env <- env
          if (!is.null(child_env) && !("PATH" %in% names(child_env))) {
            child_env <- c(child_env, PATH = Sys.getenv("PATH"))
          }
          private$processes_[[id]] <- wsl_compatible_process_new(
            command = command,
            args = args,
            wd = wd,
            env = child_env,
            stdout = "|",
            stderr = "|",
            echo_cmd = is_verbose_mode()
          )
        }
      )
      invisible(self)
    },
    active_procs = function() {
      private$active_procs_
    },
    set_active_procs = function(procs) {
      private$active_procs_ <- procs
      invisible(NULL)
    },
    proc_section_time = function(section, id = NULL) {
      if (section %in% colnames(private$proc_section_time_)) {
        if (is.null(id)) {
          return(private$proc_section_time_[, section])
        }
        private$proc_section_time_[id, section]
      }else {
        NA_real_
      }
    },
    proc_total_time = function(id = NULL) {
      if (is.null(id)) {
        return(private$proc_total_time_[self$is_finished()])
      }
      private$proc_total_time_[[id]]
    },
    total_time = function() {
      # scalar overall time
      private$total_time_
    },
    set_total_time = function(time) {
      private$total_time_ <- as.numeric(time)
      invisible(self)
    },
    check_finished = function() {
      for (id in private$proc_ids_) {
        if (self$is_still_working(id) && !self$is_queued(id) && !self$is_alive(id)) {
          # if the process just finished make sure we process all
          # input and mark the process finished
          self$process_output(id)
          self$process_error_output(id)
          if (self$get_proc(id)$get_exit_status() == 0) {
            self$set_proc_state(id = id, new_state = 5)
          } else {
            self$set_proc_state(id = id, new_state = 4)
          }
          self$mark_proc_stop(id)
        }
      }
      invisible(self)
    },
    is_alive = function(id = NULL) {
      if (!is.null(id)) {
        return(private$processes_[[id]]$is_alive())
      }
      sapply(private$processes_, function(x) x$is_alive())
    },
    is_still_working = function(id = NULL) {
      self$proc_state(id) < 6
    },
    is_finished = function(id = NULL) {
      self$proc_state(id) == 6
    },
    is_failed = function(id = NULL) {
      self$proc_state(id) == 7
    },
    is_queued = function(id = NULL) {
      self$proc_state(id) == 0
    },
    num_alive = function() {
      sum(self$is_alive())
    },
    num_failed = function() {
      sum(self$proc_state() == 7)
    },
    any_queued = function() {
      any(self$is_queued())
    },
    proc_output = function(id = NULL) {
      out <- private$proc_output_
      if (is.null(id)) {
        return(out)
      }
      out[[id]]
    },
    proc_state = function(id = NULL) {
      if (is.null(id)) {
        return(private$proc_state_)
      }
      private$proc_state_[[id]]
    },
    set_proc_state = function(id, new_state) {
      private$proc_state_[[id]] <- new_state
      if (identical(as.integer(new_state), 7L)) {
        chain_ids <- private$invocation_chain_ids_[[id]]
        private$chain_progress_[as.character(chain_ids)] <- 7L
      }
    },
    mark_proc_start = function(id) {
      private$proc_state_[[id]] <- 1
      private$proc_output_[[id]] <- c("")
      invisible(self)
    },
    mark_proc_stop = function(id) {
      if (private$proc_state_[[id]] == 5) {
        private$proc_state_[[id]] <- 6
      } else {
        private$proc_state_[[id]] <- 7
      }
      chain_ids <- private$invocation_chain_ids_[[id]]
      private$chain_progress_[as.character(chain_ids)] <- private$proc_state_[[id]]
      invisible(self)
    },
    is_error_message = function(line) {
      startsWith(line, "Exception:") ||
      (grepl("either mistyped or misplaced.", line, perl = TRUE)) ||
      (grepl("A method must be specified!", line, perl = TRUE)) ||
      (grepl("is not a valid value for", line, perl = TRUE))
    },
    process_error_output = function(id) {
      err_out <- self$get_proc(id)$read_error_lines()
      if (length(err_out)) {
        for (err_line in err_out) {
          private$proc_output_[[id]] <- c(private$proc_output_[[id]], err_line)
          self$record_chain_output(err_line)
          if (private$show_stderr_messages_) {
            if (is.na(parse_chain_output_id(err_line))) {
              message("Chain ", id, " ", err_line)
            } else {
              message(err_line)
            }
          }
        }
      }
    },
    process_output = function(id) {
      out <- self$get_proc(id)$read_output_lines()
      if (length(out) == 0) {
        return(NULL)
      }
      for (line in out) {
        private$proc_output_[[id]] <- c(private$proc_output_[[id]], line)
        if (nzchar(line)) {
          if (grepl("Optimization terminated with error", line, perl = TRUE)) {
            self$set_proc_state(id, new_state = 3.5)
          }
          if (grepl("Optimization terminated normally", line, perl = TRUE)) {
            self$set_proc_state(id, new_state = 4)
          }
          if (self$proc_state(id) == 2 && grepl("refresh = ", line, perl = TRUE)) {
            self$set_proc_state(id, new_state = 2.5)
          }
          if (self$proc_state(id) == 2.5 && grepl("Exception:", line, fixed = TRUE)) {
            self$set_proc_state(id, new_state = 3)
          }
          if (private$proc_state_[[id]] == 3.5) {
            message(line)
          } else if ((private$show_stdout_messages_ && private$proc_state_[[id]] >= 3) || is_verbose_mode()) {
            cat(line, collapse = "\n")
          }
        } else {
          # after the metadata is printed and we found a blank line
          # this represents the start of fitting
          if (self$proc_state(id) == 2.5) {
              self$set_proc_state(id, new_state = 3)
          }
        }
      }
      invisible(self)
    },
    report_time = function(id = NULL) {
      if (self$proc_state(id) == 7) {
        warning("Fitting finished unexpectedly! Use the $output() method for more information.\n", immediate. = TRUE, call. = FALSE)
      }
      if (private$show_stdout_messages_) {
        cat("Finished in ",
            base::format(round(self$total_time(), 1), nsmall = 1),
            "seconds.\n")
      }
    },
    return_codes = function() {
      ret <- c()
      for (id in private$proc_ids_) {
        ret <- c(ret, self$get_proc(id)$get_exit_status())
      }
      ret
    }
  ),
  private = list(
    processes_ = NULL, # will be list of processx::process objects
    proc_ids_ = integer(),
    num_procs_ = integer(),
    parallel_procs_ = integer(),
    active_procs_ = integer(),
    threads_per_proc_ = integer(),
    proc_state_ = NULL,
    proc_start_time_ = NULL,
    proc_total_time_ = NULL,
    proc_section_time_ = data.frame(),
    proc_output_ = list(),
    proc_error_ouput_ = list(),
    total_time_ = numeric(),
    show_stderr_messages_ = TRUE,
    show_stdout_messages_ = TRUE,
    invocation_chain_ids_ = list(),
    chain_output_ = list(),
    chain_progress_ = integer()
  )
)

# Process R6 class that overrides the default
# function for processing the output
CmdStanMCMCProcs <- R6::R6Class(
  classname = "CmdStanMCMCProcs",
  inherit = CmdStanProcs,
  public = list(
    process_output = function(id) {
      out <- self$get_proc(id)$read_output_lines()
      if (length(out) == 0) {
        return(invisible(NULL))
      }
      for (line in out) {
        private$proc_output_[[id]] <- c(private$proc_output_[[id]], line)
        self$record_chain_output(line)
        if (nzchar(line)) {
          chain_prefixed <- grepl("^Chain \\[[0-9]+\\](?: |$)", line, perl = TRUE)
          parsed_line <- sub("^Chain \\[[0-9]+\\](?: |$)", "", line, perl = TRUE)
          parse_section_time <- function(value) {
            match <- regexpr(
              "[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?(?=\\s+seconds)",
              value,
              perl = TRUE
            )
            if (match[[1]] < 0L) {
              return(NA_real_)
            }
            as.double(regmatches(value, match))
          }
          ignore_line <- FALSE
          last_section_start_time <- private$proc_section_time_[id, "last_section_start"]
          state <- private$proc_state_[[id]]
          # State machine for reading stdout.
          # 0 - chain has not started yet
          # 1 - chain is initializing (before iterations) and no output is printed
          # 2 - chain is initializing and inital values were rejected (output is printed)
          # 3 - chain is in warmup
          # 4 - chain is in sampling
          # 5 - iterations are done but the chain process has not stopped yet
          # 6 - the chain's process has stopped
          # (Note: state 2 is only used because rejection in cmdstan is printed
          # to stdout not stderr and we want to avoid printing the intial chain metadata)
          next_state <- state
          if (state < 3 && grepl("refresh =", parsed_line, perl = TRUE)) {
            state <- 1.5
            next_state <- 1.5
          }
          if (state <= 3 && grepl("Rejecting initial value:", parsed_line, perl = TRUE)) {
            state <- 2
            next_state <- 2
          }
          if (state < 3 && grepl("Iteration:", parsed_line, perl = TRUE)) {
            state <- 3 # 3 =  warmup
            next_state <- 3
          }
          if (state < 3 && grepl("Elapsed Time:", parsed_line, perl = TRUE)) {
            state <- 5 # 5 = end of sampling
            next_state <- 5
          }
          if (private$proc_state_[[id]] == 3 &&
              grepl("(Sampling)", parsed_line, perl = TRUE)) {
            next_state <- 4 # 4 = sampling
          }
          if (grepl("\\[100%\\]", parsed_line, perl = TRUE)) {
            next_state <- 5 # writing csv and finishing
          }
          if (grepl("seconds (Total)", parsed_line, fixed = TRUE)) {
            private$proc_total_time_[[id]] <- parse_section_time(parsed_line)
            next_state <- 5
            state <- 5
          }
          if (grepl("seconds (Sampling)", parsed_line, fixed = TRUE)) {
            private$proc_section_time_[id, "sampling"] <- parse_section_time(parsed_line)
            next_state <- 5
            state <- 5
          }
          if (grepl("seconds (Warm-up)", parsed_line, fixed = TRUE)) {
            private$proc_section_time_[id, "warmup"] <- parse_section_time(parsed_line)
            next_state <- 5
            state <- 5
          }
          if (grepl("Gradient evaluation took", parsed_line, fixed = TRUE)
              || grepl("leapfrog steps per transition would take", parsed_line, fixed = TRUE)
              || grepl("Adjust your expectations accordingly!", parsed_line, fixed = TRUE)
              || grepl("stanc_version", parsed_line, fixed = TRUE)
              || grepl("stancflags", parsed_line, fixed = TRUE)) {
            ignore_line <- TRUE
          }
          if ((state > 1.5 && state < 5 && !ignore_line && private$show_stdout_messages_) || is_verbose_mode()) {
            if (state == 2) {
              if (chain_prefixed) message(line) else message("Chain ", id, " ", line)
            } else {
              if (chain_prefixed) cat(line, "\n") else cat("Chain", id, line, "\n")
            }
          }
          if (self$is_error_message(parsed_line)) {
            # will print all remaining output in case of exceptions
            if (state == 1) {
              state <- 2;
            }
            if (private$show_stderr_messages_) {
              if (chain_prefixed) message(line) else message("Chain ", id, " ", line)
            }
          }
          private$proc_state_[[id]] <- next_state
        } else {
          if (private$proc_state_[[id]] == 1.5) {
            private$proc_state_[[id]] <- 3
          }
        }
      }
      invisible(self)
    },
    report_time = function(id = NULL) {
      if (!private$show_stdout_messages_) {
        return(invisible(NULL))
      }
      if (!is.null(id)) {
        if (self$proc_state(id) == 7) {
          chain_ids <- self$invocation_chain_ids(id)
          if (length(chain_ids) == 1L) {
            warning(
              "Chain ", chain_ids, " finished unexpectedly!\n",
              immediate. = TRUE,
              call. = FALSE
            )
          } else if (length(chain_ids) == self$num_chains()) {
            warning(
              "All chains finished unexpectedly!\n",
              immediate. = TRUE,
              call. = FALSE
            )
          } else {
            warning(
              length(chain_ids), " chain(s) finished unexpectedly!\n",
              immediate. = TRUE,
              call. = FALSE
            )
          }
        } else if (length(self$invocation_chain_ids(id)) > 1L) {
          cat(
            "CmdStan invocation", id, "finished in",
            base::format(round(self$proc_total_time(id), 1), nsmall = 1),
            "seconds.\n"
          )
        } else {
          cat(
            "Chain", self$invocation_chain_ids(id), "finished in",
            base::format(round(self$proc_total_time(id), 1), nsmall = 1),
            "seconds.\n"
          )
        }
        return(invisible(NULL))
      } else {
        num_chains <- self$num_chains()
        if (num_chains > 1) {
          num_failed <- self$num_failed_chains()
          if (num_failed == 0) {
            if (num_chains == 2) {
              cat("\nBoth chains finished successfully.\n")
            } else {
              cat("\nAll", num_chains, "chains finished successfully.\n")
            }
            timing_scope <- if (all(lengths(self$invocation_chain_ids()) == 1L)) {
              "chain"
            } else {
              "invocation"
            }
            cat("Mean", timing_scope, "execution time:",
                base::format(round(mean(self$proc_total_time()), 1), nsmall = 1),
                "seconds.\n")
            cat("Total execution time:",
                base::format(round(self$total_time(), 1), nsmall = 1),
                "seconds.\n\n")
          } else if (num_failed == num_chains) {
            warning("All chains finished unexpectedly! Use the $output(chain_id) method for more information.\n", call. = FALSE)
            warning("Use read_cmdstan_csv() to read the results of the failed chains.",
                    immediate. = TRUE,
                    call. = FALSE)
          } else {
            warning(num_failed, " chain(s) finished unexpectedly!",
                    immediate. = TRUE,
                    call. = FALSE)
            cat("The remaining chains had a mean execution time of",
                base::format(round(mean(self$total_time()), 1), nsmall = 1),
                "seconds.\n")
            warning("The returned fit object will only read in results of successful chains. ",
              "Please use read_cmdstan_csv() to read the results of the failed chains separately.",
              "Use the $output(chain_id) method for more output of the failed chains.",
              immediate. = TRUE,
              call. = FALSE)
          }
        }
        return(invisible(NULL))
      }
    }
  )
)

CmdStanGQProcs <- R6::R6Class(
  classname = "CmdStanGQProcs",
  inherit = CmdStanProcs,
  public = list(
    check_finished = function() {
      for (id in private$proc_ids_) {
        # if process is not finished yet
        if (self$is_still_working(id) && !self$is_queued(id) && !self$is_alive(id)) {
          # if the process just finished make sure we process all
          # input and mark the process finished
          self$process_output(id)
          self$process_error_output(id)
          if (self$get_proc(id)$get_exit_status() == 0) {
            self$set_proc_state(id = id, new_state = 5) # mark_proc_stop will mark this process successful
          } else {
            self$set_proc_state(id = id, new_state = 4) # mark_proc_stop will mark this process unsuccessful
          }
          self$mark_proc_stop(id)
        }
      }
      invisible(self)
    },
    process_output = function(id) {
      out <- self$get_proc(id)$read_output_lines()
      if (length(out) == 0) {
        return(NULL)
      }
      for (line in out) {
        private$proc_output_[[id]] <- c(private$proc_output_[[id]], line)
        self$record_chain_output(line)
        if (nzchar(line)) {
          chain_prefixed <- grepl("^Chain \\[[0-9]+\\](?: |$)", line, perl = TRUE)
          parsed_line <- sub("^Chain \\[[0-9]+\\](?: |$)", "", line, perl = TRUE)
          if (self$proc_state(id) == 1 && grepl("refresh = ", parsed_line, perl = TRUE)) {
            self$set_proc_state(id, new_state = 1.5)
          } else if (self$proc_state(id) >= 2 && private$show_stdout_messages_) {
            if (chain_prefixed) cat(line, "\n") else cat("Chain", id, line, "\n")
          }
        } else {
          # after the metadata is printed and we found a blank line
          # this represents the start of fitting
          if (self$proc_state(id) == 1.5) {
              self$set_proc_state(id, new_state = 2)
          }
        }
      }
      invisible(self)
    },
    report_time = function(id = NULL) {
      if (!private$show_stdout_messages_) {
        return(invisible(NULL))
      }
      if (!is.null(id)) {
        if (self$proc_state(id) == 7) {
          chain_ids <- self$invocation_chain_ids(id)
          if (length(chain_ids) == 1L) {
            warning(
              "Chain ", chain_ids, " finished unexpectedly!\n",
              immediate. = TRUE,
              call. = FALSE
            )
          } else if (length(chain_ids) == self$num_chains()) {
            warning(
              "All chains finished unexpectedly!\n",
              immediate. = TRUE,
              call. = FALSE
            )
          } else {
            warning(
              length(chain_ids), " chain(s) finished unexpectedly!\n",
              immediate. = TRUE,
              call. = FALSE
            )
          }
        } else if (length(self$invocation_chain_ids(id)) > 1L) {
          cat(
            "CmdStan invocation", id, "finished in",
            base::format(round(self$proc_total_time(id), 1), nsmall = 1),
            "seconds.\n"
          )
        } else {
          cat(
            "Chain", self$invocation_chain_ids(id), "finished in",
            base::format(round(self$proc_total_time(id), 1), nsmall = 1),
            "seconds.\n"
          )
        }
        return(invisible(NULL))
      } else {
        num_chains <- self$num_chains()
        if (num_chains > 1) {
          num_failed <- self$num_failed_chains()
          if (num_failed == 0) {
            if (num_chains == 2) {
              cat("\nBoth chains finished successfully.\n")
            } else {
              cat("\nAll", num_chains, "chains finished successfully.\n")
            }
            timing_scope <- if (all(lengths(self$invocation_chain_ids()) == 1L)) {
              "chain"
            } else {
              "invocation"
            }
            cat("Mean", timing_scope, "execution time:",
                base::format(round(mean(self$proc_total_time()), 1), nsmall = 1),
                "seconds.\n")
            cat("Total execution time:",
                base::format(round(self$total_time(), 1), nsmall = 1),
                "seconds.\n")
          } else if (num_failed == num_chains) {
            warning("All chains finished unexpectedly!\n", call. = FALSE)
            warning("Use read_cmdstan_csv() to read the results of the failed chains.",
                    "Use $output(chain_id) on the fit object for more output of the failed chains.",
                    immediate. = TRUE,
                    call. = FALSE)
          } else {
            warning(num_failed, " chain(s) finished unexpectedly!",
                    immediate. = TRUE,
                    call. = FALSE)
            cat("The remaining chains had a mean execution time of",
                base::format(round(mean(self$total_time()), 1), nsmall = 1),
                "seconds.\n")
            warning("The returned fit object will only read in results of successful chains. ",
                    "Please use read_cmdstan_csv() to read the results of the failed chains separately.",
                    "Use $output(chain_id) on the fit object for more output of the failed chains.",
                    immediate. = TRUE,
                    call. = FALSE)
          }
        }
        return(invisible(NULL))
      }
    }
  )
)

tbb_path <- function(dir = NULL) {
  path_to_TBB <- NULL
  if (os_is_windows()) {
    if (is.null(dir)) {
      dir <- cmdstan_path()
    }
    path_to_TBB <- file.path(dir, "stan", "lib", "stan_math", "lib", "tbb")
  }
  path_to_TBB
}
