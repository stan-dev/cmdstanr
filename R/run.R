# CmdStanRun (RunSet) -----------------------------------------------------

# Record of CmdStan runs for a specified configuration and number of chains.
CmdStanRun <- R6::R6Class(
  classname = "CmdStanRun",
  public = list(
    # Initialize object
    # @param args CmdStanArgs object.
    # @param procs CmdStanProcs object.
    args = NULL,
    procs = NULL,
    initialize = function(args, procs) {
      checkmate::assert_r6(args, classes = "CmdStanArgs")
      checkmate::assert_r6(procs, classes = "CmdStanProcs")
      self$args <- args
      self$procs <- procs
      private$output_files_ <- self$new_output_files()
      if (self$args$save_latent_dynamics) {
        private$latent_dynamics_files_ <- self$new_latent_dynamics_files()
      }
      invisible(self)
    },

    num_procs = function() self$procs$num_procs(),
    proc_ids = function() self$procs$proc_ids(),
    exe_file = function() self$args$exe_file,
    model_name = function() self$args$model_name,
    method = function() self$args$method,
    data_file = function() self$args$data_file,
    new_output_files = function() {
      self$args$new_files(type = "output")
    },
    new_latent_dynamics_files = function() {
      self$args$new_files(type = "diagnostic")
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
        ok <- self$procs$is_finished() | self$procs$is_queued()
        private$latent_dynamics_files_[ok]
      }
    },
    output_files = function(include_failed = FALSE) {
      if (include_failed) {
        private$output_files_
      } else {
        ok <- self$procs$is_finished() | self$procs$is_queued()
        private$output_files_[ok]
      }
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
        ids = self$procs$proc_ids(),
        ext = ".csv",
        timestamp = timestamp,
        random = random
      )
      file.remove(current_files[!current_files %in% new_paths])
      private$output_files_ <- new_paths
      message("Moved ", length(current_files),
              " files and set internal paths to new locations:\n",
              paste("-", new_paths, collapse = "\n"))
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
        ids = self$proc_ids(),
        ext = ".csv",
        timestamp = timestamp,
        random = random
      )
      file.remove(current_files[!current_files %in% new_paths])
      private$latent_dynamics_files_ <- new_paths
      message("Moved ", length(current_files),
              " files and set internal paths to new locations:\n",
              paste("-", new_paths, collapse = "\n"))
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

    command = function() self$args$command(),
    command_args = function() {
      if (!length(private$command_args_)) {
        # create a list of character vectors (one per run/chain) of cmdstan arguments
        private$command_args_ <- lapply(self$procs$proc_ids(), function(j) {
          self$args$compose_all_args(
            idx = j,
            output_file = private$output_files_[j],
            latent_dynamics_file = private$latent_dynamics_files_[j] # maybe NULL
          )
        })
      }
      private$command_args_
    },

    run_cmdstan = function() {
      if (self$method() == "sample") {
        private$run_sample_()
      } else if (self$method() == "optimize") {
        private$run_optimize_()
      } else if (self$method() == "variational") {
        private$run_variational_()
      }
    },

    # run bin/stansummary or bin/diagnose
    # @param tool The name of the tool in `bin/` to run.
    # @param flags An optional character vector of flags (e.g. c("--sig_figs=1")).
    run_cmdstan_tool = function(tool = c("stansummary", "diagnose"), flags = NULL) {
      if (self$method() == "optimize") {
        stop("Not available for optimize method.", call. = FALSE)
      }
      tool <- match.arg(tool)
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("No CmdStan runs finished successfully. ",
             "Unable to run bin/", tool, ".", call. = FALSE)
      }
      target_exe = file.path("bin", cmdstan_ext(tool))
      check_target_exe(target_exe)
      run_log <- processx::run(
        command = target_exe,
        args = c(self$output_files(include_failed = FALSE), flags),
        wd = cmdstan_path(),
        echo_cmd = TRUE,
        echo = TRUE,
        error_on_status = TRUE
      )
    },

    time = function() {
      if (self$method() != "sample") {
        time <- list(total = self$procs$total_time())
      } else {
        chain_time <- data.frame(
          chain_id = self$procs$proc_ids()[self$procs$is_finished()],
          warmup = self$procs$proc_section_time("warmup")[self$procs$is_finished()],
          sampling = self$procs$proc_section_time("sampling")[self$procs$is_finished()],
          total = self$procs$proc_total_time()[self$procs$is_finished()]
        )

        if (isTRUE(self$args$refresh == 0)) {
          warning("Separate warmup and sampling times are not available ",
                  "after running with 'refresh=0'.", call. = FALSE)
        }
        time <- list(total = self$procs$total_time(), chains = chain_time)
      }
      time
    }
  ),
  private = list(
    output_files_ = character(),
    latent_dynamics_files_ = NULL,
    command_args_ = list(),

    finalize = function() {
      if (self$args$using_tempdir) {
        temp_files <- c(
          self$output_files(include_failed = TRUE),
          if (self$args$save_latent_dynamics)
            self$latent_dynamics_files(include_failed = TRUE)
        )
        message("Cleaning up CmdStanR temporary files")
        unlink(temp_files)
      }
    }
  )
)


# run helpers -------------------------------------------------
.run_sample <- function() {
  procs <- self$procs
  on.exit(procs$cleanup(), add = TRUE)

  # add path to the TBB library to the PATH variable
  if (cmdstan_version() >= "2.21" && os_is_windows()) {
    path_to_TBB <- file.path(cmdstan_path(), "stan", "lib", "stan_math", "lib", "tbb")
    current_path <- Sys.getenv("PATH")
    if (regexpr("path_to_TBB", current_path, perl = TRUE) <= 0) {
      Sys.setenv(PATH = paste0(path_to_TBB, ";", Sys.getenv("PATH")))
    }
  }
  if (procs$num_procs() == 1) {
    start_msg <- "Running MCMC with 1 chain"
  } else if (procs$num_procs() == procs$parallel_procs()) {
    start_msg <- paste0("Running MCMC with ", procs$num_procs(), " parallel chains")
  } else {
    if (procs$parallel_procs() == 1) {
      start_msg <- paste0("Running MCMC with ", procs$num_procs(), " sequential chains")
    } else {
      start_msg <- paste0("Running MCMC with ", procs$num_procs(), " chains, at most ", procs$parallel_procs(), " in parallel")
    }
  }
  if (is.null(procs$threads_per_proc())) {
    cat(paste0(start_msg, "...\n\n"))
  } else {
    cat(paste0(start_msg, ", with ", procs$threads_per_proc(), " thread(s) per chain...\n\n"))
    Sys.setenv("STAN_NUM_THREADS" = as.integer(procs$threads_per_proc()))
  }
  start_time <- Sys.time()
  chains <- procs$proc_ids()
  chain_ind <- 1
  while (!all(procs$is_finished() | procs$is_failed())) {
    while (procs$active_procs() != procs$parallel_procs() && procs$any_queued()) {
      chain_id <- chains[chain_ind]
      procs$new_proc(
        id = chain_id,
        command = self$command(),
        args = self$command_args()[[chain_id]],
        wd = dirname(self$exe_file())
      )
      procs$mark_proc_start(chain_id)
      procs$set_active_procs(procs$active_procs() + 1)
      chain_ind <- chain_ind + 1
    }
    start_active_procs <- procs$active_procs()

    while (procs$active_procs() == start_active_procs &&
           procs$active_procs() > 0) {
      procs$wait(0.1)
      procs$poll(0)
      for (chain_id in chains) {
        if (!procs$is_queued(chain_id)) {
          output <- procs$get_proc(chain_id)$read_output_lines()
          procs$process_output(output, chain_id)
          error_output <- procs$get_proc(chain_id)$read_error_lines()
          procs$process_error_output(error_output, chain_id)
        }
      }
      procs$set_active_procs(procs$num_alive())
    }
    procs$check_finished()
  }
  procs$set_total_time(as.double((Sys.time() - start_time), units = "secs"))
  procs$report_time()
}
CmdStanRun$set("private", name = "run_sample_", value = .run_sample)

.run_other <- function() {
  procs <- self$procs
  # add path to the TBB library to the PATH variable
  if (cmdstan_version() >= "2.21" && os_is_windows()) {
    path_to_TBB <- file.path(cmdstan_path(), "stan", "lib", "stan_math", "lib", "tbb")
    current_path <- Sys.getenv("PATH")
    if (regexpr("path_to_TBB", current_path, perl = TRUE) <= 0) {
      Sys.setenv(PATH = paste0(path_to_TBB, ";", Sys.getenv("PATH")))
    }
  }
  start_time <- Sys.time()
  id <- 1
  procs$new_proc(
    id = id,
    command = self$command(),
    args = self$command_args()[[id]],
    wd = dirname(self$exe_file())
  )
  procs$set_active_procs(1)
  procs$mark_proc_start(id)
  procs$set_proc_state(id = id, new_state = 2) # active process
  while (procs$active_procs() == 1) {
    procs$wait(0.1)
    procs$poll(0)
    if (!procs$is_queued(id)) {
      output <- procs$get_proc(id)$read_output_lines()
      procs$process_output(output, id)
      error_output <- procs$get_proc(id)$read_error_lines()
      procs$process_error_output(error_output, id)
    }
    procs$set_active_procs(procs$num_alive())
  }
  procs$set_proc_state(id = id, new_state = 5) # successful process
  procs$mark_proc_stop(id)
  procs$set_total_time(as.double((Sys.time() - start_time), units = "secs"))
  procs$report_time()
}
CmdStanRun$set("private", name = "run_optimize_", value = .run_other)
CmdStanRun$set("private", name = "run_variational_", value = .run_other)


# CmdStanProcs ------------------------------------------------------------

# System processes for model fitting
CmdStanProcs <- R6::R6Class(
  classname = "CmdStanProcs",
  public = list(
    # @param num_procs The number of CmdStan processes to start for a run.
    #   For MCMC this is the number of chains. Currently for other methods
    #   this must be set to 1.
    # @param parallel_procs The maximum number of processes to run in parallel.
    #   Currently for non-sampling this must be set to 1.
    # @param threads_per_proc The number of threads to use per process
    #   to run parallel sections of model.
    initialize = function(num_procs, parallel_procs = NULL, threads_per_proc = NULL) {
      checkmate::assert_integerish(num_procs, lower = 1, len = 1, any.missing = FALSE)
      checkmate::assert_integerish(parallel_procs, lower = 1, len = 1, any.missing = FALSE,
                                   .var.name = "parallel_procs", null.ok = TRUE)
      checkmate::assert_integerish(threads_per_proc, lower = 1, len = 1, null.ok = TRUE,
                                   .var.name = "threads_per_proc")
      private$num_procs_ <- as.integer(num_procs)
      if (is.null(parallel_procs)) {
        private$parallel_procs_ <- private$num_procs_
      } else {
        private$parallel_procs_ <- as.integer(parallel_procs)
      }
      private$threads_per_proc_ <- as.integer(threads_per_proc)
      private$threads_per_proc_ <- threads_per_proc
      private$active_procs_ <- 0
      private$proc_ids_ <- seq_len(num_procs)
      zeros <- rep(0, num_procs)
      names(zeros) <- private$proc_ids_
      private$proc_state_ = zeros
      private$proc_start_time_ = zeros
      private$proc_total_time_ = zeros
      invisible(self)
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
    new_proc = function(id, command, args, wd) {
      private$processes_[[id]] <- processx::process$new(
        command = command,
        args = args,
        wd = wd,
        echo_cmd = FALSE,
        stdout = "|",
        stderr = "|"
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
        # if process is not finished yet
        if (self$is_still_working(id)) {
          if (!self$is_queued(id) && !self$is_alive(id)) {
            # if the process just finished make sure we process all
            # input and mark the process finished
            output <- self$get_proc(id)$read_output_lines()
            self$process_output(output, id)
            error_output <- self$get_proc(id)$read_error_lines()
            self$process_error_output(error_output, id)
            self$mark_proc_stop(id)
            self$report_time(id)
          }
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
    },
    mark_proc_start = function(id) {
      private$proc_start_time_[[id]] <- Sys.time()
      private$proc_state_[[id]] <- 1
      private$proc_section_time_[id, "last_section_start"] <- private$proc_start_time_[[id]]
      private$proc_output_[[id]] <- c("")
      invisible(self)
    },
    mark_proc_stop = function(id) {
      if (private$proc_state_[[id]] == 5) {
        private$proc_state_[[id]] <- 6
        private$proc_total_time_[[id]] <- as.double((Sys.time() - private$proc_start_time_[[id]]), units = "secs")
      } else {
        private$proc_state_[[id]] <- 7
      }
      invisible(self)
    },
    is_error_message = function(line) {
      startsWith(line, "Exception:") ||
      (regexpr("either mistyped or misplaced.", line, perl = TRUE) > 0) ||
      (regexpr("A method must be specified!", line, perl = TRUE) > 0) ||
      (regexpr("is not a valid value for", line, perl = TRUE) > 0)
    },
    process_error_output = function(err_out, id) {
      if(length(err_out)) {
        for(err_line in err_out) {
          message("Chain ", id, " ", err_line)
        }
      }
    },
    process_output = function(out, id) {
      if (length(out) == 0) {
        return(NULL)
      }
      for (line in out) {
        private$proc_output_[[id]] <- c(private$proc_output_[[id]], line)
        cat(line, collapse = "\n")
      }
      invisible(self)
    },
    report_time = function(id = NULL) {
      cat("Finished in ",
              format(round(self$total_time(), 1), nsmall = 1),
              "seconds.\n")
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
    total_time_ = numeric()
  )
)

# Process R6 class that overrides the default
# function for processing the output
CmdStanMCMCProcs <- R6::R6Class(
  classname = "CmdStanMCMCProcs",
  inherit = CmdStanProcs,
  public = list(
    process_output = function(out, id) {
      if (length(out) == 0) {
        return(invisible(NULL))
      }
      for (line in out) {
        private$proc_output_[[id]] <- c(private$proc_output_[[id]], line)
        if (nzchar(line)) {
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
          if (state < 3 && regexpr("Rejecting initial value:", line, perl = TRUE) > 0) {
            state <- 2
            next_state <- 2
          }
          if (state < 3 && regexpr("Iteration:", line, perl = TRUE) > 0) {
            state <- 3 # 3 =  warmup
            next_state <- 3
            private$proc_section_time_[id, "last_section_start"] <- Sys.time()
          }
          if (state < 3 && regexpr("Elapsed Time:", line, perl = TRUE) > 0) {
            state <- 5 # 5 = end of samp+ling
            next_state <- 5
          }
          if (private$proc_state_[[id]] == 3 &&
              regexpr("(Sampling)", line, perl = TRUE) > 0) {
            next_state <- 4 # 4 = sampling
            private$proc_section_time_[id, "warmup"] <- as.double((Sys.time() - last_section_start_time), units = "secs")
            private$proc_section_time_[id, "last_section_start"] <- Sys.time()
          }
          if (regexpr("\\[100%\\]", line, perl = TRUE) > 0) {
            if (state == 3) { #warmup only run
              private$proc_section_time_[id, "warmup"] <- as.double((Sys.time() - last_section_start_time), units = "secs")
            } else if (state == 4) { # sampling
              private$proc_section_time_[id, "sampling"] <- as.double((Sys.time() - last_section_start_time), units = "secs")
            }
            next_state <- 5 # writing csv and finishing
          }
          if (state > 1 && state < 5) {
            if(state == 2) {
              message("Chain ", id, " ", line)
            } else {
              cat("Chain", id, line, "\n")
            }
          }
          if (self$is_error_message(line)) {
            # will print all remaining output in case of exceptions
            if(state == 1) {
              state <- 2;
            }
            message("Chain ", id, " ", line)
          }
          private$proc_state_[[id]] <- next_state
        }
      }
      invisible(self)
    },
    report_time = function(id = NULL) {
      if (!is.null(id)) {
        if (self$proc_state(id) == 7) {
          warning("Chain ", id, " finished unexpectedly!\n", immediate. = TRUE, call. = FALSE)
        } else {
          cat("Chain", id, "finished in", format(round(self$proc_total_time(id), 1), nsmall = 1), "seconds.\n")
        }
        return(invisible(NULL))
      } else {
        num_chains <- self$num_procs()
        if (num_chains > 1) {
          num_failed <- self$num_failed()
          if (num_failed == 0) {
            if (num_chains == 2) {
              cat("\nBoth chains finished successfully.\n")
            } else {
              cat("\nAll", num_chains, "chains finished successfully.\n")
            }
            cat("Mean chain execution time:",
                format(round(mean(self$proc_total_time()), 1), nsmall = 1),
                "seconds.\n")
            cat("Total execution time:",
                format(round(self$total_time(), 1), nsmall = 1),
                "seconds.\n")
          } else if (num_failed == num_chains) {
            warning("All chains finished unexpectedly!\n", call. = FALSE)
            warning("Use read_sample_csv() to read the results of the failed chains.",
                    immediate. = TRUE,
                    call. = FALSE)
          } else {
            warning(num_failed, " chain(s) finished unexpectedly!",
                    immediate. = TRUE,
                    call. = FALSE)
            cat("The remaining chains had a mean execution time of",
                format(round(mean(self$total_time()), 1), nsmall = 1),
                "seconds.\n")
            warning("The returned fit object will only read in results of succesful chains. Please use read_sample_csv() to read the results of the failed chains separately.",
                    immediate. = TRUE,
                    call. = FALSE)
          }
        }
        return(invisible(NULL))
      }
    }
  )
)
