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
      if (self$args$save_diagnostics) {
        private$diagnostic_files_ <- self$new_diagnostic_files()
      }
      invisible(self)
    },

    num_runs = function() self$procs$num_runs(),
    num_chains = function() self$num_runs(),
    num_cores = function() self$procs$num_cores(),
    run_ids = function() self$args$run_ids,
    exe_file = function() self$args$exe_file,
    model_name = function() self$args$model_name,
    method = function() self$args$method,
    csv_basename = function() self$args$csv_basename(),
    data_file = function() self$args$data_file,
    new_output_files = function() self$args$new_output_files(),
    new_diagnostic_files = function() self$args$new_diagnostic_files(),
    diagnostic_files = function() {
      if (!length(private$diagnostic_files_)) {
        stop(
          "No diagnostic files found. ",
          "Set 'save_diagnostics=TRUE' when fitting the model.",
          call. = FALSE
        )
      }
      private$diagnostic_files_
    },
    output_files = function() {
      # if we are using background processes only output the file if
      # the process finished normally
      ok <- self$procs$is_finished() | self$procs$is_queued()
      private$output_files_[ok]
    },
    save_output_files = function(dir = ".",
                                 basename = NULL,
                                 timestamp = TRUE,
                                 random = TRUE) {
      # FIXME use self$output_files(include_failed=TRUE) once #76 is fixed
      current_files <- private$output_files_
      new_paths <- copy_temp_files(
        current_paths = current_files,
        new_dir = dir,
        new_basename = basename %||% self$model_name(),
        ids = self$run_ids(),
        ext = ".csv",
        timestamp = timestamp,
        random = random
      )
      file.remove(current_files[!current_files %in% new_paths])
      private$output_files_ <- new_paths
      message("Moved ", length(current_files),
              " output files and set internal paths to new locations:\n",
              paste("-", new_paths, collapse = "\n"))
      invisible(new_paths)
    },
    save_diagnostic_files = function(dir = ".",
                                     basename = NULL,
                                     timestamp = TRUE,
                                     random = TRUE) {
      # FIXME use self$diagnostic_files(include_failed=TRUE) once #76 is fixed
      current_files <- self$diagnostic_files() # used so we get error if 0 files
      current_files <- private$diagnostic_files_ # used so we still save all of them
      new_paths <- copy_temp_files(
        current_paths = current_files,
        new_dir = dir,
        new_basename = paste0(basename %||% self$model_name(), "-diagnostic"),
        ids = self$run_ids(),
        ext = ".csv",
        timestamp = timestamp,
        random = random
      )
      file.remove(current_files[!current_files %in% new_paths])
      private$diagnostic_files_ <- new_paths
      message("Moved ", length(current_files),
              " diagnostic files and set internal paths to new locations:\n",
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
        private$command_args_ <- lapply(self$run_ids(), function(j) {
          self$args$compose_all_args(
            idx = j,
            output_file = private$output_files_[j],
            diagnostic_file = private$diagnostic_files_[j] # maybe NULL
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
      tool <- match.arg(tool)
      if (!length(self$output_files())) {
        stop("No CmdStan runs finished successfully. ",
             "Unable to run bin/", tool, ".", call. = FALSE)
      }
      target_exe = file.path("bin", cmdstan_ext(tool))
      check_target_exe(target_exe)
      run_log <- processx::run(
        command = target_exe,
        args = c(self$output_files(), flags),
        wd = cmdstan_path(),
        echo_cmd = TRUE,
        echo = TRUE,
        error_on_status = TRUE
      )
    },

    time = function() {
      if (self$method() != "sample") {
        # FIXME add time for other methods?
        return(NULL)
      }

      procs <- self$procs
      info <- procs$chain_info()
      info <- info[procs$is_finished(), ]
      chain_time <- data.frame(
        chain_id = info$id,
        warmup = info$warmup_time,
        sampling = info$sampling_time,
        total = info$total_time
      )

      if (isTRUE(self$args$refresh == 0)) {
        warning("Separate warmup and sampling times are not available ",
                "after running with 'refresh=0'.", call. = FALSE)
        chain_time$warmup <- NA_real_
        chain_time$sampling <- NA_real_
      }

      list(total = procs$total_time(), chains = chain_time)
    }
  ),
  private = list(
    output_files_ = character(),
    diagnostic_files_ = NULL,
    command_args_ = list()
  )
)


# run helpers -------------------------------------------------
.run_sample <- function() {
  procs <- self$procs
  on.exit(procs$cleanup(), add = TRUE)

  cat("Running MCMC with", procs$num_runs(), "chain(s) on", procs$num_cores(),
      "core(s)...\n\n")

  start_time <- Sys.time()
  chains <- procs$run_ids()
  chain_ind <- 1

  while (!procs$all_finished()) {

    # if we have free cores and any leftover chains
    while (procs$active_cores() != procs$num_cores() &&
           procs$any_queued()) {
      chain_id <- chains[chain_ind]
      procs$new_proc(
        id = chain_id,
        command = self$command(),
        args = self$command_args()[[chain_id]],
        wd = dirname(self$exe_file())
      )
      procs$mark_chain_start(chain_id)
      procs$set_active_cores(procs$active_cores() + 1)
      chain_ind <- chain_ind + 1
    }
    start_active_cores <- procs$active_cores()

    while (procs$active_cores() == start_active_cores &&
           procs$active_cores() > 0) {
      procs$wait(0.1)
      procs$poll(0)
      for (chain_id in chains) {
        if (!procs$is_queued(chain_id)) {
          output <- procs$get_proc(chain_id)$read_output_lines()
          procs$process_sample_output(output, chain_id)
        }
      }
      procs$set_active_cores(procs$num_alive())
    }
  }
  procs$set_total_time(Sys.time() - start_time)
  procs$report_time()
}
CmdStanRun$set("private", name = "run_sample_", value = .run_sample)

.run_other <- function() {
  # FIXME for consistency we should use a CmdStanProcs object
  # for optimize and variational too, but for now this is fine
  run_log <- processx::run(
    command = self$command(),
    args = self$command_args()[[1]],
    wd = dirname(self$exe_file()),
    echo_cmd = FALSE,
    echo = TRUE,
    error_on_status = TRUE
  )
}
CmdStanRun$set("private", name = "run_optimize_", value = .run_other)
CmdStanRun$set("private", name = "run_variational_", value = .run_other)


# CmdStanProcs ------------------------------------------------------------

# System processes for model fitting
# Currently only used to run sampling in parallel
CmdStanProcs <- R6::R6Class(
  classname = "CmdStanProcs",
  public = list(
    # @param num_runs The number of CmdStan runs. For MCMC this is the number of
    #   chains. Currently for other methods this must be set to 1.
    # @param num_cores The number of cores for running MCMC chains in parallel.
    #   Currently for other method this must be set to 1.
    initialize = function(num_runs, num_cores) {
      checkmate::assert_integerish(num_runs, lower = 1, len = 1, any.missing = FALSE)
      checkmate::assert_integerish(num_cores, lower = 1, len = 1, any.missing = FALSE)
      private$num_runs_ <- as.integer(num_runs)
      private$num_cores_ <- as.integer(num_cores)
      private$active_cores_ <- 0
      private$run_ids_ <- seq_len(num_runs)
      zeros <- rep(0, num_runs)
      private$chain_info_ <- data.frame(
        id = private$run_ids_,
        state = zeros,
        start_time = zeros,
        warmup_time = zeros,
        sampling_time = zeros,
        total_time = zeros,
        last_section_start_time = zeros
      )
      invisible(self)
    },
    num_runs = function() {
      private$num_runs_
    },
    num_cores = function() {
      private$num_cores_
    },
    run_ids = function() {
      private$run_ids_
    },
    cleanup = function() {
      lapply(private$processes_, function(p) p$kill_tree())
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
        echo_cmd = TRUE,
        stdout = "|",
        stderr = "|"
      )
      invisible(self)
    },
    active_cores = function() {
      private$active_cores_
    },
    set_active_cores = function(cores) {
      private$active_cores_ <- cores
      invisible(self)
    },
    total_chain_times = function() {
      # vector of total times (length is number of chains)
      info <- self$chain_info()
      info[self$is_finished(), "total_time"]
    },
    total_time = function() {
      # scalar overall time
      private$total_time_
    },
    set_total_time = function(time) {
      private$total_time_ <- as.numeric(time)
      invisible(self)
    },
    all_finished = function() {
      finished <- TRUE
      for (id in self$run_ids()) {
        # if chain is not finished yet
        if (self$is_still_working(id)) {
          if (!self$is_queued(id) && !self$is_alive(id)) {
            # if the chain just finished make sure we process all
            # input and mark the chain finished
            output <- self$get_proc(id)$read_output_lines()
            self$process_sample_output(output, id)
            self$mark_chain_stop(id)
          } else {
            finished <- FALSE
          }
        }
      }
      finished
    },
    is_alive = function(id = NULL) {
      if (!is.null(id)) {
        return(private$processes_[[id]]$is_alive())
      }
      sapply(private$processes_, function(x) x$is_alive())
    },
    is_still_working = function(id = NULL) {
      self$chain_state(id) < 5
    },
    is_finished = function(id = NULL) {
      self$chain_state(id) == 5
    },
    is_queued = function(id = NULL) {
      self$chain_state(id) == 0
    },
    num_alive = function() {
      sum(self$is_alive())
    },
    num_failed = function() {
      length(self$run_ids()) - sum(self$is_finished())
    },
    any_queued = function() {
      any(self$is_queued())
    },
    chain_info = function() {
      private$chain_info_
    },
    chain_output = function(id = NULL) {
      out <- private$chain_output_
      if (is.null(id)) {
        return(out)
      }
      out[[id]]
    },
    chain_state = function(id = NULL) {
      states <- self$chain_info()$state
      if (is.null(id)) {
        return(states)
      }
      states[id]
    },
    mark_chain_start = function(id) {
      id <- as.character(id)
      private$chain_info_[id,"start_time"] <- Sys.time()
      private$chain_info_[id,"state"] <- 1
      private$chain_info_[id,"last_section_start_time"] <- private$chain_info_[id,"start_time"]
      private$chain_output_[[id]] <- c("")
      invisible(self)
    },
    mark_chain_stop = function(id) {
      id <- as.character(id)
      if (private$chain_info_[id,"state"] == 4) {
        private$chain_info_[id,"state"] <- 5
        private$chain_info_[id,"total_time"] <- Sys.time() - private$chain_info_[id,"start_time"]
        self$report_time(id)
      } else {
        private$chain_info_[id,"state"] <- 6
        warning("Chain ", id, " finished unexpectedly!\n", immediate. = TRUE, call. = FALSE)
      }
      invisible(self)
    },
    is_error_message = function(line) {
      startsWith(line, "Exception:") ||
      (regexpr("either mistyped or misplaced.", line, perl = TRUE) > 0) ||
      (regexpr("A method must be specified!", line, perl = TRUE) > 0) ||
      (regexpr("is not a valid value for", line, perl = TRUE) > 0)
    },
    process_sample_output = function(out, id) {
      id <- as.character(id)
      if (length(out) == 0) {
        return(NULL)
      }
      for (line in out) {
        private$chain_output_[[id]] <- c(private$chain_output_[[id]], line)
        if (nzchar(line)) {
          last_section_start_time <- private$chain_info_[id,"last_section_start_time"]
          state <- private$chain_info_[id,"state"]
          next_state <- state
          if (state == 1 && regexpr("Iteration:", line, perl = TRUE) > 0) {
            state <- 2
            next_state <- 2
            private$chain_info_[id,"last_section_start_time"] <- Sys.time()
          }
          if (state == 1 && regexpr("Elapsed Time:", line, perl = TRUE) > 0) {
            state <- 4
            next_state <- 4
          }
          if (private$chain_info_[id,"state"] == 2 &&
              regexpr("(Sampling)", line, perl = TRUE) > 0) {
            next_state <- 3 # 3 = sampling
            private$chain_info_[id,"warmup_time"] <- Sys.time() - last_section_start_time
            private$chain_info_[id,"last_section_start_time"] <- Sys.time()
          }
          if (regexpr("\\[100%\\]", line, perl = TRUE) > 0) {
            if (state == 2) { #warmup only run
              private$chain_info_[id,"warmup_time"] <- Sys.time() - last_section_start_time
            } else if (state == 3) { # sampling
              private$chain_info_[id,"sampling_time"] <- Sys.time() - last_section_start_time
            }
            next_state <- 4 # writing csv and finishing
          }
          if (state > 1 && state < 4) {
            cat("Chain", id, line, "\n")
          }
          if (self$is_error_message(line)) {
            # will print all remaining output in case of excpetions
            if(state == 1) {
              state = 2;
            }
            message("Chain ", id, " ", line)
          }
          private$chain_info_[id,"state"] <- next_state
        }
      }
      invisible(self)
    },
    report_time = function(id = NULL) {
      if (!is.null(id)) {
        cat("Chain", id, "finished in",
            format(round(mean(self$chain_info()[id,"total_time"]), 1), nsmall = 1),
            "seconds.\n")
        return(invisible(self))
      }

      num_chains <- self$num_runs()
      if (num_chains > 1) {
        num_failed <- self$num_failed()
        if (num_failed == 0) {
          if (num_chains == 2) {
            cat("\nBoth chains finished succesfully.\n")
          } else {
            cat("\nAll", num_chains, "chains finished succesfully.\n")
          }
          cat("Mean chain execution time:",
              format(round(mean(self$total_chain_times()), 1), nsmall = 1),
              "seconds.\n")
          cat("Total execution time:",
              format(round(self$total_time(), 1), nsmall = 1),
              "seconds.\n")
        } else if (num_failed == num_chains) {
          warning("All chains finished unexpectedly!\n", call. = FALSE)
        } else {
          warning(num_failed, " chain(s) finished unexpectedly!",
                  immediate. = TRUE,
                  call. = FALSE)
          cat("The remaining chains had a mean execution time of",
              format(round(mean(self$total_time()), 1), nsmall = 1),
              "seconds.\n")
        }
      }
      invisible(self)
    }
  ),
  private = list(
    processes_ = NULL, # will be list of processx::process objects
    run_ids_ = integer(),
    num_runs_ = integer(),
    num_cores_ = integer(),
    active_cores_ = integer(),
    chain_info_ = data.frame(),
    chain_output_ = list(),
    total_time_ = numeric()
  )
)
