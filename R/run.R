# RunSet ---------------------------------------------------------------

# Record of CmdStan runs for a specified configuration and number of chains.
CmdStanRun <- R6::R6Class(
  classname = "CmdStanRun",
  public = list(
    # Initialize object
    # @param args CmdStanArgs object. See args.R.
    # @param num_runs The number of CmdStan runs. For MCMC this is the number of
    #   chains. Currently for other methods this must be set to 1.
    initialize = function(args, num_runs) {
      checkmate::assert_r6(args, classes = "CmdStanArgs")
      checkmate::assert_integerish(num_runs,
                                   any.missing = FALSE,
                                   null.ok = FALSE,
                                   len = 1,
                                   lower = 1)
      private$args_ <- args
      private$num_runs_ <- as.integer(num_runs)
      # diagnostic csv files if diagnostic_file=TRUE
      private$diagnostic_files_ <- NULL
      if (args$save_diagnostics) {
        private$diagnostic_files_ <-
          tempfile(
            pattern = paste0(args$csv_basename(), "-diagnostic-", args$run_ids, "-"),
            tmpdir = cmdstan_tempdir(),
            fileext = ".csv"
          )
        invisible(file.create(private$diagnostic_files_))
      }

      # output csv files
      private$output_files_ <-
        tempfile(
          pattern = paste0(args$csv_basename(), "-", args$run_ids, "-"),
          tmpdir = cmdstan_tempdir(),
          fileext = ".csv"
        )
      invisible(file.create(private$output_files_))

      # files to store console output (NOT USED CURRENTLY)
      private$console_files_ <- change_ext(private$output_files_, ".txt")
      invisible(file.create(private$console_files_))

      # create the commands to run each chain
      private$command_args_ <- lapply(args$run_ids, function(j) {
        args$compose_all_args(
          idx = j,
          output_file = private$output_files_[j],
          diagnostic_file = private$diagnostic_files_[j] # maybe NULL
        )
      })

      private$procs_ <- CmdStanProcs$new(self$run_ids())
      invisible(self)
    },

    args = function() private$args_,
    num_runs = function() private$num_runs_,
    num_chains = function() private$num_runs_,
    run_ids = function() private$args_$run_ids,
    model_name = function() private$args_$model_name,
    method = function() private$args_$method,
    command = function() private$args_$command(),
    command_args = function() private$command_args_,
    console_files = function() private$console_files_,
    data_file = function() private$args_$data_file,
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
      ok <- self$procs()$is_finished() | self$procs()$is_queued()
      private$output_files_[ok]
    },

    save_output_files = function(dir = ".",
                                 basename = NULL,
                                 timestamp = TRUE) {
      copy_temp_files(
        current_paths = self$output_files(),
        new_dir = dir,
        new_basename = basename %||% self$model_name(),
        ids = self$run_ids(),
        ext = ".csv",
        timestamp = timestamp
      )
    },
    save_diagnostic_files = function(dir = ".",
                                     basename = NULL,
                                     timestamp = TRUE) {
      copy_temp_files(
        current_paths = self$diagnostic_files(),
        new_dir = dir,
        new_basename = paste0(basename %||% self$model_name(), "-diagnostic"),
        ids = self$run_ids(),
        ext = ".csv",
        timestamp = timestamp
      )
    },
    save_data_file = function(dir = ".",
                              basename = NULL,
                              timestamp = TRUE) {
      copy_temp_files(
        current_paths = self$data_file(),
        new_dir = dir,
        new_basename = basename %||% self$model_name(),
        ids = NULL,
        ext = ".json",
        timestamp = timestamp
      )
    },

    procs = function() {
      private$procs_
    },
    time = function() {
      if (self$method() != "sample") {
        return(NULL)
      }

      procs <- self$procs()
      info <- procs$chain_info()
      info <- info[procs$is_finished(), ]
      chain_time <- data.frame(
        chain_id = info$id,
        warmup = info$warmup_time,
        sampling = info$sampling_time,
        total = info$total_time
      )

      if (isTRUE(self$args()$refresh == 0)) {
        warning("Separate warmup and sampling times are not available ",
                "after running with 'refresh=0'.", call. = FALSE)
        chain_time$warmup <- NA_real_
        chain_time$sampling <- NA_real_
      }

      list(total = procs$total_time(), chains = chain_time)
    }
  ),
  private = list(
    args_ = NULL,  # CmdStanArgs object
    procs_ = NULL, # CmdStanProcs object
    num_runs_ = integer(),
    output_files_ = character(),
    diagnostic_files_ = character(),
    console_files_ = character(),
    command_args_ = list()
  )
)


# CmdStanProcs ------------------------------------------------------------

# System processes for model fitting
# Currently only used to run sampling in parallel
CmdStanProcs <- R6::R6Class(
  classname = "CmdStanProcs",
  public = list(
    initialize = function(run_ids) {
      private$run_ids_ <- run_ids

      num_runs <- length(run_ids)
      zeros <- rep(0, num_runs)
      private$chain_info_ <- data.frame(
        id = seq_len(num_runs),
        state = zeros,
        start_time = zeros,
        warmup_time = zeros,
        sampling_time = zeros,
        total_time = zeros,
        last_section_start_time = zeros
      )
      private$active_cores_ <- 0

      invisible(self)
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
      (regexpr(line, "either mistyped or misplaced.") > 0) ||
      (regexpr(line, "A method must be specified!") > 0) ||
      (regexpr(line, "is not a valid value for") > 0)
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
          if (state == 1 && regexpr("Iteration:", line) > 0) {
            state <- 2
            next_state <- 2
            private$chain_info_[id,"last_section_start_time"] <- Sys.time()
          }
          if (state == 1 && regexpr("Elapsed Time:", line) > 0) {
            state <- 4
            next_state <- 4
          }
          if (private$chain_info_[id,"state"] == 2 &&
              regexpr("(Sampling)", line) > 0) {
            next_state <- 3 # 3 = sampling
            private$chain_info_[id,"warmup_time"] <- Sys.time() - last_section_start_time
            private$chain_info_[id,"last_section_start_time"] <- Sys.time()
          }
          if (regexpr("\\[100%\\]", line) > 0) {
            if (state == 2) { #warmup only run
              private$chain_info_[id,"warmup_time"] <- Sys.time() - last_section_start_time
            } else if (state == 3) { # sampling
              private$chain_info_[id,"sampling_time"] <- Sys.time() - last_section_start_time
            }
            next_state <- 4 # writing csv and finishing
          }
          if (state > 1 && state < 4) {
            cat("CHAIN ", id,": ", line, "\n")
          }
          if (self$is_error_message(line)) {
            # will print all remaining output in case of excpetions
            if(state == 1) {
              state = 2;
            }
            message("CHAIN ", id,": ", line)
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

      num_chains <- length(self$run_ids())
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
    active_cores_ = integer(),
    run_ids_ = integer(),
    chain_info_ = data.frame(),
    chain_output_ = list(),
    total_time_ = numeric()
  )
)
