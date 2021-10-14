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
#'
CmdStanRun <- R6::R6Class(
  classname = "CmdStanRun",
  public = list(
    args = NULL,
    procs = NULL,
    initialize = function(args, procs) {
      checkmate::assert_r6(args, classes = "CmdStanArgs")
      checkmate::assert_r6(procs, classes = "CmdStanProcs")
      self$args <- args
      self$procs <- procs
      private$output_files_ <- self$new_output_files()
      if (cmdstan_version() >= "2.26.0") {
        private$profile_files_ <- self$new_profile_files()
      }
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
    new_profile_files = function() {
      self$args$new_files(type = "profile")
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
    profile_files = function(include_failed = FALSE) {
      files <- private$profile_files_
      if (!length(files) || !any(file.exists(files))) {
        stop(
          "No profile files found. ",
          "The model that produced the fit did not use any profiling.",
          call. = FALSE
        )
      }
      if (include_failed) {
        files
      } else {
        ok <- self$procs$is_finished() | self$procs$is_queued()
        files[ok]
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
        ids = self$proc_ids(),
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
      current_files <- self$profile_files(include_failed = TRUE) # used so we get error if 0 files
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

    command = function() self$args$command(),
    command_args = function() {
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
      run_method <- paste0("run_", self$method(), "_")
      private[[run_method]]()
    },

    run_cmdstan_mpi = function(mpi_cmd, mpi_args) {
      private$run_sample_(mpi_cmd, mpi_args)
    },

    #' Run `bin/stansummary` or `bin/diagnose`
    #' @param tool The name of the tool in `bin/` to run.
    #' @param flags An optional character vector of flags (e.g. `c("--sig_figs=1")`).
    #' @noRd
    run_cmdstan_tool = function(tool = c("stansummary", "diagnose"), flags = NULL) {
      if (self$method() == "optimize") {
        stop("Not available for optimize method.", call. = FALSE)
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
      run_log <- processx::run(
        command = target_exe,
        args = c(self$output_files(include_failed = FALSE), flags),
        wd = cmdstan_path(),
        echo = TRUE,
        echo_cmd = is_verbose_mode(),
        error_on_status = TRUE
      )
    },

    time = function() {
      if (self$method() %in% c("optimize", "variational")) {
        time <- list(total = self$procs$total_time())
      } else if (self$method() == "generate_quantities") {
        chain_time <- data.frame(
          chain_id = self$procs$proc_ids()[self$procs$is_finished()],
          total = self$procs$proc_total_time()[self$procs$is_finished()]
        )

        time <- list(total = self$procs$total_time(), chains = chain_time)
      } else {
        chain_time <- data.frame(
          chain_id = self$procs$proc_ids()[self$procs$is_finished()],
          warmup = self$procs$proc_section_time("warmup")[self$procs$is_finished()],
          sampling = self$procs$proc_section_time("sampling")[self$procs$is_finished()],
          total = self$procs$proc_total_time()[self$procs$is_finished()]
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
    command_args_ = list(),

    finalize = function() {
      if (self$args$using_tempdir) {
        temp_files <- c(
          if (!private$output_files_saved_)
            self$output_files(include_failed = TRUE),
          if (self$args$save_latent_dynamics && !private$latent_dynamics_files_saved_)
            self$latent_dynamics_files(include_failed = TRUE),
          if (cmdstan_version() > "2.25.0" && !private$profile_files_saved_)
            private$profile_files_
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
    run_log <- processx::run(
      command = make_cmd(),
      args = exe,
      wd = cmdstan_path(),
      echo_cmd = TRUE,
      echo = TRUE,
      error_on_status = TRUE
    )
  }
}

.run_sample <- function(mpi_cmd = NULL, mpi_args = NULL) {
  procs <- self$procs
  on.exit(procs$cleanup(), add = TRUE)
  if (!is.null(mpi_cmd)) {
    if (is.null(mpi_args)) {
      mpi_args <- list()
    }
    mpi_args[["exe"]] <- self$exe_file()
  }
  check_tbb_path()
  if (procs$num_procs() == 1) {
    start_msg <- "Running MCMC with 1 chain"
  } else if (procs$num_procs() == procs$parallel_procs()) {
    start_msg <- paste0("Running MCMC with ", procs$num_procs(), " parallel chains")
  } else {
    if (procs$parallel_procs() == 1) {
      if (!is.null(mpi_cmd)) {
        if (!is.null(mpi_args[["n"]])) {
          mpi_n_process <- mpi_args[["n"]]
        } else if (!is.null(mpi_args[["np"]])) {
          mpi_n_process <- mpi_args[["np"]]
        }
        if (is.null(mpi_n_process)) {
          start_msg <- paste0("Running MCMC with ", procs$num_procs(), " chains using MPI")
        } else {
          start_msg <- paste0("Running MCMC with ", procs$num_procs(), " chains using MPI with ", mpi_n_process, " processes")
        }
      } else {
        start_msg <- paste0("Running MCMC with ", procs$num_procs(), " sequential chains")
      }
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
        wd = dirname(self$exe_file()),
        mpi_cmd = mpi_cmd,
        mpi_args = mpi_args
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
          procs$process_output(chain_id)
          procs$process_error_output(chain_id)
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

.run_generate_quantities <- function() {
  procs <- self$procs
  on.exit(procs$cleanup(), add = TRUE)
  check_tbb_path()
  if (procs$num_procs() == 1) {
    start_msg <- "Running standalone generated quantities after 1 MCMC chain"
  } else if (procs$num_procs() == procs$parallel_procs()) {
    start_msg <- paste0("Running standalone generated quantities after ", procs$num_procs(), " MCMC chains, all chains in parallel ")
  } else {
    if (procs$parallel_procs() == 1) {
      start_msg <- paste0("Running standalone generated quantities after ", procs$num_procs(), " MCMC chains, 1 chain at a time ")
    } else {
      start_msg <- paste0("Running standalone generated quantities after ", procs$num_procs(), " MCMC chains, ", procs$parallel_procs(), " chains at a time ")
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
          procs$process_output(chain_id)
          procs$process_error_output(chain_id)
        }
      }
      procs$set_active_procs(procs$num_alive())
    }
    procs$check_finished()
  }
  procs$set_total_time(as.double((Sys.time() - start_time), units = "secs"))
  procs$report_time()
}
CmdStanRun$set("private", name = "run_generate_quantities_", value = .run_generate_quantities)

.run_other <- function() {
  procs <- self$procs
  check_tbb_path()
  if (!is.null(procs$threads_per_proc())) {
    Sys.setenv("STAN_NUM_THREADS" = as.integer(procs$threads_per_proc()))
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
      procs$process_output(id)
      procs$process_error_output(id)
    }
    procs$set_active_procs(procs$num_alive())
  }
  procs$process_output(id)
  procs$process_error_output(id)
  successful_fit <- FALSE
  if (self$method() == "optimize") {
    if (procs$proc_state(id = id) > 3) {
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
CmdStanRun$set("private", name = "run_variational_", value = .run_other)

.run_diagnose <- function() {
  procs <- self$procs
  check_tbb_path()
  if (!is.null(procs$threads_per_proc())) {
    Sys.setenv("STAN_NUM_THREADS" = as.integer(procs$threads_per_proc()))
  }
  stdout_file <- tempfile()
  stderr_file <- tempfile()
  ret <- processx::run(
    command = self$command(),
    args = self$command_args()[[1]],
    wd = dirname(self$exe_file()),
    stderr = stderr_file,
    stdout = stdout_file,
    error_on_status = FALSE
  )
  if (is.na(ret$status) || ret$status != 0) {
    if (file.exists(stdout_file)) {
      cat(readLines(stdout_file), sep = "\n")
    }
    if (file.exists(stdout_file)) {
      cat(readLines(stdout_file), sep = "\n")
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
                          show_stderr_messages = TRUE,
                          show_stdout_messages = TRUE) {
      checkmate::assert_integerish(num_procs, lower = 1, len = 1, any.missing = FALSE)
      checkmate::assert_integerish(parallel_procs, lower = 1, len = 1, any.missing = FALSE, null.ok = TRUE)
      checkmate::assert_integerish(threads_per_proc, lower = 1, len = 1, null.ok = TRUE)
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
      private$proc_state_ <- zeros
      private$proc_start_time_ <- zeros
      private$proc_total_time_ <- zeros
      private$show_stderr_messages_ <- show_stderr_messages
      private$show_stdout_messages_ <- show_stdout_messages
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
    new_proc = function(id, command, args, wd, mpi_cmd = NULL, mpi_args = NULL) {
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
      private$processes_[[id]] <- processx::process$new(
        command = command,
        args = args,
        wd = wd,
        stdout = "|",
        stderr = "|",
        echo_cmd = is_verbose_mode()
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
          self$mark_proc_stop(id)
          self$report_time(id)
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
          if (private$show_stderr_messages_) {
            message("Chain ", id, " ", err_line)
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
      } else {
        cat("Finished in ",
            format(round(self$total_time(), 1), nsmall = 1),
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
    show_stdout_messages_ = TRUE
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
        if (nzchar(line)) {
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
          if (state < 3 && grepl("refresh =", line, perl = TRUE)) {
            state <- 1.5
            next_state <- 1.5
          }
          if (state <= 3 && grepl("Rejecting initial value:", line, perl = TRUE)) {
            state <- 2
            next_state <- 2
          }
          if (state < 3 && grepl("Iteration:", line, perl = TRUE)) {
            state <- 3 # 3 =  warmup
            next_state <- 3
          }
          if (state < 3 && grepl("Elapsed Time:", line, perl = TRUE)) {
            state <- 5 # 5 = end of sampling
            next_state <- 5
          }
          if (private$proc_state_[[id]] == 3 &&
              grepl("(Sampling)", line, perl = TRUE)) {
            next_state <- 4 # 4 = sampling
          }
          if (grepl("\\[100%\\]", line, perl = TRUE)) {
            next_state <- 5 # writing csv and finishing
          }
          if (grepl("seconds (Total)", line, fixed = TRUE)) {
            private$proc_total_time_[[id]] <- as.double(trimws(sub("seconds (Total)", "", line, fixed = TRUE)))
            next_state <- 5
            state <- 5
          }
          if (grepl("seconds (Sampling)", line, fixed = TRUE)) {
            private$proc_section_time_[id, "sampling"] <- as.double(trimws(sub("seconds (Sampling)", "", line, fixed = TRUE)))
            next_state <- 5
            state <- 5
          }
          if (grepl("seconds (Warm-up)", line, fixed = TRUE)) {
            private$proc_section_time_[id, "warmup"] <- as.double(trimws(sub("Elapsed Time: ", "", sub("seconds (Warm-up)", "", line, fixed = TRUE), fixed = TRUE)))
            next_state <- 5
            state <- 5
          }
          if (grepl("Gradient evaluation took", line, fixed = TRUE)
              || grepl("leapfrog steps per transition would take", line, fixed = TRUE)
              || grepl("Adjust your expectations accordingly!", line, fixed = TRUE)
              || grepl("stanc_version", line, fixed = TRUE)
              || grepl("stancflags", line, fixed = TRUE)) {
            ignore_line <- TRUE
          }
          if ((state > 1.5 && state < 5 && !ignore_line) || is_verbose_mode()) {
            if (state == 2) {
              message("Chain ", id, " ", line)
            } else {
              cat("Chain", id, line, "\n")
            }
          }
          if (self$is_error_message(line)) {
            # will print all remaining output in case of exceptions
            if (state == 1) {
              state <- 2;
            }
            message("Chain ", id, " ", line)
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
            warning("All chains finished unexpectedly! Use the $output(chain_id) method for more information.\n", call. = FALSE)
            warning("Use read_cmdstan_csv() to read the results of the failed chains.",
                    immediate. = TRUE,
                    call. = FALSE)
          } else {
            warning(num_failed, " chain(s) finished unexpectedly!",
                    immediate. = TRUE,
                    call. = FALSE)
            cat("The remaining chains had a mean execution time of",
                format(round(mean(self$total_time()), 1), nsmall = 1),
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
          if (self$get_proc(id)$get_exit_status() == 0) {
            self$set_proc_state(id = id, new_state = 5) # mark_proc_stop will mark this process successful
          } else {
            self$set_proc_state(id = id, new_state = 4) # mark_proc_stop will mark this process unsuccessful
          }
          self$mark_proc_stop(id)
          self$report_time(id)
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
        if (nzchar(line)) {
          if (self$proc_state(id) == 1 && grepl("refresh = ", line, perl = TRUE)) {
            self$set_proc_state(id, new_state = 1.5)
          } else if (self$proc_state(id) >= 2) {
            cat("Chain", id, line, "\n")
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
            warning("Use read_cmdstan_csv() to read the results of the failed chains.",
                    "Use $output(chain_id) on the fit object for more output of the failed chains.",
                    immediate. = TRUE,
                    call. = FALSE)
          } else {
            warning(num_failed, " chain(s) finished unexpectedly!",
                    immediate. = TRUE,
                    call. = FALSE)
            cat("The remaining chains had a mean execution time of",
                format(round(mean(self$total_time()), 1), nsmall = 1),
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

# add path to the TBB library to the PATH variable
check_tbb_path <- function() {
  if (cmdstan_version() >= "2.21" && os_is_windows()) {
    path_to_TBB <- file.path(cmdstan_path(), "stan", "lib", "stan_math", "lib", "tbb")
    current_path <- Sys.getenv("PATH")
    if (!grepl(path_to_TBB, current_path, perl = TRUE)) {
      Sys.setenv(PATH = paste0(path_to_TBB, ";", Sys.getenv("PATH")))
    }
  }
}
