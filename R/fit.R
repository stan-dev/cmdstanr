# CmdStanMCMC -------------------------------------------------------------

#' CmdStanMCMC objects
#'
#' @name CmdStanMCMC
#' @family fitted model objects
#' @template seealso-docs
#'
#' @description A `CmdStanMCMC` object is the fitted model object returned by
#'   the [`$sample()`][model-method-sample] method of a [`CmdStanModel`] object.
#'   Like `CmdStanModel` objects, `CmdStanMCMC` objects are [R6][R6::R6]
#'   objects.
#'
#' @details
#' `CmdStanMCMC` objects have the following associated methods:
#'
#' \tabular{ll}{
#'  **Method** \tab **Description** \cr
#'  `summary` \tab Run and print CmdStan's `bin/stansummary`. \cr
#'  `diagnose` \tab Run and print CmdStan's `bin/diagnose`. \cr
#'  [`save_output_files`][fit-method-save_output_files]
#'    \tab Save output CSV files to a specified location. \cr
#'  [`save_data_file`][fit-method-save_data_file]
#'    \tab Save JSON data file to a specified location. \cr
#'  [`save_diagnostic_files`][fit-method-save_diagnostic_files]
#'    \tab Save diagnostic CSV files to a specified location. \cr
#'  `draws` \tab
#'    Return post-warmup draws as an `iters x chains x variables` array. \cr
#'  `time` \tab Return a data frame of execution times of all chains. \cr
#'  `output` \tab Return the stdout and stderr of all chains as a list of
#'    character vectors, or print the output for a single chain if
#'    `id` argument is specified. \cr
#' }
#'
NULL

CmdStanMCMC <- R6::R6Class(
  classname = "CmdStanMCMC",
  public = list(
    runset =  NULL,
    initialize = function(runset) {
      checkmate::assert_r6(runset, classes = "RunSet")
      self$runset <- runset
      invisible(self)
    },
    summary = function() {
      # Run cmdstan's bin/stansummary on csv files
      if (length(self$output_files()) == 0) {
        stop("No chains finished successfully. Unable to run summary()!")
      }
      target_exe = file.path("bin", cmdstan_ext("stansummary"))
      check_target_exe(target_exe)
      run_log <- processx::run(
        command = target_exe,
        args = self$output_files(),
        wd = cmdstan_path(),
        echo_cmd = TRUE,
        echo = TRUE
      )
    },
    diagnose = function() {
      # Run cmdstan's bin/diagnose on csv files
      if (length(self$output_files()) == 0) {
        stop("No chains finished successfully. Unable to run diagnose()")
      }
      target_exe = file.path("bin", cmdstan_ext("diagnose"))
      check_target_exe(target_exe)
      run_log <- processx::run(
        command = target_exe,
        args = self$output_files(),
        wd = cmdstan_path(),
        echo_cmd = TRUE,
        echo = TRUE
      )
    },
    draws = function() {
      # iter x chains x params array
      if (is.null(private$draws_)) private$read_csv()
      private$draws_
    },
    time = function() {
      self$runset$time()
    },
    output = function(id = NULL) {
      if (is.null(id)) {
        self$runset$output()
      } else {
        cat(paste(self$runset$output()[[id]], collapse="\n"))
      }
    }
    # sampler_params = function() {
    #   # currently sampler params list from rstan::get_sampler_params()
    #   # but this shouldn't use rstan
    #   if (is.null(private$sampler_params_)) private$read_csv()
    #   private$sampler_params_
    # }
  ),
  private = list(
    draws_ = NULL,
    sampler_params_ = NULL,
    stanfit_ = NULL,
    read_csv = function() {
      if (!all(file.exists(self$output_files()))) {
        stop("Can't find output file(s).", call. = FALSE)
      }
      if(length(self$output_files()) == 0) {
        stop("No chains finished successfully. Unable to retrieve the fit.", call. = FALSE)
      }
      # FIXME don't use rstan
      if (!requireNamespace("rstan", quietly = TRUE)) {
        stop("Please install the 'rstan' package. This is temporarily required ",
             "for reading the csv files from CmdStan.", call. = FALSE)
      }
      stanfit <- rstan::read_stan_csv(self$output_files())
      private$draws_ <-
        rstan::extract(stanfit, permuted = FALSE, inc_warmup = FALSE)
      private$sampler_params_ <-
        rstan::get_sampler_params(stanfit, inc_warmup = FALSE)
    }
  )
)


# CmdStanMLE -------------------------------------------------------------

#' CmdStanMLE objects
#'
#' @name CmdStanMLE
#' @family fitted model objects
#' @template seealso-docs
#'
#' @description A `CmdStanMLE` object is the fitted model object returned by the
#'   [`$optimize()`][model-method-optimize] method of a [`CmdStanModel`]
#'   object.
#'
#' @details
#' `CmdStanMLE` objects have the following associated methods:
#'
#' \tabular{ll}{
#'  **Method** \tab **Description** \cr
#'  `mle` \tab Return the MLE (or posterior mode) as a named vector. \cr
#'  `lp` \tab Return the the total log probability density (up to an additive
#'    constant) computed in the model block of the Stan program. \cr
#'  [`save_output_files`][fit-method-save_output_files]
#'    \tab Save output CSV files to a specified location. \cr
#'  [`save_data_file`][fit-method-save_data_file]
#'    \tab Save JSON data file to a specified location. \cr
#' }
#'
NULL

CmdStanMLE <- R6::R6Class(
  classname = "CmdStanMLE",
  public = list(
    runset = NULL,
    initialize = function(runset) {
      checkmate::assert_r6(runset, classes = "RunSet")
      self$runset <- runset
      invisible(self)
    },
    summary = function() {
      # FIXME: what should summary for optimization do?
      # (bin/stansummary isn't compatible)

      cat("Estimates from optimization:\n")
      c(self$mle(), self$lp())
    },
    mle = function() {
      if (is.null(private$mle_)) private$read_csv()
      private$mle_
    },
    lp = function() {
      if (is.null(private$lp_)) private$read_csv()
      private$lp_
    }
  ),
  private = list(
    mle_ = NULL,
    lp_ = NULL,
    read_csv = function() {
      optim_output <- read_optim_csv(self$output_files())
      private$mle_ <- optim_output[["mle"]]
      private$lp_ <- optim_output[["lp"]]
    }
  )
)

# CmdStanVB ---------------------------------------------------------------

#' CmdStanVB objects
#'
#' @name CmdStanVB
#' @family fitted model objects
#' @template seealso-docs
#'
#' @description A `CmdStanVB` object is the fitted model object returned by the
#'   [`$variational()`][model-method-variational] method of a
#'   [`CmdStanModel`] object.
#'
#' @details
#' `CmdStanVB` objects have the following associated methods:
#'
#' \tabular{ll}{
#'  **Method** \tab **Description** \cr
#'  `summary` \tab Run and print CmdStan's `bin/stansummary`. \cr
#'  `draws` \tab Return approximate posterior draws as matrix with one colunm per
#'    variable. \cr
#'  [`save_output_files`][fit-method-save_output_files]
#'    \tab Save output CSV files to a specified location. \cr
#'  [`save_data_file`][fit-method-save_data_file]
#'    \tab Save JSON data file to a specified location. \cr
#'  [`save_diagnostic_files`][fit-method-save_diagnostic_files]
#'    \tab Save diagnostic CSV files to a specified location. \cr
#' }
#'
NULL

CmdStanVB <- R6::R6Class(
  classname = "CmdStanVB",
  public = list(
    runset = NULL,
    initialize = function(runset) {
      checkmate::assert_r6(runset, classes = "RunSet")
      self$runset <- runset
      invisible(self)
    },
    summary = function() {
      # Run cmdstan's bin/stansummary on csv files
      target_exe = file.path("bin", cmdstan_ext("stansummary"))
      check_target_exe(target_exe)
      run_log <- processx::run(
        command = target_exe,
        args = self$output_files(),
        wd = cmdstan_path(),
        echo_cmd = TRUE,
        echo = TRUE
      )
    },
    draws = function() {
      # iter x params array
      if (is.null(private$draws_)) private$read_csv()
      private$draws_
    },
    log_p = function() {
      # iter x params array
      if (is.null(private$log_p_)) private$read_csv()
      private$log_p_
    },
    log_g = function() {
      # iter x params array
      if (is.null(private$log_g_)) private$read_csv()
      private$log_g_
    }
  ),
  private = list(
    draws_ = NULL,
    log_p_ = NULL,
    log_g_ = NULL,
    read_csv = function() {
      if (!all(file.exists(self$output_files()))) {
        stop("Can't find output file(s).", call. = FALSE)
      }

      vb_output <- read_vb_csv(self$output_files())
      private$draws_ <- vb_output[["draws"]]
      private$log_p_ <- vb_output[["log_p"]]
      private$log_g_ <- vb_output[["log_g"]]
    }
  )
)

# CmdStanGQ ---------------------------------------------------------------
# CmdStanGQ <- R6::R6Class(
#   classname = "CmdStanGQ"
# )

# Shared methods ----------------------------------------------------------

#' Save output and data files
#'
#' @name fit-method-save_output_files
#' @aliases fit-method-save_data_file fit-method-save_diagnostic_files
#'   fit-method-output_files fit-method-data_file fit-method-diagnostic_files
#'
#' @description All fitted model objects have methods for saving (copying to a
#'   specified location) the temporary files created by CmdStanR for CmdStan
#'   output and data files. These methods move output files or data files from
#'   the CmdStanR temporary directory to a user-specified location. By default
#'   the suffix `'-<run_id>_<timestamp>'` is added to the file name(s), where
#'   `run_id` is the chain number if applicable (MCMC only) and `1` otherwise.
#'   If files with the specified names already exist they are overwritten, but
#'   this shouldn't occur unless the `timestamp` argument has been intentionally
#'   set to `FALSE`.
#'
#'   If necessary, the versions without the `save_` prefix (e.g.,
#'   `$output_files()`) can be used to get the path(s) to the temporary file(s)
#'   themselves.
#'
#' @section Usage:
#'   ```
#'   $save_output_files(dir = ".", basename = NULL, timestamp = TRUE)
#'   $save_data_file(dir = ".", basename = NULL, timestamp = TRUE)
#'   $save_diagnostic_files(dir = ".", basename = NULL, timestamp = TRUE)
#'
#'   $output_files()
#'   $data_file()
#'   $diagnostic_files()
#'   ```
#'
#' @section Arguments:
#' * `dir`: (string) Path to directory where the files should be saved.
#' * `basename`: (string) Base filename to use.
#' * `timestamp`: (logical) Should a timestamp be added to the file name(s)?
#'   Defaults to `TRUE`. The timestamp is preceeded by an underscore is of
#'   the form
#'
#' @section Value: For the `$save_*` methods, the paths to the new files or `NA`
#'   for any that couldn't be copied. For the methods without the `save_`
#'   prefix, the path to the temporary files.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`]
#'
NULL

output_files_method <- function() {
  self$runset$output_files()
}
diagnostic_files_method = function() {
  self$runset$diagnostic_files()
}
data_file_method <- function() {
  self$runset$data_file()
}
CmdStanMCMC$set("public", "output_files", output_files_method)
CmdStanMLE$set("public", "output_files", output_files_method)
CmdStanVB$set("public", "output_files", output_files_method)
CmdStanMCMC$set("public", "diagnostic_files", diagnostic_files_method)
CmdStanMLE$set("public", "diagnostic_files", diagnostic_files_method)
CmdStanVB$set("public", "diagnostic_files", diagnostic_files_method)
CmdStanMCMC$set("public", "data_file", data_file_method)
CmdStanMLE$set("public", "data_file", data_file_method)
CmdStanVB$set("public", "data_file", data_file_method)

save_output_files_method <- function(dir = ".", basename = NULL, timestamp = TRUE) {
  self$runset$save_output_files(dir, basename, timestamp)
}
save_diagnostic_files_method <- function(dir = ".", basename = NULL, timestamp = TRUE) {
  self$runset$save_diagnostic_files(dir, basename, timestamp)
}
save_data_file_method = function(dir = ".", basename = NULL, timestamp = TRUE) {
  self$runset$save_data_file(dir, basename, timestamp)
}
CmdStanMCMC$set("public", "save_output_files", save_output_files_method)
CmdStanMLE$set("public", "save_output_files", save_output_files_method)
CmdStanVB$set("public", "save_output_files", save_output_files_method)
CmdStanMCMC$set("public", "save_diagnostic_files", save_diagnostic_files_method)
CmdStanMLE$set("public", "save_diagnostic_files", save_diagnostic_files_method)
CmdStanVB$set("public", "save_diagnostic_files", save_diagnostic_files_method)
CmdStanMCMC$set("public", "save_data_file", save_data_file_method)
CmdStanMLE$set("public", "save_data_file", save_data_file_method)
CmdStanVB$set("public", "save_data_file", save_data_file_method)


# RunSet ---------------------------------------------------------------

# Record of CmdStan runs for a specified configuration and number of chains.
RunSet <- R6::R6Class(
  classname = "RunSet",
  public = list(
    # Initialize object
    # @param args CmdStanArgs object.
    # @param num_runs The number of CmdStan runs. For MCMC this is the number of
    #   chains. For optimization this must be set to 1.
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
      private$retcodes_ <- rep(-1L, num_runs)

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
      chain_finished <- sapply(strsplit(private$output_files_, "-"), function(x) {
        # if we are using background processes only output the file if
        # the process finished normally
        private$chain_info_[as.integer(x[4]), "state"] == 0 ||
        private$chain_info_[as.integer(x[4]), "state"] == 5
      })
      private$output_files_[chain_finished]
    },
    # ._check_retcodes = function() all(private$retcodes_  == 0),
    # ._retcode = function(idx) private$retcodes_[idx],
    # ._set_retcode = function(idx, val) {
    #   private$retcodes_[idx] <- val
    #   invisible(self)
    # },
    # ._check_console_msgs = function() {},
    # validate = function() {},

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
    process_sample_output = function(out, id) {
      id <- as.character(id)
      if (length(out) == 0) {
        return(NULL)
      }
      for (line in out) {
        private$chain_output_[[id]] <- c(private$chain_output_[[id]], line)
        if (nzchar(line)) {
          last_secion_start_time <- private$chain_info_[id,"last_section_start_time"]
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
          if (private$chain_info_[id,"state"] == 2 && regexpr("(Sampling)", line) > 0) {
            next_state <- 3 # 3 = sampling
            private$chain_info_[id,"warmup_time"] <- Sys.time() - last_secion_start_time
            private$chain_info_[id,"last_section_start_time"] <- Sys.time()
          }
          if (regexpr("\\[100%\\]", line) > 0) {
            if (state == 2) { #warmup only run
              private$chain_info_[id,"warmup_time"] <- Sys.time() - last_secion_start_time
            } else if (state == 3) { # sampling
              private$chain_info_[id,"sampling_time"] <- Sys.time() - last_secion_start_time
            }
            next_state <- 4 # writing csv and finishing
          }
          if ((state > 1 && state < 4) ||
              (state == 1 && startsWith(line, "Exception:"))) {
            cat(paste0("CHAIN ", id,": ", line, "\n"))
          }
          private$chain_info_[id,"state"] <- next_state
        }
      }
    },
    mark_chain_start = function(id) {
      id <- as.character(id)
      private$chain_info_[id,"start_time"] <- Sys.time()
      private$chain_info_[id,"state"] <- 1
      private$chain_info_[id,"last_section_start_time"] <- private$chain_info_[id,"start_time"]
      private$chain_output_[[id]] <- c("")
    },
    mark_chain_stop = function(id) {
      id <- as.character(id)
      if (private$chain_info_[id,"state"] == 4) {
        private$chain_info_[id,"state"] <- 5
        private$chain_info_[id,"total_time"] <- Sys.time() - private$chain_info_[id,"start_time"]
        cat("Chain", id, "finished in",
            format(round(mean(private$chain_info_[id,"total_time"]), 1), nsmall = 1),
            "seconds.\n")
      } else {
        private$chain_info_[id,"state"] <- 6
        warning("Chain ", id, " finished unexpectedly!\n", immediate. = TRUE, call. = FALSE)
      }
    },
    chain_state = function(id = NULL) {
      if (is.null(id)) {
        private$chain_info_$state
      } else {
        private$chain_info_[id,"state"]
      }
    },
    time = function() {
      info <- private$chain_info_[private$chain_info_$state==5,]
      chain_time <- data.frame(chain_id = info$id,
                 warmup_time = info$warmup_time,
                 sampling_time = info$sampling_time,
                 total_time = info$total_time)

      list(total_time = private$total_time_, chain_time = chain_time)
    },
    output = function() {
      private$chain_output_
    },
    all_chains_finished = function() {
      all_finished <- TRUE
      for (id in self$run_ids()) {
        # if chain is not finished yet
        if (self$chain_state(id) < 5) {
          if (self$chain_state(id) > 0 && !private$procs_[[id]]$is_alive()) {
            # if the chain just finished make sure we process all
            # input and mark the chain finished
            output <- private$procs_[[id]]$read_output_lines()
            self$process_sample_output(output, id)
            self$mark_chain_stop(id)
          } else {
            all_finished <- FALSE
          }
        }
      }
      all_finished
    },
    any_chains_queued = function() {
      any(sapply(self$run_ids(), function(x) self$chain_state(x) == 0))
    },
    procs = function(id = NULL,
                     proc = NULL) {
      if (is.null(id)) {
        private$procs_
      } else {
        private$procs_[[id]] <- proc
      }
    },
    active_cores = function(num = NULL) {
      if (is.null(num)) {
        private$active_cores_
      } else {
        private$active_cores_ = num
      }
    },
    num_of_running_chains = function() {
      num <- sum(sapply(private$procs_, function(x) x$is_alive()))
    },
    set_total_time = function(time) {
      private$total_time_ = as.numeric(time)
    }
  ),
  private = list(
    args_ = NULL,
    num_runs_ = integer(),
    output_files_ = character(),
    diagnostic_files_ = character(),
    console_files_ = character(),
    command_args_ = list(),
    retcodes_ = integer(),
    chain_info_ = NULL,
    chain_output_ = list(),
    procs_ = list(),
    active_cores_ = NULL,
    total_time_ = NULL
  )
)
