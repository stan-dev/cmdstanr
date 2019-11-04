# CmdStanMCMC -------------------------------------------------------------

#' CmdStanMCMC objects
#'
#' @name CmdStanMCMC
#' @family fitted model objects
#' @template seealso-docs
#'
#' @description A `CmdStanMCMC` object is the fitted model object returned by
#'   the [`sample()`][model-method-sample] method of a [`CmdStanModel`]
#'   object. Like `CmdStanModel` objects, `CmdStanMCMC` objects are [R6][R6::R6]
#'   objects.
#'
#' @details
#' `CmdStanMCMC` objects have the following associated methods:
#'
#' \tabular{ll}{
#'  **Method** \tab **Description** \cr
#'  `summary` \tab Run and print CmdStan's `bin/stansummary`. \cr
#'  `diagnose` \tab Run and print CmdStan's `bin/diagnose`. \cr
#'  `draws` \tab
#'    Return post-warmup draws as an `iters x chains x variables` array. \cr
#'  [`save_output_files`][fit-method-save_output_files]
#'    \tab Save output CSV files to a specified location. \cr
#'  [`save_data_file`][fit-method-save_data_file]
#'    \tab Save JSON data file to a specified location. \cr
#'  [`save_diagnostic_files`][fit-method-save_diagnostic_files]
#'    \tab Save diagnostic CSV files to a specified location. \cr
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

      # output csv files if diagnostic_file=TRUE
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
    output_files = function() private$output_files_,
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
    }
  ),
  private = list(
    args_ = NULL,
    num_runs_ = integer(),
    output_files_ = character(),
    diagnostic_files_ = character(),
    console_files_ = character(),
    command_args_ = list(),
    retcodes_ = integer()
  )
)
