# CmdStanMCMC -------------------------------------------------------------

#' CmdStanMCMC objects
#'
#' @name CmdStanMCMC
#' @family CmdStanFit objects
#' @template seealso-website
#'
#' @description A `CmdStanMCMC` object is the fitted model object returned by
#'   the [`sample()`][CmdStanModel-method-sample] method of a [`CmdStanModel`]
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
#'  [`save_output_files`][CmdStanFit-method-save_output_files]
#'    \tab Save output csv files to a specified location. \cr
#'  [`save_data_file`][CmdStanFit-method-save_data_file]
#'    \tab Save R dump or JSON data file to a specified location. \cr
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
      run_log <- processx::run(
        command = file.path("bin", cmdstan_ext("stansummary")),
        args = self$output_files(),
        wd = cmdstan_path(),
        echo_cmd = TRUE,
        echo = TRUE
      )
    },
    diagnose = function() {
      # Run cmdstan's bin/diagnose on csv files
      run_log <- processx::run(
        command = file.path("bin", cmdstan_ext("diagnose")),
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
    # sampler_params = function() {
    #   # currently sampler params list from rstan::get_sampler_params()
    #   # but this shouldn't use rstan
    #   if (is.null(private$sampler_params_)) private$read_csv()
    #   private$sampler_params_
    # },
    output_files = function() {
      # get the paths to the temporary output csv files
      self$runset$output_files()
    }
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
#' @family CmdStanFit objects
#' @template seealso-website
#'
#' @description A `CmdStanMLE` object is the fitted model object returned by the
#'   [`optimize()`][CmdStanModel-method-optimize] method of a [`CmdStanModel`]
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
#'  [`save_output_files`][CmdStanFit-method-save_output_files]
#'    \tab Save output csv files to a specified location. \cr
#'  [`save_data_file`][CmdStanFit-method-save_data_file]
#'    \tab Save R dump or JSON data file to a specified location. \cr
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
    mle = function() {
      if (is.null(private$mle_)) private$read_csv()
      private$mle_
    },
    lp = function() {
      if (is.null(private$lp_)) private$read_csv()
      private$lp_
    },
    output_files = function() {
      self$runset$output_files()
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
#' @family CmdStanFit objects
#' @template seealso-website
#'
#' @description A `CmdStanVB` object is the fitted model object returned by the
#'   [`variational()`][CmdStanModel-method-variational] method of a
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
#'  [`save_output_files`][CmdStanFit-method-save_output_files]
#'    \tab Save output csv files to a specified location. \cr
#'  [`save_data_file`][CmdStanFit-method-save_data_file]
#'    \tab Save R dump or JSON data file to a specified location. \cr
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
      run_log <- processx::run(
        command = file.path("bin", cmdstan_ext("stansummary")),
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
    },
    output_files = function() {
      self$runset$output_files()
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
#' @name CmdStanFit-method-save_output_files
#' @aliases CmdStanFit-method-save_data_file
#'
#' @description All fitted model objects have methods `save_output_files()` and
#'   `save_data_file()`. These methods move csv output files and R dump or json
#'   data files from the CmdStanR temporary directory to a user-specified
#'   location. By default the suffix `'-<run_id>_<timestamp>'` is added to the
#'   file name(s), where `run_id` is the chain number if applicable (MCMC only)
#'   and `1` otherwise. If files with the specified names already exist they are
#'   overwritten, but this shouldn't occur unless the `timestamp` argument has
#'   been intentionally set to `FALSE`.
#'
#' @section Usage:
#'   ```
#'   $save_output_files(dir = ".", basename = NULL, timestamp = TRUE)
#'   $save_data_file(dir = ".", basename = NULL, timestamp = TRUE)
#'   ```
#'
#' @section Arguments: `save_output_files()` and `save_data_file()` have the
#'   same arguments:
#' * `dir`: (string) Path to directory where the files should be saved.
#' * `basename`: (string) Base filename to use.
#' * `timestamp`: (logical) Should a timestamp be added to the file name(s)?
#'   Defaults to `TRUE`.
#'
#' @section Value: The paths to the new files or `NA` for any that couldn't be
#'   copied.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`]
#'
NULL

save_output_files_method <- function(dir = ".", basename = NULL, timestamp = TRUE) {
  self$runset$save_output_files(dir, basename, timestamp)
}
save_data_file_method = function(dir = ".", basename = NULL, timestamp = TRUE) {
  self$runset$save_data_file(dir, basename, timestamp)
}
CmdStanMCMC$set("public", "save_output_files", save_output_files_method)
CmdStanMLE$set("public", "save_output_files", save_output_files_method)
CmdStanVB$set("public", "save_output_files", save_output_files_method)
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

      private$output_files_ <-
        file.path(
          cmdstan_tempdir(),
          paste0(args$csv_basename(), "-", args$run_ids, ".csv")
        )
      private$console_files_ <- change_ext(private$output_files_, ".txt")
      private$commands_ <- lapply(args$run_ids, function(j) {
        args$compose_all_args(idx = j, output_file = private$output_files_[j])
      })
      private$retcodes_ <- rep(-1L, num_runs)

      invisible(file.create(private$output_files_, private$console_files_))
      invisible(self)
    },
    args = function() private$args_,
    num_runs = function() private$num_runs_,
    num_chains = function() private$num_runs_,
    run_ids = function() private$args_$run_ids,
    model_name = function() private$args_$model_name,
    method = function() private$args_$method,
    commands = function() private$commands_,
    data_file = function() private$args_$data_file,
    output_files = function() private$output_files_,
    console_files = function() private$console_files_,

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
    save_data_file = function(dir = ".",
                              basename = NULL,
                              timestamp = TRUE) {
      copy_temp_files(
        current_paths = self$data_file(),
        new_dir = dir,
        new_basename = basename %||% self$model_name(),
        ids = NULL,
        ext = ".data.R",
        timestamp = timestamp
      )
    }
  ),
  private = list(
    args_ = NULL,
    num_runs_ = integer(),
    output_files_ = character(),
    console_files_ = character(),
    commands_ = list(),
    retcodes_ = integer()
  )
)
