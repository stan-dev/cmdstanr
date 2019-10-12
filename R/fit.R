# CmdStanMCMC -------------------------------------------------------------

#' CmdStanMCMC objects
#'
#' A `CmdStanMCMC` object is the fitted model object returned by the
#' [`sample()`][CmdStanModel-method-sample] method of a [`CmdStanModel`] object.
#' Like `CmdStanModel` objects, `CmdStanMCMC` objects are [R6][R6::R6] objects.
#'
#' @name CmdStanMCMC
#'
#' @section Available Methods: `CmdStanMCMC` objects have the following
#'   associated methods:
#' \describe{
#'   \item{`summary()`}{
#'   Run CmdStan's `bin/stansummary` on output csv files.
#'   }
#'   \item{`diagnose()`}{
#'   Run CmdStan's `bin/diagnose` on output csv files.
#'   }
#'   \item{`draws()`}{
#'   Return the posterior sample (post-warmup draws) as a 3-D array with
#'   dimensions ordered as `(iterations, chains, variables)`.
#'   }
#'   \item{`save_output_files(dir, basename = NULL)`}{
#'   Move csv output files from temporary directory to a specified directory
#'   `dir` using the provided file `basename`. The suffix `'-<chain_id>.csv'`
#'   is appended each file. If files with the specified names already exist they
#'   are overwritten.
#'   Arguments:
#'   * `dir`: Path to directory where the files should be saved.
#'   * `basename`: Base filename to use.
#'
#'   Return: the output from `base::file.copy()`, which is a logical vector
#'   indicating if the operation succeeded for each of the files.
#'   }
#'   \item{`save_data_file(dir, basename = NULL)`}{
#'   Same as `save_output_files()` but applies to the temporary file containing
#'   the data instead of the output csv files.
#'   }
#'   \item{More coming soon...}{}
#' }
#'
#' @seealso [`CmdStanModel`]
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
    print = function() {
      self$summary()
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
    sampler_params = function() {
      # currently sampler params list from rstan::get_sampler_params()
      # but this shouldn't use rstan
      if (is.null(private$sampler_params_)) private$read_csv()
      private$sampler_params_
    },
    output_files = function() {
      # get the paths to the temporary output csv files
      self$runset$output_files()
    },
    save_output_files = function(dir = ".", basename = NULL) {
      self$runset$save_output_files(dir, basename)
    },
    save_data_file = function(dir = ".", basename = NULL) {
      self$runset$save_data_file(dir, basename)
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
        stop("Please install the 'rstan' package.\n",
             "This is required for reading the csv files from CmdStan ",
             "until CmdStanR has its own implementation.",
             call. = FALSE)
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
#' A `CmdStanMLE` object is the fitted model object returned by the
#' [`optimize()`][CmdStanModel-method-optimize] method of a [`CmdStanModel`]
#' object.
#'
#' @name CmdStanMLE
#'
#' @section Available Methods: `CmdStanMLE` objects have the following
#'   associated methods:
#' \describe{
#'   \item{`mle()`}{
#'   Return the maximum likelihood estimate or estimated posterior mode.
#'   },
#'   \item{`lp()`}{
#'   Return the the total log probability density (up to an additive constant)
#'   computed in the model block of the Stan program.
#'   },
#'   \item{`save_output_files(dir, basename = NULL)`}{
#'   Move output csv file from temporary directory to a specified directory.
#'   }
#'   \item{`save_data_file(dir, basename = NULL)`}{
#'   Move data file from temporary directory to a specified directory.
#'   }
#'   \item{More coming soon...}{}
#' }
#'
#' @seealso [`CmdStanModel`]
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
    },
    save_output_files = function(dir = ".", basename = NULL) {
      self$runset$save_output_files(dir, basename)
    },
    save_data_file = function(dir = ".", basename = NULL) {
      self$runset$save_data_file(dir, basename)
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
#' A `CmdStanVB` object is the fitted model object returned by the
#' [`variational()`][CmdStanModel-method-variational] method of a
#' [`CmdStanModel`] object.
#'
#' @name CmdStanVB
#'
#' @section Available Methods: `CmdStanVB` objects have the following
#'   associated methods:
#' \describe{
#'   \item{`summary()`}{
#'   Run CmdStan's `bin/stansummary` on output csv file.
#'   }
#'   \item{`draws()`}{
#'   Return the draws from the approximate posterior as a matrix with one
#'   column per variable.
#'   }
#'   \item{`save_output_files(dir, basename = NULL)`}{
#'   Move output csv file from temporary directory to a specified directory.
#'   }
#'   \item{`save_data_file(dir, basename = NULL)`}{
#'   Move data file from temporary directory to a specified directory.
#'   }
#'   \item{More coming soon...}{}
#' }
#'
#' @seealso [`CmdStanModel`]
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
    },
    save_output_files = function(dir = ".", basename = NULL) {
      self$runset$save_output_files(dir, basename)
    },
    save_data_file = function(dir = ".", basename = NULL) {
      self$runset$save_data_file(dir, basename)
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
    # public but for internal use
    ._check_retcodes = function() all(private$retcodes_  == 0),
    ._retcode = function(idx) private$retcodes_[idx],
    ._set_retcode = function(idx, val) {
      private$retcodes_[idx] <- val
      invisible(self)
    },
    # ._check_console_msgs = function() {},
    # validate = function() {},

    save_output_files = function(dir = ".", basename = NULL) {
      copy_temp_files(
        current_paths = self$output_files(),
        new_dir = dir,
        new_basename = basename %||% self$model_name(),
        ids = self$run_ids(),
        ext = ".csv"
      )
    },
    save_data_file = function(dir = ".", basename = NULL) {
      copy_temp_files(
        current_paths = self$data_file(),
        new_dir = dir,
        new_basename = basename %||% self$model_name(),
        ids = NULL,
        ext = ".data.R"
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


