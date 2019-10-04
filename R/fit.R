
## FIXME: currently using some stuff from RStan but don't want to depend on RStan

#' CmdStanMCMC objects
#'
#' A `CmdStanMCMC` object is returned by the `sample()` method of a
#' [`CmdStanModel`] object.
#'
#' @name CmdStanMCMC
#' @aliases cmdstanmcmc
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
#'   \item{`save_csvfiles(dir, basename)`} {
#'   Move csv output files to specified directory using specified basename,
#'   appending suffix ‘-<id>.csv’ to each. If files with the specified
#'   names already exist they are overwritten.
#'   Arguments:
#'   * `dir`: Path to directory where the files should be saved.
#'   * `basename`: Base filename to use.
#'
#'   Return: the output from `base::file.copy()`, which is a logical vector
#'   indicating if the operation succeeded for each of the files.
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
    cmdstan_args = NULL, # TODO: replace this with RunSet like cmdstanpy?
    output_files = character(),
    initialize = function(output_files, cmdstan_args) {
      checkmate::assert_character(output_files, pattern = ".csv")
      self$output_files <- output_files
      self$cmdstan_args <- cmdstan_args
    },
    save_csvfiles = function(dir, basename) {
      .save_csvfiles(self, dir, basename)
    },
    print = function() {
      self$summary()
    },
    summary = function() {
      # Run cmdstan's bin/stansummary on csv files
      run_log <- processx::run(
        command = file.path("bin", cmdstan_ext("stansummary")),
        args = self$output_files,
        wd = cmdstan_path(),
        echo_cmd = TRUE,
        echo = TRUE
      )
    },
    diagnose = function() {
      # Run cmdstan's bin/diagnose on csv files
      run_log <- processx::run(
        command = file.path("bin", cmdstan_ext("diagnose")),
        args = self$output_files,
        wd = cmdstan_path(),
        echo_cmd = TRUE,
        echo = TRUE
      )
    },
    get_drawset = function() {
      # See cmdstanpy
      stop("Not implemented yet.")
    },
    sample = function() {
      # iter x chains x params array
      if (is.null(private$posterior_sample_)) private$read_csv()
      private$posterior_sample_
    },
    sampler_params = function() {
      # currently sampler params list from rstan::get_sampler_params()
      # but this shouldn't use rstan
      if (is.null(private$sampler_params_)) private$read_csv()
      private$sampler_params_
    }
  ),
  private = list(
    posterior_sample_ = NULL,
    sampler_params_ = NULL,
    stanfit_ = NULL,
    read_csv = function() {
      if (!all(file.exists(self$output_files))) {
        stop("Can't find output file(s).", call. = FALSE)
      }

      # FIXME don't use rstan
      if (!requireNamespace("rstan", quietly = TRUE)) {
        stop("Please install the 'rstan' package.\n",
             "This is required for reading the csv files from CmdStan ",
             "until CmdStanR has its own implementation.",
             call. = FALSE)
      }
      stanfit <- rstan::read_stan_csv(self$output_files)
      private$posterior_sample_ <- # FIXME get save_warmup from CmdStanArgs
        rstan::extract(stanfit, permuted = FALSE, inc_warmup = FALSE)
      private$sampler_params_ <- rstan::get_sampler_params(private$stanfit,
                                                           inc_warmup = FALSE)
    }
  )
)

CmdStanMLE <- R6::R6Class(
  classname = "CmdStanMLE",
  public = list(
    cmdstan_args = NULL,
    output_files = character(),
    mle = NULL, # FIXME
    initialize = function(output_files, cmdstan_args) {
      checkmate::assert_character(output_files, pattern = ".csv")
      self$output_files <- output_files
      self$cmdstan_args <- cmdstan_args
      self$mle <- read_optim_csv(output_files)
    },
    save_csvfiles = function(dir, basename) {
      .save_csvfiles(self, dir, basename)
    }
  )
)

