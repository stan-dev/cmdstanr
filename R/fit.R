
## FIXME: currently using some stuff from RStan but don't want to depend on RStan

#' CmdStanFit objects
#'
#' A `CmdStanFit` object is returned by the `sample()` method of a
#' [`CmdStanModel`] object.
#'
#' @name CmdStanFit
#' @aliases cmdstanfit
#'
#' @section Available Methods: `CmdStanFit` objects have the following
#'   associated methods:
#' \describe{
#'   \item{`summary()`}{
#'   Run CmdStan's `bin/stansummary` on output csv files.
#'   }
#'   \item{`diagnose()`}{
#'   Run CmdStan's `bin/diagnose` on output csv files.
#'   }
#'   \item{More coming soon...}{}
#' }
#'
#' @seealso [`CmdStanModel`]
#'
NULL

CmdStanFitMLE <- R6::R6Class(
  classname = "CmdStanFitMLE",
  public = list(
    cmdstan_args = NULL,
    output_files = character(),
    mle = NULL, # FIXME
    initialize = function(output_files, cmdstan_args) {
      checkmate::assert_character(output_files, pattern = ".csv")
      self$output_files <- output_files
      self$cmdstan_args <- cmdstan_args
      self$mle <- read_optim_csv(output_files)
    }
  )
)

CmdStanFit <- R6::R6Class(
  classname = "CmdStanFit",
  public = list(
    cmdstan_args = NULL, # TODO: replace this with RunSet like cmdstanpy?
    output_files = character(),
    initialize = function(output_files, cmdstan_args) {
      checkmate::assert_character(output_files, pattern = ".csv")
      self$output_files <- output_files
      self$cmdstan_args <- cmdstan_args
    },
    print = function() {
      self$summary()
    },
    summary = function() {
      # Run cmdstan's bin/stansummary on csv files
      cmd_path <- file.path(cmdstan_path(), "bin", cmdstan_ext("stansummary"))
      cmd <- paste(cmd_path, paste(self$output_files, collapse = " "))
      system(cmd)
    },
    diagnose = function() {
      # Run cmdstan's bin/diagnose on csv files
      cmd_path <- file.path(cmdstan_path(), "bin", cmdstan_ext("diagnose"))
      cmd <- paste(cmd_path, paste(self$output_files, collapse = " "))
      system(cmd)
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

