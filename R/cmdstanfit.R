
## FIXME: currently using some stuff from RStan but don't want to depend on RStan

#' CmdStanFit objects
#'
#' @noRd
#' @param output_files The path(s) to the csv file(s) containing the output from
#'   CmdStan.
#'
#' The following methods are available:
#' \describe{
#'   \item{`summary()`}{
#'   Run CmdStan's `bin/stansummary`` on output csv files.
#'   }
#'   \item{`diagnose()`}{
#'   Run CmdStan's `bin/diagnose`` on output csv files.
#'   }
#' }
#'
CmdStanFit <- R6::R6Class(
  classname = "CmdStanFit",
  public = list(
    output_files = character(),
    initialize = function(output_files) {
      self$output_files <- output_files
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
      private$stanfit_ <- rstan::read_stan_csv(self$output_files)
      private$posterior_sample_ <- as.array(private$stanfit_)
      private$sampler_params_ <- rstan::get_sampler_params(private$stanfit_)
    }
  )
)

