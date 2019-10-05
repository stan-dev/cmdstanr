# CmdStanMCMC -------------------------------------------------------------

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
    args = NULL, # TODO: replace this with RunSet like cmdstanpy?
    output_files = NULL,
    initialize = function(output_files, args) {
      checkmate::assert_character(output_files, pattern = ".csv")
      checkmate::assert_r6(args, classes = "CmdStanArgs")
      self$output_files <- output_files
      self$args <- args
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
    },
    save_output_files = function(dir, basename = NULL) {
      .save_output_files(self, dir, basename)
    },
    save_data_file = function(dir, basename = NULL) {
      .save_data_file(self, dir, basename)
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


# CmdStanMLE -------------------------------------------------------------

#' CmdStanMLE objects
#'
#' A `CmdStanMLE` object is returned by the `optimize()` method of a
#' [`CmdStanModel`] object.
#'
#' @name CmdStanMLE
#' @aliases cmdstanmle
#'
#' @section Available Methods: `CmdStanMLE` objects have the following
#'   associated methods:
#' \describe{
#'   \item{`print()`}{
#'   Print the estimates.
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

CmdStanMLE <- R6::R6Class(
  classname = "CmdStanMLE",
  public = list(
    args = NULL,
    output_files = NULL,
    mle = NULL, # FIXME
    initialize = function(output_files, args) {
      checkmate::assert_character(output_files, pattern = ".csv")
      checkmate::assert_r6(args, classes = "CmdStanArgs")
      self$output_files <- output_files
      self$args <- args
      self$mle <- read_optim_csv(output_files)
    },
    save_output_files = function(dir, basename = NULL) {
      .save_output_files(self, dir, basename)
    },
    save_data_file = function(dir, basename = NULL) {
      .save_data_file(self, dir, basename)
    }
  )
)



# RunSet ------------------------------------------------------------------

# Record of CmdStan run for a specified configuration and number of chains.
RunSet <- R6::R6Class(
  classname = "RunSet",
  lock_objects = FALSE,
  public = list(
    args = NULL,
    initialize = function(args, num_chains) {
      checkmate::assert_r6(args, classes = "CmdStanArgs")
      checkmate::assert_integerish(num_chains,
                                   any.missing = FALSE,
                                   len = 1,
                                   lower = 1)
      self$args <- args
      self$num_chains <- num_chains

      csv_basename <- paste0("stan-", args$model_name, "-", args$method)
      private$csv_files <-
        file.path(
          cmdstan_tempdir(),
          paste0(csv_basename, "-", 1:num_chains, ".csv")
        )
      private$console_files <- change_ext(self$csv_files, ".txt")
      invisible(file.create(private$csv_files, private$console_files))

      private$commands <- lapply(1:self$num_chains, function(j) {
        self$args$compose_all_args(idx = j, csv_file = private$csv_files[j])
      })

      invisible(self)
    },
    validate = function() {
    }
  ),
  private = list(
    csv_files = character(),
    console_files = character(),
    commands = list()
  )
)


# internal ----------------------------------------------------------------

#' Copy temporary files to a different location
#'
#' Copies to specified directory using specified basename,
#' appending suffix `-id.ext` to each. If files with the specified
#' names already exist they are overwritten.
#'
#' @noRd
#' @param current_paths Paths to current temporary files.
#' @param new_dir Path to directory where the files should be saved.
#' @param new_basename Base filename to use.
#' @param ids Unique identifiers (e.g., `chain_ids`).
#' @param ext Extension to use for all saved files.
#' @return The output from `base::file.copy()`, which is a logical vector
#'   indicating if the operation succeeded for each of the files.
.copy_files <-
  function(current_paths,
           new_dir,
           new_basename,
           ids = NULL,
           ext = ".csv") {
    checkmate::assert_directory_exists(new_dir, access = "w")

    new_names <- new_basename
    if (!is.null(ids)) {
      new_names <- paste0(new_basename, "-", ids)
    }
    new_names <- paste0(new_names, ext)
    destinations <- file.path(new_dir, new_names)
    file.copy(from = current_paths,
              to = destinations,
              overwrite = TRUE)
  }

.save_output_files <- function(self, dir, basename) {
  .copy_files(
    current_paths = self$output_files,
    new_dir = dir,
    new_basename = basename %||% self$args$model_name,
    ids = self$args$chain_ids,
    ext = ".csv"
  )
}

.save_data_file <- function(self, dir, basename) {
  .copy_files(
    current_paths = self$args$data_file,
    new_dir = dir,
    new_basename = basename %||% self$args$model_name,
    ids = NULL,
    ext = ".data.R"
  )
}


