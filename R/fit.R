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
#'  `draws` \tab Return post-warmup draws as a
#'    [`draws_array`][posterior::draws_array] object.\cr
#'  [`summary`][fit-method-summary]
#'    \tab Run [posterior::summarise_draws()]. \cr
#'  [`cmdstan_summary`][fit-method-cmdstan_summary]
#'    \tab Run and print CmdStan's `bin/stansummary`. \cr
#'  [`cmdstan_diagnose`][fit-method-cmdstan_summary]
#'    \tab Run and print CmdStan's `bin/diagnose`. \cr
#'  [`save_output_files`][fit-method-save_output_files]
#'    \tab Save output CSV files to a specified location. \cr
#'  [`save_data_file`][fit-method-save_data_file]
#'    \tab Save JSON data file to a specified location. \cr
#'  [`save_diagnostic_files`][fit-method-save_diagnostic_files]
#'    \tab Save diagnostic CSV files to a specified location. \cr
#'  `time` \tab Return a list containing the total time and a data frame of
#'    execution times of all chains. \cr
#'  `output` \tab Return the stdout and stderr of all chains as a list of
#'    character vectors, or pretty print the output for a single chain if
#'    `id` argument is specified. \cr
#' }
#'
NULL

CmdStanMCMC <- R6::R6Class(
  classname = "CmdStanMCMC",
  public = list(
    runset =  NULL,
    initialize = function(runset) {
      checkmate::assert_r6(runset, classes = "CmdStanRun")
      self$runset <- runset
      invisible(self)
    },
    draws = function() {
      # iter x chains x params array
      if (is.null(private$draws_)) {
        private$read_csv()
      }
      posterior::as_draws_array(private$draws_)
    },
    time = function() {
      self$runset$time()
    },
    output = function(id = NULL) {
      if (is.null(id)) {
        self$runset$procs$chain_output()
      } else {
        cat(paste(self$runset$procs$chain_output(id), collapse="\n"))
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
      if (!length(self$output_files())) {
        stop("No chains finished successfully. Unable to retrieve the fit.",
             call. = FALSE)
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
      checkmate::assert_r6(runset, classes = "CmdStanRun")
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
#'  `draws` \tab
#'    Return approximate posterior draws as a
#'    [`draws_matrix`][posterior::draws_matrix] object. \cr
#'  [`summary`][fit-method-summary]
#'    \tab Run [posterior::summarise_draws()]. \cr
#'  [`cmdstan_summary`][fit-method-cmdstan_summary]
#'    \tab Run and print CmdStan's `bin/stansummary`. \cr
#'  [`cmdstan_diagnose`][fit-method-cmdstan_diagnose]
#'    \tab Run and print CmdStan's `bin/diagnose`. \cr
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
      checkmate::assert_r6(runset, classes = "CmdStanRun")
      self$runset <- runset
      invisible(self)
    },
    draws = function() {
      # iter x params array
      if (is.null(private$draws_)) {
        private$read_csv()
      }
      posterior::as_draws_matrix(private$draws_)
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

#' Run `posterior::summarise_draws()`
#'
#' @name fit-method-summary
#' @description Run [posterior::summarise_draws()] from the \pkg{posterior}
#'   package.
#'
#' @section Usage:
#'   ```
#'   $summary(...)
#'   ```
#' @section Arguments:
#' * `...`: Arguments to pass to [posterior::summarise_draws()].
#'
#' @section Value:
#' The data frame returned by [posterior::summarise_draws()].
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`]
#'
NULL

summary_method <- function(...) {
  if (self$runset$method() == "sample") {
    summary <- posterior::summarise_draws(self$draws(), ...)
  } else { # don't include MCMC diagnostics for non MCMC
    args <- list(...)
    args$x <- self$draws()
    if (!"measures" %in% names(args)) {
      args$measures <- posterior::default_summary_measures()
    }
    summary <- do.call(posterior::summarise_draws, args)
  }
  summary
}
CmdStanMCMC$set("public", "summary", summary_method)
CmdStanVB$set("public", "summary", summary_method)


#' Run CmdStan's `bin/stansummary` and `bin/diagnose`
#'
#' @name fit-method-cmdstan_summary
#' @aliases fit-method-cmdstan_diagnose
#' @note Although these methods also work for models fit using the
#'   [`$variational()`][model-method-variational] method, much of the output is
#'   only relevant for models fit using the [`$sample()`][model-method-sample]
#'   method.
#'
#' @section Usage:
#'   ```
#'   $cmdstan_summary()
#'   $cmdstan_diagnose()
#'   ```
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`]
#'
NULL

cmdstan_summary_method <- function(...) {
  self$runset$run_cmdstan_tool("stansummary", ...)
}
cmdstan_diagnose_method <- function(...) {
  self$runset$run_cmdstan_tool("diagnose", ...)
}
CmdStanMCMC$set("public", "cmdstan_summary", cmdstan_summary_method)
CmdStanVB$set("public", "cmdstan_summary", cmdstan_summary_method)
CmdStanMCMC$set("public", "cmdstan_diagnose", cmdstan_diagnose_method)
CmdStanVB$set("public", "cmdstan_diagnose", cmdstan_diagnose_method)


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
