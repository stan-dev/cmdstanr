# CmdStanFit superclass ---------------------------------------------------
# CmdStanMCMC, CmdStanMLE, and CmdStanVB all share the methods of the
# superclass CmdStanFit and also have their own unique methods
CmdStanFit <- R6::R6Class(
  classname = "CmdStanFit",
  public = list(
    runset = NULL,
    initialize = function(runset) {
      checkmate::assert_r6(runset, classes = "CmdStanRun")
      self$runset <- runset
      invisible(self)
    },

    num_runs = function() {
      self$runset$num_runs()
    },

    time = function() {
      self$runset$time()
    },

    draws = function() {
      if (is.null(private$draws_)) {
        private$read_csv_()
      }
      private$draws_
    },

    lp = function() {
      lp__ <- posterior::subset_draws(self$draws(), variable = "lp__")
      lp__ <- posterior::as_draws_matrix(lp__) # if mcmc this combines all chains, otherwise does nothing
      as.numeric(lp__)
    },

    sampling_info = function() {
      if (is.null(private$sampling_info_)) {
        private$read_csv_()
      }
      private$sampling_info_
    },

    summary = function(...) {
      if (self$runset$method() == "sample") {
        summary <- posterior::summarise_draws(self$draws(), ...)
      } else {
        if (!length(list(...))) {
          # if user didn't supply any args use default summary measures,
          # which don't include MCMC-specific things
          summary <- posterior::summarise_draws(
            self$draws(),
            posterior::default_summary_measures()
          )
        } else {
          # otherwise use whatever the user specified via ...
          summary <- posterior::summarise_draws(self$draws(), ...)
        }
      }
      if (self$runset$method() == "optimize") {
        summary <- summary[, c("variable", "mean")]
        colnames(summary) <- c("variable", "estimate")
      }
      summary
    },
    cmdstan_summary = function(...) {
      self$runset$run_cmdstan_tool("stansummary", ...)
    },
    cmdstan_diagnose = function(...) {
      self$runset$run_cmdstan_tool("diagnose", ...)
    },

    output_files = function() {
      self$runset$output_files()
    },
    diagnostic_files = function() {
      self$runset$diagnostic_files()
    },
    data_file = function() {
      self$runset$data_file()
    },
    save_output_files = function(dir = ".",
                                 basename = NULL,
                                 timestamp = TRUE,
                                 random = TRUE) {
      self$runset$save_output_files(dir, basename, timestamp, random)
    },
    save_diagnostic_files = function(dir = ".",
                                     basename = NULL,
                                     timestamp = TRUE,
                                     random = TRUE) {
      self$runset$save_diagnostic_files(dir, basename, timestamp, random)
    },
    save_data_file = function(dir = ".",
                              basename = NULL,
                              timestamp = TRUE,
                              random = TRUE) {
      self$runset$save_data_file(dir, basename, timestamp, random)
    }
  ),
  private = list(
    draws_ = NULL,
    sampling_info_ = NULL
  )
)


# Document shared methods ----------------------------------------------------------

#' Extract posterior draws
#'
#' @name fit-method-draws
#' @description Extract posterior draws after MCMC or approximate posterior
#'   draws after variational approximation using formats provided by the
#'   \pkg{posterior} package.
#'
#'   The variables include the `parameters`, `transformed parameters`, and
#'   `generated quantities` from the Stan program as well as `lp__`, the total
#'   log probability (`target`) accumulated in the `model` block.
#'
#' @section Usage:
#'   ```
#'   $draws(inc_warmup = FALSE, ...)
#'   ```
#' @section Arguments:
#' * `inc_warmup`: Should warmup draws be included? Defaults to `FALSE`. Only
#' applicable for MCMC.
#' * `...`: Arguments passed on to [posterior::as_draws()].
#'
#' @section Value:
#' * For MCMC, a 3-D [`draws_array`][posterior::draws_array] object (iteration x
#' chain x variable).
#' * For variational inference, a 2-D [`draws_matrix`][posterior::draws_matrix]
#' object (draw x variable). An additional variable `lp_approx__` is also
#' included, which is the log density of the variational approximation to the
#' posterior evaluated at each of the draws.
#' * For optimization, a 1-row [`draws_matrix`][posterior::draws_matrix] with
#' one column per variable. These are *not* actually draws, just point estimates
#' stored in the `draws_matrix` format.
#'
NULL

#' Extract sampler diagnostics
#'
#' @name fit-method-sampler_diagnostics
#' @description Extract the values of sampler diagnostics for each iteration and
#'   chain of MCMC.
#'
#' @section Usage:
#'   ```
#'   $sampler_diagnostics(inc_warmup = FALSE, ...)
#'   ```
#' @section Arguments:
#' * `inc_warmup`: Should warmup draws be included? Defaults to `FALSE`.
#' * `...`: Optional arguments to pass to [posterior::as_draws_array()].
#'
#' @section Value:
#' A 3-D [`draws_array`][posterior::draws_array] object (iteration x chain x
#' variable). The variables for Stan's default MCMC algorithm are
#' `"accept_stat__"`, `"stepsize__"`, `"treedepth__"`, `"n_leapfrog__"`,
#' `"divergent__"`, `"energy__"`.
#'
#' @seealso [`CmdStanMCMC`]
#'
NULL

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
#' * `...`: Optional arguments to pass to [posterior::summarise_draws()].
#'
#' @section Value:
#' The data frame returned by [posterior::summarise_draws()].
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`]
#'
NULL


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


#' Save output and data files
#'
#' @name fit-method-save_output_files
#' @aliases fit-method-save_data_file fit-method-save_diagnostic_files
#'   fit-method-output_files fit-method-data_file fit-method-diagnostic_files
#'
#' @description All fitted model objects have methods for saving (moving to a
#'   specified location) the files created by CmdStanR to hold CmdStan output
#'   csv files and input data files. These methods move the files from their
#'   current location (possibly the temporary directory) to a user-specified
#'   location. __The paths stored in the fitted model object will also be
#'   updated to point to the new file locations.__
#'
#'   The versions without the `save_` prefix (e.g., `$output_files()`) return
#'   the current file paths without moving any files.
#'
#' @section Usage:
#'   ```
#'   $save_output_files(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)
#'   $save_diagnostic_files(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)
#'   $save_data_file(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)
#'
#'   $output_files()
#'   $diagnostic_files()
#'   $data_file()
#'   ```
#'
#' @section Arguments:
#' * `dir`: (string) Path to directory where the files should be saved.
#' * `basename`: (string) Base filename to use. See __Details__.
#' * `timestamp`: (logical) Should a timestamp be added to the file name(s)?
#'   Defaults to `TRUE`. See __Details__.
#' * `random`: (logical) Should random alphanumeric characters be added to the
#'   end of the file name(s)? Defaults to `TRUE`. See __Details__.
#'
#' @section Details:
#' For `$save_output_files()` the files moved to `dir` will have names of
#' the form `basename-timestamp-id-random`, where
#' * `basename` is the user's provided `basename` argument;
#' * `timestamp` is of the form `format(Sys.time(), "%Y%m%d%H%M")`;
#' * `id` is the MCMC chain id (or `1` for non MCMC);
#' * `random` contains six random alphanumeric characters.
#'
#' For `$save_diagnostic_files()` everything is the same as for
#' `$save_output_files()` except `"-diagnostic-"` is included in the new
#' file name after `basename`.
#'
#' For `$save_data_file()` no `id` is included in the file name because even
#' with multiple MCMC chains the data file is the same.
#'
#' @section Value:
#' The `$save_*` methods print a message with the new file paths and (invisibly)
#' return a character vector of the new paths (or `NA` for any that couldn't be
#' copied). They also have the side effect of setting the internal paths in the
#' fitted model object to the new paths.
#'
#' The methods _without_ the `save_` prefix return character vectors of file
#' paths without moving any files.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`]
#'
NULL


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
#' `CmdStanMCMC` objects have the following methods:
#'
#' \tabular{ll}{
#'  **Method** \tab **Description** \cr
#'  [`$draws()`][fit-method-draws]
#'    \tab Return posterior draws as a [`draws_array`][posterior::draws_array]. \cr
#'  [`$sampler_diagnostics()`][fit-method-sampler_diagnostics]
#'    \tab Return sampler diagnostics as a [`draws_array`][posterior::draws_array]. \cr
#'  [`$summary()`][fit-method-summary]
#'    \tab Run [posterior::summarise_draws()]. \cr
#'  [`$cmdstan_summary()`][fit-method-cmdstan_summary]
#'    \tab Run and print CmdStan's `bin/stansummary`. \cr
#'  [`$cmdstan_diagnose()`][fit-method-cmdstan_summary]
#'    \tab Run and print CmdStan's `bin/diagnose`. \cr
#'  [`$save_output_files()`][fit-method-save_output_files]
#'    \tab Save output CSV files to a specified location. \cr
#'  [`$save_data_file()`][fit-method-save_data_file]
#'    \tab Save JSON data file to a specified location. \cr
#'  [`$save_diagnostic_files()`][fit-method-save_diagnostic_files]
#'    \tab Save diagnostic CSV files to a specified location. \cr
#'  `$time()` \tab Return a list containing the total time and a data frame of
#'    execution times of all chains. \cr
#'  `$output()` \tab Return the stdout and stderr of all chains as a list of
#'    character vectors, or pretty print the output for a single chain if
#'    `id` argument is specified. \cr
#' }
#'
NULL
CmdStanMCMC <- R6::R6Class(
  classname = "CmdStanMCMC",
  inherit = CmdStanFit,
  public = list(
    initialize = function(runset) {
      super$initialize(runset)
      if (!length(self$output_files())) {
        warning("No chains finished successfully. Unable to retrieve the fit.")
      } else {
        private$read_csv_(diagnostic_warnings = !runset$args$method_args$fixed_param)
      }
    },
    num_chains = function() {
      super$num_runs()
    },
    output = function(id = NULL) {
      if (is.null(id)) {
        self$runset$procs$chain_output()
      } else {
        cat(paste(self$runset$procs$chain_output(id), collapse="\n"))
      }
    },

    draws = function(inc_warmup = FALSE) {
      if (is.null(private$draws_)) {
        private$read_csv_()
      }
      if (inc_warmup) {
        if (!private$sampling_info_$save_warmup) {
          stop("Warmup draws were requested from a fit object without them! Please restart the sampling with save_warmup = TRUE.")
        }
        posterior::bind_draws(private$warmup_draws_, private$draws_, along="iteration")
      } else {
        private$draws_
      }
    },

    sampler_diagnostics = function(inc_warmup = FALSE) {
      if (is.null(private$draws_)) {
        private$read_csv_()
      }
      if (inc_warmup) {
        if (!private$sampling_info_$save_warmup) {
          stop("Warmup sampler diagnostics were requested from a fit object without them! Please restart the sampling with save_warmup = TRUE.")
        }
        posterior::bind_draws(private$warmup_sampler_diagnostics_, private$sampler_diagnostics_, along="iteration")
      } else {
        private$sampler_diagnostics_
      }
    }
  ),
  private = list(
    sampler_diagnostics_ = NULL,
    warmup_sampler_diagnostics_ = NULL,
    warmup_draws_ = NULL,
    draws_ = NULL,
    read_csv_ = function(diagnostic_warnings = FALSE) {
      if (!length(self$output_files())) {
        stop("No chains finished successfully. Unable to retrieve the fit.",
             call. = FALSE)
      }
      data_csv <- read_sample_csv(self$output_files())
      if (diagnostic_warnings) {
        check_divergences(data_csv)
        check_sampler_transitions_treedepth(data_csv)
      }
      private$draws_ <- data_csv$post_warmup_draws
      private$sampler_diagnostics_ <- data_csv$post_warmup_sampler_diagnostics
      private$sampling_info_ <- data_csv$sampling_info
      if (!is.null(data_csv$sampling_info$save_warmup)
         && data_csv$sampling_info$save_warmup) {
        private$warmup_draws_ <- data_csv$warmup_draws
        private$warmup_sampler_diagnostics_ <- data_csv$warmup_sampler_diagnostics
      }
      invisible(self)
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
#'   [`$optimize()`][model-method-optimize] method of a [`CmdStanModel`] object.
#'
#' @details
#' `CmdStanMLE` objects have the following methods:
#'
#' \tabular{ll}{
#'  **Method** \tab **Description** \cr
#'  [`draws()`][fit-method-draws] \tab Return the point estimate as a 1-row
#'  [`draws_matrix`][posterior::draws_matrix]. \cr
#'  [`$summary()`][fit-method-summary] \tab Run [posterior::summarise_draws()]. \cr
#'  `$lp()` \tab Return the total log probability density (`target`) computed
#'  in the model block of the Stan program. \cr
#'  `$mle()` \tab Return the penalized maximum likelihod estimate (posterior
#'  mode) as a numeric vector with one element per variable (excluding `lp()`). \cr
#'  [`$save_output_files()`][fit-method-save_output_files] \tab Save output CSV
#'  files to a specified location. \cr
#'  [`$save_data_file()`][fit-method-save_data_file] \tab Save JSON data file
#'  to a specified location. \cr
#' }
#'
NULL

CmdStanMLE <- R6::R6Class(
  classname = "CmdStanMLE",
  inherit = CmdStanFit,
  public = list(
    mle = function() {
      x <- self$draws()
      x <- x[, colnames(x) != "lp__"]
      estimate <- setNames(as.numeric(x), nm = posterior::variables(x))
      estimate
    }
  ),
  private = list(
    read_csv_ = function() {
      optim_output <- read_optim_csv(self$output_files())
      private$draws_ <- optim_output[["draws"]]
      invisible(self)
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
#' `CmdStanVB` objects have the following methods:
#'
#' \tabular{ll}{
#'  **Method** \tab **Description** \cr
#'  [`$draws()`][fit-method-draws] \tab Return approximate posterior draws
#'  as a [`draws_matrix`][posterior::draws_matrix]. \cr
#'  [`$summary()`][fit-method-summary] \tab Run [posterior::summarise_draws()]. \cr
#'  `$lp()` \tab Return a numeric vector containing the target
#'  (log-posterior) evaluated at each of the draws. \cr
#'  `$lp_approx()` \tab Return a numeric vector containing the log density of the
#'  variational approximation to the posterior evaluated at each of the draws. \cr
#'  [`$cmdstan_summary()`][fit-method-cmdstan_summary]
#'    \tab Run and print CmdStan's `bin/stansummary`. \cr
#'  [`$cmdstan_diagnose()`][fit-method-cmdstan_summary]
#'    \tab Run and print CmdStan's `bin/diagnose`. \cr
#'  [`$save_output_files()`][fit-method-save_output_files]
#'    \tab Save output CSV files to a specified location. \cr
#'  [`$save_data_file()`][fit-method-save_data_file]
#'    \tab Save JSON data file to a specified location. \cr
#'  [`$save_diagnostic_files()`][fit-method-save_diagnostic_files]
#'    \tab Save diagnostic CSV files to a specified location. \cr
#' }
#'
NULL

CmdStanVB <- R6::R6Class(
  classname = "CmdStanVB",
  inherit = CmdStanFit,
  public = list(
    lp_approx = function() {
      as.numeric(self$draws()[, "lp_approx__"])
    }
  ),
  private = list(
    read_csv_ = function() {
      vb_output <- read_vb_csv(self$output_files())
      private$draws_ <- vb_output[["draws"]]
      invisible(self)
    }
  )
)

