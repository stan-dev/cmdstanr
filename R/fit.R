# CmdStanFit superclass ---------------------------------------------------

# CmdStanMCMC, CmdStanMLE, CmdStanVB, and CmdStanGQ all share the methods of the
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

    save_object = function(file, ...) {
      self$draws()
      try(self$sampler_diagnostics(), silent = TRUE)
      try(self$init(), silent = TRUE)
      saveRDS(self, file = file, ...)
      invisible(self)
    },

    num_procs = function() {
      self$runset$num_procs()
    },

    time = function() {
      self$runset$time()
    },

    draws = function(variables = NULL) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Fitting failed. Unable to retrieve the draws.", call. = FALSE)
      }
      # CmdStanMCMC has its own implementation, this is used for VB and MLE
      if (is.null(private$draws_)) {
        private$read_csv_()
      }
      posterior::subset_draws(private$draws_, variable = variables)
    },

    lp = function() {
      lp__ <- self$draws(variables = "lp__")
      lp__ <- posterior::as_draws_matrix(lp__) # if mcmc this combines all chains, otherwise does nothing
      as.numeric(lp__)
    },

    metadata = function() {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Fitting failed. Unable to retrieve the metadata.", call. = FALSE)
      }
      if (is.null(private$metadata_)) {
        private$read_csv_()
      }
      private$metadata_
    },

    init = function() {
      if (is.null(private$init_)) {
        init_paths <- self$metadata()$init
        if (!is.character(init_paths) || any(!file.exists(init_paths))) {
          stop("Can't find initial values files.", call. = FALSE)
        }
        private$init_ <- lapply(init_paths, jsonlite::read_json, simplifyVector = TRUE)
      }
      private$init_
    },

    summary = function(variables = NULL, ...) {
      draws <- self$draws(variables)
      if (self$runset$method() == "sample") {
        summary <- posterior::summarise_draws(draws, ...)
      } else {
        if (!length(list(...))) {
          # if user didn't supply any args use default summary measures,
          # which don't include MCMC-specific things
          summary <- posterior::summarise_draws(
            draws,
            posterior::default_summary_measures()
          )
        } else {
          # otherwise use whatever the user specified via ...
          summary <- posterior::summarise_draws(draws, ...)
        }
      }
      if (self$runset$method() == "optimize") {
        summary <- summary[, c("variable", "mean")]
        colnames(summary) <- c("variable", "estimate")
      }
      summary
    },

    # print summary table without using tibbles
    print = function(variables = NULL, ..., digits = 2, max_rows = 10) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Fitting failed. Unable to print.", call. = FALSE)
      }
      # filter variables before passing to summary to avoid computing anything
      # that won't be printed because of max_rows
      all_variables <- self$metadata()$model_params
      if (is.null(variables)) {
        total_rows <- length(all_variables)
        variables_to_print <- all_variables[seq_len(max_rows)]
      } else {
        matches <- matching_variables(variables, all_variables)
        if (length(matches$not_found) > 0) {
          stop("Can't find the following variable(s): ",
               paste(matches$not_found, collapse = ", "), call. = FALSE)
        }
        total_rows <- length(matches$matching)
        variables_to_print <- matches$matching[seq_len(max_rows)]
      }

      # if max_rows > length(variables_to_print) some will be NA
      variables_to_print <- variables_to_print[!is.na(variables_to_print)]

      out <- self$summary(variables_to_print, ...)
      out <- as.data.frame(out)
      out[, 1] <- format(out[, 1], justify = "left")
      out[, -1] <- format(round(out[, -1], digits = digits), nsmall = digits)
      for (col in grep("ess_", colnames(out), value = TRUE)) {
        out[[col]] <- as.integer(out[[col]])
      }

      opts <- options(max.print = prod(dim(out)))
      on.exit(options(max.print = opts$max.print), add = TRUE)
      print(out, row.names=FALSE)
      if (max_rows < total_rows) {
        cat("\n # showing", max_rows, "of", total_rows,
            "rows (change via 'max_rows' argument)\n")
      }
      invisible(self)
    },

    cmdstan_summary = function(...) {
      self$runset$run_cmdstan_tool("stansummary", ...)
    },
    cmdstan_diagnose = function(...) {
      self$runset$run_cmdstan_tool("diagnose", ...)
    },
    output = function(id = NULL) {
      # non-MCMC fit is obtained with one process only
      # so fit$output() prints the output of that process
      cat(paste(self$runset$procs$proc_output(1), collapse="\n"))
    },
    output_files = function(include_failed = FALSE) {
      self$runset$output_files(include_failed)
    },
    latent_dynamics_files = function(include_failed = FALSE) {
      self$runset$latent_dynamics_files(include_failed)
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
    save_latent_dynamics_files = function(dir = ".",
                                     basename = NULL,
                                     timestamp = TRUE,
                                     random = TRUE) {
      self$runset$save_latent_dynamics_files(dir, basename, timestamp, random)
    },
    save_data_file = function(dir = ".",
                              basename = NULL,
                              timestamp = TRUE,
                              random = TRUE) {
      self$runset$save_data_file(dir, basename, timestamp, random)
    },
    return_codes = function() {
      self$runset$procs$return_codes()
    }
  ),
  private = list(
    draws_ = NULL,
    metadata_ = NULL,
    init_ = NULL
  )
)


# Document methods ----------------------------------------------------------

#' Save fitted model object to a file
#'
#' @name fit-method-save_object
#' @aliases save_object
#' @description This method is a wrapper around [base::saveRDS()] that ensures
#'   that all posterior draws and diagnostics are saved when saving a fitted
#'   model object. Because the contents of the CmdStan output CSV files are only
#'   read into R lazily (i.e., as needed), the `$save_object()` method is the
#'   safest way to guarantee that everything has been read in before saving.
#'
#' @section Usage:
#'   ```
#'   $save_object(file, ...)
#'   ```
#'
#' @param file (string) Path where the file should be saved.
#' @param ... Other arguments to pass to [base::saveRDS()] besides `object` and `file`.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanGQ`]
#'
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic")
#'
#' temp_rds_file <- tempfile(fileext = ".RDS")
#' fit$save_object(file = temp_rds_file)
#' rm(fit)
#'
#' fit <- readRDS(temp_rds_file)
#' fit$summary()
#' }
#'
NULL

#' Extract posterior draws
#'
#' @name fit-method-draws
#' @aliases draws
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
#'   $draws(variables = NULL, inc_warmup = FALSE, ...)
#'   ```
#' @param variables (character vector) The variables (parameters and generated
#' quantities) to read in. If `NULL` (the default) then the draws of all
#' variables are included.
#' @param inc_warmup (logical) For MCMC only, should warmup draws be included?
#' Defaults to `FALSE`.
#' @param ... Arguments passed on to
#' [`posterior::as_draws_array()`][posterior::draws_array].
#'
#' @section Value:
#' * For [MCMC][model-method-sample], a 3-D
#' [`draws_array`][posterior::draws_array] object (iteration x chain x
#' variable).
#' * For standalone [generated quantities][model-method-generate-quantities], a
#' 3-D [`draws_array`][posterior::draws_array] object (iteration x chain x
#' variable).
#' * For [variational inference][model-method-variational], a 2-D
#' [`draws_matrix`][posterior::draws_matrix] object (draw x variable). An
#' additional variable `lp_approx__` is also included, which is the log density
#' of the variational approximation to the posterior evaluated at each of the
#' draws.
#' * For [optimization][model-method-optimize], a 1-row
#' [`draws_matrix`][posterior::draws_matrix] with one column per variable. These
#' are *not* actually draws, just point estimates stored in the `draws_matrix`
#' format. See [`$mle()`][fit-method-mle] to extract them as a numeric vector.
#'
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanGQ`]
#'
#' @examples
#' \dontrun{
#' library(posterior)
#' library(bayesplot)
#' color_scheme_set("brightblue")
#'
#' # logistic regression with intercept alpha and coefficients beta
#' fit <- cmdstanr_example("logistic")
#'
#' # returned as 3-D array (see ?posterior::draws_array)
#' draws <- fit$draws()
#' dim(draws)
#' str(draws)
#'
#' # can easily convert to other formats (data frame, matrix, list)
#' as_draws_df(draws)  # see also as_draws_matrix, as_draws_list
#'
#' # can select specific parameters
#' fit$draws("alpha")
#' fit$draws("beta")  # selects entire vector beta
#' fit$draws(c("alpha", "beta[2]"))
#'
#' # can be passed directly to bayesplot plotting functions
#' mcmc_dens(fit$draws(c("alpha", "beta")))
#' mcmc_scatter(fit$draws(c("beta[1]", "beta[2]")), alpha = 0.3)
#' }
#'
NULL

#' Extract sampler diagnostics
#'
#' @name fit-method-sampler_diagnostics
#' @aliases sampler_diagnostics
#' @description Extract the values of sampler diagnostics for each iteration and
#'   chain of MCMC.
#'
#' @section Usage:
#'   ```
#'   $sampler_diagnostics(inc_warmup = FALSE, ...)
#'   ```
#'
#' @param inc_warmup (logical) Should warmup draws be included? Defaults to `FALSE`.
#' @param ... Arguments passed on to
#' [`posterior::as_draws_array()`][posterior::draws_array].
#'
#' @section Value:
#' A 3-D [`draws_array`][posterior::draws_array] object (iteration x chain x
#' variable). The variables for Stan's default MCMC algorithm are
#' `"accept_stat__"`, `"stepsize__"`, `"treedepth__"`, `"n_leapfrog__"`,
#' `"divergent__"`, `"energy__"`.
#'
#' @seealso [`CmdStanMCMC`]
#'
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic")
#' sampler_diagnostics <- fit$sampler_diagnostics()
#' str(sampler_diagnostics)
#'
#' library(posterior)
#' as_draws_df(sampler_diagnostics)
#' }
#'
NULL

#' Extract inverse metric (mass matrix)
#'
#' @name fit-method-inv_metric
#' @aliases inv_metric
#' @description Return a list containing the inverse metric (mass matrix) for
#'   each chain.
#'
#' @section Usage:
#'   ```
#'   $inv_metric(matrix = TRUE)
#'   ```
#'
#' @param matrix (logical) If a diagonal metric was used, setting `matrix = FALSE`
#' returns a list containing just the diagonals of the matrices instead of the
#' full matrices. Setting `matrix = FALSE` has no effect for dense metrics.
#'
#' @section Value:
#' A list of length equal to the number of MCMC chains. See the `matrix`
#' argument for details.
#'
#' @seealso [`CmdStanMCMC`]
#'
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic")
#' fit$inv_metric()
#' fit$inv_metric(matrix=FALSE)
#'
#' fit <- cmdstanr_example("logistic", metric = "dense_e")
#' fit$inv_metric()
#' }
#'
NULL

#' Extract user-specified initial values
#'
#' @name fit-method-init
#' @aliases init
#' @description Return user-specified initial values. If the user provided
#'   initial values files or \R objects (list of lists or function) via the
#'   `init` argument when fitting the model then these are returned (always in
#'   the list of lists format). Currently it is not possible to extract initial
#'   values generated automatically by CmdStan, although CmdStan may support
#'   this in the future.
#'
#' @section Usage:
#'   ```
#'   $init()
#'   ```
#'
#' @section Value:
#' A list of lists. See **Examples**.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`]
#'
#' @examples
#' \dontrun{
#' init_fun <- function() list(alpha = rnorm(1), beta = rnorm(3))
#' fit <- cmdstanr_example("logistic", init = init_fun, chains = 2)
#' str(fit$init())
#'
#' # partial inits (only specifying for a subset of parameters)
#' init_list <- list(
#'   list(mu = 10, tau = 2),
#'   list(mu = -10, tau = 1)
#' )
#' fit <- cmdstanr_example("schools_ncp", init = init_list, chains = 2, adapt_delta = 0.9)
#'
#' # only user-specified inits returned
#' str(fit$init())
#' }
#'
NULL

#' Extract log probability (target)
#'
#' @name fit-method-lp
#' @aliases lp lp_approx
#' @description The `$lp()` method extracts `lp__`, the total log probability
#'   (`target`) accumulated in the `model` block of the Stan program. For
#'   variational inference the log density of the variational approximation to
#'   the posterior is also available via the `$lp_approx()` method.
#'
#'   See the [Log Probability Increment vs. Sampling
#'   Statement](https://mc-stan.org/docs/2_23/reference-manual/sampling-statements-section.html)
#'   section of the Stan Reference Manual for details on when normalizing
#'   constants are dropped from log probability calculations.
#'
#' @section Usage:
#'   ```
#'   $lp()
#'   $lp_approx()
#'   ```
#'
#' @section Details:
#' `lp__` is the unnormalized log density on Stan's [unconstrained
#' space](https://mc-stan.org/docs/2_23/reference-manual/variable-transforms-chapter.html).
#' This will in general be different than the unnormalized model log density
#' evaluated at a posterior draw (which is on the constrained space). `lp__` is
#' intended to diagnose sampling efficiency and evaluate approximations.
#'
#' `lp_approx__` is the log density of the variational approximation to `lp__`
#' (also on the unconstrained space). It is exposed in the variational method
#' for performing the checks described in Yao et al. (2018) and implemented in
#' the \pkg{loo} package.
#'
#' @section Value:
#' A numeric vector with length equal to the number of (post-warmup) draws for
#' MCMC and variational inference, and length equal to `1` for optimization.
#'
#' @references
#' Yao, Y., Vehtari, A., Simpson, D., and Gelman, A. (2018). Yes, but did it
#' work?: Evaluating variational inference. *Proceedings of the 35th
#' International Conference on Machine Learning*, PMLR 80:5581–5590.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`]
#'
#' @examples
#' \dontrun{
#' fit_mcmc <- cmdstanr_example("logistic")
#' head(fit_mcmc$lp())
#'
#' fit_mle <- cmdstanr_example("logistic", method = "optimize")
#' fit_mle$lp()
#'
#' fit_vb <- cmdstanr_example("logistic", method = "variational")
#' plot(fit_vb$lp(), fit_vb$lp_approx())
#' }
#'
NULL

#' Extract (penalized) maximum likelihood estimate after optimization
#'
#' @name fit-method-mle
#' @aliases mle
#' @description The `$mle()` method is only available for [`CmdStanMLE`] objects.
#' It returns the penalized maximum likelihood estimate (posterior mode) as a
#' numeric vector with one element per variable. The returned vector does not
#' include `lp__`, the total log probability (`target`) accumulated in the
#' `model` block of the Stan program, which is available via the
#' [`$lp()`][fit-method-lp] method and also included in the
#' [`$draws()`][fit-method-draws] method.
#'
#' @section Usage:
#'   ```
#'   $mle(variables = NULL)
#'   ```
#'
#' @param variables (character vector) The variables (parameters and generated
#' quantities) to include. If NULL (the default) then all variables are
#' included.
#'
#' @section Value:
#' A numeric vector. See **Examples**.
#'
#' @seealso [`CmdStanMLE`]
#'
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic", method = "optimize")
#' fit$mle()
#' fit$mle("alpha")
#' fit$mle("beta")
#' fit$mle("beta[2]")
#' }
#'
NULL

#' Compute a summary table of estimates and diagnostics
#'
#' @name fit-method-summary
#' @aliases summary print.CmdStanMCMC print.CmdStanMLE print.CmdStanVB
#' @description The `$summary()` method runs
#'   [`summarise_draws()`][posterior::draws_summary] from the \pkg{posterior}
#'   package and returns the output. For MCMC, only post-warmup draws are
#'   included in the summary.
#'
#'   The `$print()` method prints the same summary stats but removes the extra
#'   formatting used for printing tibbles and returns the fitted model object
#'   itself. The `$print()` method may also be faster than `$summary()` because
#'   it is designed to only compute the summary statistics for the variables
#'   that will actually fit in the printed output (see argument `max_rows`)
#'   whereas `$summary()` will compute them for all of the specified variables
#'   in order to be able to return them to the user.
#'
#' @section Usage:
#'   ```
#'   $summary(variables = NULL, ...)
#'   $print(variables = NULL, ..., digits = 2, max_rows = 10)
#'   ```
#'
#' @param variables (character vector) The variables to include.
#' @param ... Optional arguments to pass to [`posterior::summarise_draws()`][posterior::draws_summary].
#' @param digits (integer) For `print` only, the number of digits to use for rounding.
#' @param max_rows (integer) For `print` only, the maximum number of rows to print.
#'
#' @section Value:
#' The `$summary()` method returns the tibble data frame created by
#' [`posterior::summarise_draws()`][posterior::draws_summary].
#'
#' The `$print()` method returns the fitted model object itself (invisibly),
#' which is the standard behavior for print methods in \R.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanGQ`]
#'
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic")
#' fit$summary()
#' fit$print()
#' fit$print(max_rows = 2) # same as print(fit, max_rows = 2)
#'
#' # include only certain variables
#' fit$summary("beta")
#' fit$print(c("alpha", "beta[2]"))
#'
#' # include all variables but only certain summaries
#' fit$summary(NULL, c("mean", "sd"))
#'
#' # can use functions created from formulas
#' # for example, calculate Pr(beta > 0)
#' fit$summary("beta", prob_gt_0 = ~ mean(. > 0))
#' }
#'
NULL


#' Run CmdStan's `stansummary` and `diagnose`
#'
#' @description
#' Run CmdStan's `stansummary` and `diagnose` utilities. These are
#' documented in the CmdStan Guide:
#' * https://mc-stan.org/docs/cmdstan-guide/stansummary.html
#' * https://mc-stan.org/docs/cmdstan-guide/diagnose.html
#'
#' @name fit-method-cmdstan_summary
#' @aliases fit-method-cmdstan_diagnose cmdstan_summary cmdstan_diagnose
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
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic")
#' fit$cmdstan_diagnose()
#' fit$cmdstan_summary()
#' }
#'
NULL

#' Save output and data files
#'
#' @name fit-method-save_output_files
#' @aliases fit-method-save_data_file fit-method-save_latent_dynamics_files
#'   fit-method-output_files fit-method-data_file fit-method-latent_dynamics_files
#'   save_output_files save_data_file save_latent_dynamics_files
#'   output_files data_file latent_dynamics_files
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
#'   $save_latent_dynamics_files(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)
#'   $save_data_file(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)
#'
#'   $output_files()
#'   $latent_dynamics_files()
#'   $data_file()
#'   ```
#'
#' @param dir (string) Path to directory where the files should be saved.
#' @param basename (string) Base filename to use. See __Details__.
#' @param timestamp (logical) Should a timestamp be added to the file name(s)?
#'   Defaults to `TRUE`. See __Details__.
#' @param random (logical) Should random alphanumeric characters be added to the
#'   end of the file name(s)? Defaults to `TRUE`. See __Details__.
#'
#' @section Details:
#' For `$save_output_files()` the files moved to `dir` will have names of
#' the form `basename-timestamp-id-random`, where
#' * `basename` is the user's provided `basename` argument;
#' * `timestamp` is of the form `format(Sys.time(), "%Y%m%d%H%M")`;
#' * `id` is the MCMC chain id (or `1` for non MCMC);
#' * `random` contains six random alphanumeric characters;
#'
#' For `$save_latent_dynamics_files()` everything is the same as for
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
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanGQ`]
#'
NULL

#' Report timing of CmdStan runs
#'
#' @name fit-method-time
#' @aliases time
#' @description Report the run time in seconds. For MCMC additional information
#'   is provided about the run times of individual chains and the warmup and
#'   sampling phases.
#'
#' @section Usage:
#'   ```
#'   $time()
#'   ```
#'
#' @section Value:
#' A list with elements
#' * `total`: (scalar) the total run time.
#' * `chains`: (data frame) for MCMC only, timing info for the individual
#' chains. The data frame has columns `"chain_id"`, `"warmup"`, `"sampling"`,
#' and `"total"`.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanGQ`]
#'
#' @examples
#' \dontrun{
#' fit_mcmc <- cmdstanr_example("logistic", method = "sample")
#' fit_mcmc$time()
#'
#' fit_mle <- cmdstanr_example("logistic", method = "optimize")
#' fit_mle$time()
#'
#' fit_vb <- cmdstanr_example("logistic", method = "variational")
#' fit_vb$time()
#' }
#'
NULL

#' Access console output
#'
#' @name fit-method-output
#' @aliases output
#' @description For MCMC the `$output()` method returns the stdout and stderr of
#' all chains as a list of character vectors. If the `id` argument is specified
#' it pretty prints the console output for a single chain.
#'
#' For optimization and variational inference `$output()` just pretty prints the
#' console output.
#'
#' @section Usage:
#'   ```
#'   $output(id = NULL)
#'   ```
#'
#' @param id (integer) For MCMC only, the chain id.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanGQ`]
#'
#' @examples
#' \dontrun{
#' fit_mcmc <- cmdstanr_example("logistic", method = "sample")
#' fit_mcmc$output(1)
#' out <- fit_mcmc$output()
#' str(out)
#'
#' fit_mle <- cmdstanr_example("logistic", method = "optimize")
#' fit_mle$output()
#'
#' fit_vb <- cmdstanr_example("logistic", method = "variational")
#' fit_vb$output()
#' }
#'
NULL

#' Extract metadata from CmdStan CSV files
#'
#' @name fit-method-metadata
#' @aliases metadata
#' @description The `$metadata()` method returns a list of information gathered
#'   from the CSV output files, including the CmdStan configuration used when
#'   fitting the model. See **Examples** and [read_cmdstan_csv()].
#'
#' @section Usage:
#'   ```
#'   $metadata()
#'   ```
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanGQ`]
#'
#' @examples
#' \dontrun{
#' fit_mcmc <- cmdstanr_example("logistic", method = "sample")
#' str(fit_mcmc$metadata())
#'
#' fit_mle <- cmdstanr_example("logistic", method = "optimize")
#' str(fit_mle$metadata())
#'
#' fit_vb <- cmdstanr_example("logistic", method = "variational")
#' str(fit_vb$metadata())
#' }
#'
NULL

#' Extract return codes from CmdStan
#'
#' @name fit-method-return_codes
#' @aliases return_codes
#' @description The `$return_codes()` method returns a vector of return codes
#'   from the CmdStan run(s). A return code of 0 indicates a successful run.
#'
#' @section Usage:
#'   ```
#'   $return_codes()
#'   ```
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanGQ`]
#'
#' @examples
#' \dontrun{
#' # example with return codes all zero
#' fit_mcmc <- cmdstanr_example("schools", method = "sample")
#' fit_mcmc$return_codes() # should be all zero
#'
#' # example of non-zero return code (optimization fails for hierarchical model)
#' fit_opt <- cmdstanr_example("schools", method = "optimize")
#' fit_opt$return_codes() # should be non-zero
#' }
#'
NULL

#' Leave-one-out cross-validation (LOO-CV)
#'
#' @name fit-method-loo
#' @aliases loo
#' @description The `$loo()` method computes approximate LOO-CV using the
#'   \pkg{loo} package. This is a simple wrapper around [loo::loo.array()]
#'   provided for convenience and requires computing the pointwise
#'   log-likelihood in your Stan program. See the \pkg{loo} package
#'   [vignettes](https://mc-stan.org/loo/articles/) for details.
#'
#' @section Usage:
#'   ```
#'   $loo(variables = "log_lik", r_eff = NULL, ...)
#'   ```
#'
#' @param variables (character vector) The name(s) of the variable(s) in the
#'   Stan program containing the pointwise log-likelihood. The default is to
#'   look for `"log_lik"`. This argument is passed to the
#'   [`$draws()][fit-method-draws] method.
#' @param r_eff There are several options:
#'   * `TRUE` (the default) will automatically call [loo::relative_eff.array()]
#'   to compute the `r_eff` argument to pass to [loo::loo.array()].
#'   * `FALSE` or `NULL` will avoid computing `r_eff` (which can sometimes be slow)
#'   but will result in a warning from the \pkg{loo} package.
#'   * If `r_eff` is anything else, that object will be passed as the `r_eff`
#'   argument to [loo::loo.array()].
#' @param ... Other arguments (e.g., `cores`, `save_psis`, etc.) passed to
#'   [loo::loo.array()].
#'
#' @section Value: The object returned by [loo::loo.array()].
#'
#' @seealso The \pkg{loo} package website with
#'   [documentation](https://mc-stan.org/loo/reference/index.html) and
#'   [vignettes](https://mc-stan.org/loo/articles/).
#'
#' @examples
#'
#' \dontrun{
#' # the "logistic" example model has "log_lik" in generated quantities
#' fit <- cmdstanr_example("logistic")
#' fit$loo(cores = 2)
#' }
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
#'   Like `CmdStanModel` objects, `CmdStanMCMC` objects are [R6][R6::R6Class]
#'   objects.
#'
#' @section Methods: `CmdStanMCMC` objects have the following associated
#'   methods, all of which have their own (linked) documentation pages.
#'
#'  ## Extract contents of fitted model object
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$draws()`][fit-method-draws] |  Return posterior draws as a [`draws_array`][posterior::draws_array]. |
#'  [`$sampler_diagnostics()`][fit-method-sampler_diagnostics] |  Return sampler diagnostics as a [`draws_array`][posterior::draws_array]. |
#'  [`$lp()`][fit-method-lp] |  Return the total log probability density (`target`). |
#'  [`$inv_metric()`][fit-method-inv_metric] |  Return the inverse metric for each chain. |
#'  [`$init()`][fit-method-init] |  Return user-specified initial values. |
#'  [`$metadata()`][fit-method-metadata] | Return a list of metadata gathered from the CmdStan CSV files. |
#'
#'  ## Summarize inferences and diagnostics
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$summary()`][fit-method-summary] |  Run [`posterior::summarise_draws()`][posterior::draws_summary]. |
#'  [`$cmdstan_summary()`][fit-method-cmdstan_summary] |  Run and print CmdStan's `bin/stansummary`. |
#'  [`$cmdstan_diagnose()`][fit-method-cmdstan_summary] |  Run and print CmdStan's `bin/diagnose`. |
#'  [`$loo()`][fit-method-loo]  |  Run [loo::loo.array()] for approximate LOO-CV |
#'
#'  ## Save fitted model object and temporary files
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$save_object()`][fit-method-save_object] |  Save fitted model object to a file. |
#'  [`$save_output_files()`][fit-method-save_output_files] |  Save output CSV files to a specified location. |
#'  [`$save_data_file()`][fit-method-save_data_file] |  Save JSON data file to a specified location. |
#'  [`$save_latent_dynamics_files()`][fit-method-save_latent_dynamics_files] |  Save diagnostic CSV files to a specified location. |
#'
#'  ## Report run times, console output, return codes
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$time()`][fit-method-time]  |  Report total and chain-specific run times. |
#'  [`$output()`][fit-method-output]  |  Return the stdout and stderr of all chains or pretty print the output for a single chain. |
#'  [`$return_codes()`][fit-method-return_codes]  |  Return the return codes from the CmdStan runs. |
#'
NULL

CmdStanMCMC <- R6::R6Class(
  classname = "CmdStanMCMC",
  inherit = CmdStanFit,
  public = list(
    initialize = function(runset) {
      super$initialize(runset)
      if (!length(self$output_files())) {
        warning("No chains finished successfully. Unable to retrieve the fit.",
                call. = FALSE)
      } else {
        if (self$runset$args$validate_csv) {
          fixed_param <- runset$args$method_args$fixed_param
          data_csv <- read_cmdstan_csv(
            self$output_files(),
            variables = "",
            sampler_diagnostics =
             if (!fixed_param) c("treedepth__", "divergent__") else ""
          )
          if (!fixed_param) {
            check_divergences(data_csv)
            check_sampler_transitions_treedepth(data_csv)
          }
          private$metadata_ <- data_csv$metadata
        }
      }
    },

    num_chains = function() {
      super$num_procs()
    },

    output = function(id = NULL) {
      if (is.null(id)) {
        self$runset$procs$proc_output()
      } else {
        cat(paste(self$runset$procs$proc_output(id), collapse="\n"))
      }
    },

    draws = function(variables = NULL, inc_warmup = FALSE) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("No chains finished successfully. Unable to retrieve the draws.", call. = FALSE)
      }
      if (inc_warmup && !private$metadata_$save_warmup) {
        stop("Warmup draws were requested from a fit object without them! ",
             "Please rerun the model with save_warmup = TRUE.", call. = FALSE)
      }

      to_read <- remaining_columns_to_read(
        requested = variables,
        currently_read = dimnames(private$draws_)$variable,
        all = private$metadata_$model_params
      )
      if (is.null(to_read) || any(nzchar(to_read))) {
        private$read_csv_(variables = to_read, sampler_diagnostics = "")
      }
      if (is.null(variables)) {
        variables <- private$metadata_$model_params
      } else {
        matching_res <- matching_variables(variables, private$metadata_$model_params)
        if (length(matching_res$not_found)) {
          stop("Can't find the following variable(s) in the output: ",
              paste(matching_res$not_found, collapse = ", "), call. = FALSE)
        }
        variables <- matching_res$matching
      }
      if (inc_warmup) {
        posterior::bind_draws(private$warmup_draws_, private$draws_, along="iteration")[,,variables]
      } else {
        private$draws_[,,variables]
      }
    },

    sampler_diagnostics = function(inc_warmup = FALSE) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("No chains finished successfully. Unable to retrieve the sampler diagnostics.", call. = FALSE)
      }
      to_read <- remaining_columns_to_read(
        requested = NULL,
        currently_read = dimnames(private$sampler_diagnostics_)$variable,
        all = private$metadata_$sampler_diagnostics
      )
      if (is.null(to_read) || any(nzchar(to_read))) {
        private$read_csv_(variables = "", sampler_diagnostics = NULL)
      }
      if (inc_warmup) {
        if (!private$metadata_$save_warmup) {
          stop("Warmup sampler diagnostics were requested from a fit object without them! ",
               "Please rerun the model with save_warmup = TRUE.", call. = FALSE)
        }
        posterior::bind_draws(
          private$warmup_sampler_diagnostics_,
          private$sampler_diagnostics_,
          along="iteration"
        )
      } else {
        private$sampler_diagnostics_
      }
    },

    # returns list of inverse metrics
    inv_metric = function(matrix = TRUE) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("No chains finished successfully. Unable to retrieve the inverse metrics.", call. = FALSE)
      }
      if (is.null(private$inv_metric_)) {
        private$read_csv_(variables = "", sampler_diagnostics = "")
      }
      out <- private$inv_metric_
      if (matrix && !is.matrix(out[[1]])) {
        # convert each vector to a diagonal matrix
        out <- lapply(out, diag)
      }
      out
    },
    # approximate loo-cv using the loo package
    loo = function(variables = "log_lik", r_eff = TRUE, ...) {
      if (!requireNamespace("loo", quietly = TRUE)) {
        stop("Please install the loo package to use this method.", call. = FALSE)
      }
      LLarray <- self$draws(variables)
      if (is.logical(r_eff)) {
        if (isTRUE(r_eff)) {
          r_eff_cores <- list(...)[["cores"]] %||% getOption("mc.cores", 1)
          r_eff <- loo::relative_eff(exp(LLarray), cores = r_eff_cores)
        } else {
          r_eff <- NULL
        }
      }
      loo::loo.array(LLarray, r_eff = r_eff, ...)
    }

  ),
  private = list(
    # also inherits draws_ and metadata_ from CmdStanFit
    sampler_diagnostics_ = NULL,
    warmup_sampler_diagnostics_ = NULL,
    warmup_draws_ = NULL,
    inv_metric_ = NULL,
    read_csv_ = function(variables = NULL, sampler_diagnostics = NULL) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("No chains finished successfully. Unable to retrieve the draws.", call. = FALSE)
      }
      data_csv <- read_cmdstan_csv(
        files = self$output_files(include_failed = FALSE),
        variables = variables,
        sampler_diagnostics = sampler_diagnostics
      )
      private$inv_metric_ <- data_csv$inv_metric
      private$metadata_ <- data_csv$metadata

      if (!is.null(data_csv$post_warmup_draws)) {
        missing_variables <- !(posterior::variables(data_csv$post_warmup_draws) %in% posterior::variables(private$draws_))
        private$draws_ <- posterior::bind_draws(
          private$draws_,
          data_csv$post_warmup_draws[,,missing_variables],
          along="variable"
        )
      }
      if (!is.null(data_csv$post_warmup_sampler_diagnostics)) {
        missing_variables <- !(posterior::variables(data_csv$post_warmup_sampler_diagnostics) %in% posterior::variables(private$sampler_diagnostics_))
        private$sampler_diagnostics_ <- posterior::bind_draws(
          private$sampler_diagnostics_,
          data_csv$post_warmup_sampler_diagnostics[,,missing_variables],
          along="variable"
        )
      }
      if (!is.null(data_csv$metadata$save_warmup)
         && data_csv$metadata$save_warmup) {
        if (!is.null(data_csv$warmup_draws)) {
          missing_variables <- !(posterior::variables(data_csv$warmup_draws) %in% posterior::variables(private$warmup_draws_))
          private$warmup_draws_ <- posterior::bind_draws(
            private$warmup_draws_,
            data_csv$warmup_draws[,,missing_variables],
            along="variable"
          )
        }
        if (!is.null(data_csv$warmup_sampler_diagnostics)) {
          missing_variables <- !(posterior::variables(data_csv$warmup_sampler_diagnostics) %in% posterior::variables(private$warmup_sampler_diagnostics_))
          private$warmup_sampler_diagnostics_ <- posterior::bind_draws(
            private$warmup_sampler_diagnostics_,
            data_csv$warmup_sampler_diagnostics[,,missing_variables],
            along="variable"
          )
        }
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
#' @section Methods: `CmdStanMLE` objects have the following associated methods,
#'   all of which have their own (linked) documentation pages.
#'
#'  ## Extract contents of fitted model object
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`draws()`][fit-method-draws]  |  Return the point estimate as a 1-row [`draws_matrix`][posterior::draws_matrix]. |
#'  [`$mle()`][fit-method-mle]  |  Return the point estimate as a numeric vector. |
#'  [`$lp()`][fit-method-lp]  |  Return the total log probability density (`target`). |
#'  [`$init()`][fit-method-init]  |  Return user-specified initial values. |
#'  [`$metadata()`][fit-method-metadata] | Return a list of metadata gathered from the CmdStan CSV files. |
#'
#'  ## Summarize inferences
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$summary()`][fit-method-summary]  |  Run [`posterior::summarise_draws()`][posterior::draws_summary]. |
#'
#'  ## Save fitted model object and temporary files
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$save_object()`][fit-method-save_object] |  Save fitted model object to a file. |
#'  [`$save_output_files()`][fit-method-save_output_files]  |  Save output CSV files to a specified location. |
#'  [`$save_data_file()`][fit-method-save_data_file]  |  Save JSON data file to a specified location. |
#'
#'  ## Report run times, console output, return codes
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$time()`][fit-method-time]      |  Report the total run time. |
#'  [`$output()`][fit-method-output]  |  Pretty print the output that was printed to the console. |
#'  [`$return_codes()`][fit-method-return_codes]  |  Return the return codes from the CmdStan runs. |
#'
NULL

CmdStanMLE <- R6::R6Class(
  classname = "CmdStanMLE",
  inherit = CmdStanFit,
  public = list(
    mle = function(variables = NULL) {
      x <- self$draws(variables)
      x <- x[, colnames(x) != "lp__"]
      estimate <- setNames(as.numeric(x), nm = posterior::variables(x))
      estimate
    }
  ),
  private = list(
    # inherits draws_ and metadata_ slots from CmdStanFit
    read_csv_ = function() {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Optimization failed. Unable to retrieve the draws.", call. = FALSE)
      }
      optim_output <- read_cmdstan_csv(self$output_files())
      private$draws_ <- optim_output$point_estimates
      private$metadata_ <- optim_output$metadata
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
#' @section Methods: `CmdStanVB` objects have the following associated methods,
#'   all of which have their own (linked) documentation pages.
#'
#'  ## Extract contents of fitted model object
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$draws()`][fit-method-draws]  |  Return approximate posterior draws as a [`draws_matrix`][posterior::draws_matrix]. |
#'  [`$lp()`][fit-method-lp]  |  Return the total log probability density (`target`) computed in the model block of the Stan program. |
#'  [`$lp_approx()`][fit-method-lp]  |  Return the log density of the variational approximation to the posterior. |
#'  [`$init()`][fit-method-init] |  Return user-specified initial values. |
#'  [`$metadata()`][fit-method-metadata] | Return a list of metadata gathered from the CmdStan CSV files. |
#'
#'  ## Summarize inferences
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$summary()`][fit-method-summary]  | Run [`posterior::summarise_draws()`][posterior::draws_summary]. |
#'  [`$cmdstan_summary()`][fit-method-cmdstan_summary] |  Run and print CmdStan's `bin/stansummary`. |
#'
#'  ## Save fitted model object and temporary files
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$save_object()`][fit-method-save_object] |  Save fitted model object to a file. |
#'  [`$save_output_files()`][fit-method-save_output_files] |  Save output CSV files to a specified location. |
#'  [`$save_data_file()`][fit-method-save_data_file] |  Save JSON data file to a specified location. |
#'  [`$save_latent_dynamics_files()`][fit-method-save_latent_dynamics_files] |  Save diagnostic CSV files to a specified location. |
#'
#'  ## Report run times, console output, return codes
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$time()`][fit-method-time]  |  Report the total run time. |
#'  [`$output()`][fit-method-output]  |  Pretty print the output that was printed to the console. |
#'  [`$return_codes()`][fit-method-return_codes]  |  Return the return codes from the CmdStan runs. |
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
    # inherits draws_ and metadata_ slots from CmdStanFit
    read_csv_ = function() {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Variational inference failed. Unable to retrieve the draws.", call. = FALSE)
      }
      vb_output <- read_cmdstan_csv(self$output_files())
      private$draws_ <- vb_output$draws
      private$metadata_ <- vb_output$metadata
      invisible(self)
    }
  )
)

# CmdStanGQ ---------------------------------------------------------------

#' CmdStanGQ objects
#'
#' @name CmdStanGQ
#' @family fitted model objects
#' @template seealso-docs
#'
#' @description A `CmdStanGQ` object is the fitted model object returned by the
#'   [`$generate_quantities()`][model-method-generate-quantities] method of a
#'   [`CmdStanModel`] object.
#'
#' @section Methods: `CmdStanGQ` objects have the following associated methods,
#'   all of which have their own (linked) documentation pages.
#'
#'  ## Extract contents of generated quantities object
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$draws()`][fit-method-draws] | Return the generated quantities as a [`draws_array`][posterior::draws_array]. |
#'  [`$metadata()`][fit-method-metadata] | Return a list of metadata gathered from the CmdStan CSV files. |
#'
#'  ## Summarize inferences
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$summary()`][fit-method-summary] | Run [`posterior::summarise_draws()`][posterior::draws_summary]. |
#'
#'  ## Save fitted model object and temporary files
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$save_object()`][fit-method-save_object] | Save fitted model object to a file. |
#'  [`$save_output_files()`][fit-method-save_output_files] | Save output CSV files to a specified location. |
#'  [`$save_data_file()`][fit-method-save_data_file] | Save JSON data file to a specified location. |
#'
#'  ## Report run times, console output, return codes
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$time()`][fit-method-time] | Report the total run time. |
#'  [`$output()`][fit-method-output] | Return the stdout and stderr of all chains or pretty print the output for a single chain. |
#'  [`$return_codes()`][fit-method-return_codes]  |  Return the return codes from the CmdStan runs. |
#'
#' @inherit model-method-generate-quantities examples
#'
CmdStanGQ <- R6::R6Class(
  classname = "CmdStanGQ",
  inherit = CmdStanFit,
  public = list(
    fitted_params_files = function() {
      self$runset$args$method_args$fitted_params
    },
    num_chains = function() {
      super$num_procs()
    },
    draws = function(variables = NULL) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Generating quantities for all MCMC chains failed. Unable to retrieve the generated quantities.", call. = FALSE)
      }
      to_read <- remaining_columns_to_read(
        requested = variables,
        currently_read = dimnames(private$draws_)$variable,
        all = private$metadata_$model_params
      )
      if (is.null(to_read) || any(nzchar(to_read))) {
        private$read_csv_(variables = to_read)
      }
      if (is.null(variables)) {
        variables <- private$metadata_$model_params
      } else {
        matching_res <- matching_variables(variables, private$metadata_$model_params)
        if (length(matching_res$not_found)) {
          stop("Can't find the following variable(s) in the output: ",
              paste(matching_res$not_found, collapse = ", "), call. = FALSE)
        }
        variables <- matching_res$matching
      }
      private$draws_[,,variables]
    },
    output = function(id = NULL) {
      if (is.null(id)) {
        self$runset$procs$proc_output()
      } else {
        cat(paste(self$runset$procs$proc_output(id), collapse="\n"))
      }
    }
  ),
  private = list(
    # inherits draws_ and metadata_ slots from CmdStanFit
    read_csv_ = function(variables = NULL) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Generating quantities for all MCMC chains failed. Unable to retrieve the generated quantities.", call. = FALSE)
      }
      data_csv <- read_cmdstan_csv(
        files = self$output_files(include_failed = FALSE),
        variables = variables,
        sampler_diagnostics = ""
      )
      private$metadata_ <- data_csv$metadata
      if (!is.null(data_csv$generated_quantities)) {
        private$draws_ <-
          posterior::bind_draws(
            private$draws_,
            data_csv$generated_quantities,
            along="variable"
          )
      }
      invisible(self)
    }
  )
)
