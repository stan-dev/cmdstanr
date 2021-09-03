# CmdStanFit ---------------------------------------------------
#' CmdStanFit superclass
#'
#' @noRd
#' @description CmdStanMCMC, CmdStanMLE, CmdStanVB, CmdStanGQ all share the
#'   methods of the superclass CmdStanFit and also have their own unique methods.
#'
CmdStanFit <- R6::R6Class(
  classname = "CmdStanFit",
  public = list(
    runset = NULL,
    initialize = function(runset) {
      checkmate::assert_r6(runset, classes = "CmdStanRun")
      self$runset <- runset
      invisible(self)
    },
    num_procs = function() {
      self$runset$num_procs()
    },
    print = function(variables = NULL, ..., digits = 2, max_rows = getOption("cmdstanr_max_rows", 10)) {
      if (is.null(private$draws_) &&
          !length(self$output_files(include_failed = FALSE))) {
        stop("Fitting failed. Unable to print.", call. = FALSE)
      }

      # filter variables before passing to summary to avoid computing anything
      # that won't be printed because of max_rows
      all_variables <- self$metadata()$variables
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
      out[,  1] <- format(out[, 1], justify = "left")
      out[, -1] <- format(round(out[, -1], digits = digits), nsmall = digits)
      for (col in grep("ess_", colnames(out), value = TRUE)) {
        out[[col]] <- as.integer(out[[col]])
      }

      opts <- options(max.print = prod(dim(out)))
      on.exit(options(max.print = opts$max.print), add = TRUE)
      base::print(out, row.names = FALSE)
      if (max_rows < total_rows) {
        cat("\n # showing", max_rows, "of", total_rows,
            "rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)\n")
      }
      invisible(self)
    }
  ),
  private = list(
    draws_ = NULL,
    metadata_ = NULL,
    init_ = NULL,
    profiles_ = NULL
  )
)

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
save_object <- function(file, ...) {
  self$draws()
  try(self$sampler_diagnostics(), silent = TRUE)
  try(self$init(), silent = TRUE)
  try(self$profiles(), silent = TRUE)
  saveRDS(self, file = file, ...)
  invisible(self)
}
CmdStanFit$set("public", name = "save_object", value = save_object)

#' Extract posterior draws
#'
#' @name fit-method-draws
#' @aliases draws
#' @description Extract posterior draws after MCMC or approximate posterior
#'   draws after variational approximation using formats provided by the
#'   \pkg{posterior} package.
#'
#'   The variables include the parameters, transformed parameters, and
#'   generated quantities from the Stan program as well as `lp__`, the total
#'   log probability (`target`) accumulated in the model block.
#'
#' @inheritParams read_cmdstan_csv
#' @param inc_warmup (logical) Should warmup draws be included? Defaults to
#'   `FALSE`. Ignored except when used with [CmdStanMCMC] objects.
#' @param format (string) The format of the returned draws or point estimates.
#'   Must be a valid format from the \pkg{posterior} package. The defaults
#'   are the following.
#'
#'   * For sampling and generated quantities the default is
#'   [`"draws_array"`][posterior::draws_array]. This format keeps the chains
#'   separate. To combine the chains use any of the other formats (e.g.
#'   `"draws_matrix"`).
#'
#'   * For point estimates from optimization and approximate draws from
#'   variational inference the default is
#'   [`"draws_matrix"`][posterior::draws_array].
#'
#'   To use a different format it can be specified as the full name of the
#'   format from the \pkg{posterior} package (e.g. `format = "draws_df"`) or
#'   omitting the `"draws_"` prefix (e.g. `format = "df"`).
#'
#'   **Changing the default format**: To change the default format for an entire
#'   R session use `options(cmdstanr_draws_format = format)`, where `format` is
#'   the name (in quotes) of a valid format from the posterior package. For
#'   example `options(cmdstanr_draws_format = "draws_df")` will change the
#'   default to a data frame.
#'
#'   **Note about efficiency**: For models with a large number of parameters
#'   (20k+) we recommend using the `"draws_list"` format, which is the most
#'   efficient and RAM friendly when combining draws from multiple chains. If
#'   speed or memory is not a constraint we recommend selecting the format that
#'   most suits the coding style of the post processing phase.
#'
#' @return
#' Depends on the value of `format`. The defaults are:
#' * For [MCMC][model-method-sample], a 3-D
#' [`draws_array`][posterior::draws_array] object (iteration x chain x
#' variable).
#' * For standalone [generated quantities][model-method-generate-quantities], a
#' 3-D [`draws_array`][posterior::draws_array] object (iteration x chain x
#' variable).
#' * For [variational inference][model-method-variational], a 2-D
#' [`draws_matrix`][posterior::draws_matrix] object (draw x variable) because
#' there are no chains. An additional variable `lp_approx__` is also included,
#' which is the log density of the variational approximation to the posterior
#' evaluated at each of the draws.
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
#' # logistic regression with intercept alpha and coefficients beta
#' fit <- cmdstanr_example("logistic", method = "sample")
#'
#' # returned as 3-D array (see ?posterior::draws_array)
#' draws <- fit$draws()
#' dim(draws)
#' str(draws)
#'
#' # can easily convert to other formats (data frame, matrix, list)
#' # using the posterior package
#' head(posterior::as_draws_matrix(draws))
#'
#' # or can specify 'format' argument to avoid manual conversion
#' # matrix format combines all chains
#' draws <- fit$draws(format = "matrix")
#' head(draws)
#'
#' # can select specific parameters
#' fit$draws("alpha")
#' fit$draws("beta")  # selects entire vector beta
#' fit$draws(c("alpha", "beta[2]"))
#'
#' # can be passed directly to bayesplot plotting functions
#' bayesplot::color_scheme_set("brightblue")
#' bayesplot::mcmc_dens(fit$draws(c("alpha", "beta")))
#' bayesplot::mcmc_scatter(fit$draws(c("beta[1]", "beta[2]")), alpha = 0.3)
#'
#'
#' # example using variational inference
#' fit <- cmdstanr_example("logistic", method = "variational")
#' head(fit$draws("beta")) # a matrix by default
#' head(fit$draws("beta", format = "df"))
#' }
#'
draws <- function(variables = NULL, inc_warmup = FALSE, format = getOption("cmdstanr_draws_format")) {
  # CmdStanMCMC and CmdStanGQ have separate implementations,
  # this is used for CmdStanVB and CmdStanMLE
  if (is.null(format)) {
    format <- "draws_matrix"
  } else {
    format <- assert_valid_draws_format(format)
  }
  if (!length(self$output_files(include_failed = FALSE))) {
    stop("Fitting failed. Unable to retrieve the draws.", call. = FALSE)
  }
  if (inc_warmup) {
    warning("'inc_warmup' is ignored except when used with CmdStanMCMC objects.",
            call. = FALSE)
  }
  if (is.null(private$draws_)) {
    private$read_csv_(format = format)
  }
  private$draws_ <- maybe_convert_draws_format(private$draws_, format)
  posterior::subset_draws(private$draws_, variable = variables)
}
CmdStanFit$set("public", name = "draws", value = draws)

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
#' @return A list of lists. See **Examples**.
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
init <- function() {
  if (is.null(private$init_)) {
    init_paths <- self$metadata()$init
    if (!is.character(init_paths) || any(!file.exists(init_paths))) {
      stop("Can't find initial values files.", call. = FALSE)
    }
    private$init_ <- lapply(init_paths, jsonlite::read_json, simplifyVector = TRUE)
  }
  private$init_
}
CmdStanFit$set("public", name = "init", value = init)

#' Extract log probability (target)
#'
#' @name fit-method-lp
#' @aliases lp lp_approx
#' @description The `$lp()` method extracts `lp__`, the total log probability
#'   (`target`) accumulated in the model block of the Stan program. For
#'   variational inference the log density of the variational approximation to
#'   the posterior is also available via the `$lp_approx()` method.
#'
#'   See the [Log Probability Increment vs. Sampling
#'   Statement](https://mc-stan.org/docs/2_23/reference-manual/sampling-statements-section.html)
#'   section of the Stan Reference Manual for details on when normalizing
#'   constants are dropped from log probability calculations.
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
#' @return A numeric vector with length equal to the number of (post-warmup)
#'   draws for MCMC and variational inference, and length equal to `1` for
#'   optimization.
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
lp <- function() {
  lp__ <- self$draws(variables = "lp__")
  lp__ <- posterior::as_draws_matrix(lp__) # if mcmc this combines all chains, otherwise does nothing
  as.numeric(lp__)
}
CmdStanFit$set("public", name = "lp", value = lp)

#' Compute a summary table of estimates and diagnostics
#'
#' @name fit-method-summary
#' @aliases summary fit-method-print print.CmdStanMCMC print.CmdStanMLE print.CmdStanVB
#' @description The `$summary()` method runs
#'   [`summarise_draws()`][posterior::draws_summary] from the \pkg{posterior}
#'   package and returns the output. For MCMC, only post-warmup draws are
#'   included in the summary.
#'
#'   There is also a `$print()` method that prints the same summary stats but
#'   removes the extra formatting used for printing tibbles and returns the
#'   fitted model object itself. The `$print()` method may also be faster than
#'   `$summary()` because it is designed to only compute the summary statistics
#'   for the variables that will actually fit in the printed output whereas
#'   `$summary()` will compute them for all of the specified variables in order
#'   to be able to return them to the user. See **Examples**.
#'
#' @param variables (character vector) The variables to include.
#' @param ... Optional arguments to pass to [`posterior::summarise_draws()`][posterior::draws_summary].
#'
#' @return
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
summary <- function(variables = NULL, ...) {
  draws <- self$draws(variables)
  if (self$metadata()$method == "sample") {
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
  if (self$metadata()$method == "optimize") {
    summary <- summary[, c("variable", "mean")]
    colnames(summary) <- c("variable", "estimate")
  }
  summary
}
CmdStanFit$set("public", name = "summary", value = summary)

#' Run CmdStan's `stansummary` and `diagnose` utilities
#'
#' @name fit-method-cmdstan_summary
#' @aliases fit-method-cmdstan_diagnose cmdstan_summary cmdstan_diagnose
#' @description Run CmdStan's `stansummary` and `diagnose` utilities. These are
#'   documented in the CmdStan Guide:
#'   * https://mc-stan.org/docs/cmdstan-guide/stansummary.html
#'   * https://mc-stan.org/docs/cmdstan-guide/diagnose.html
#'
#'   Although these methods can be used for models fit using the
#'   [`$variational()`][model-method-variational] method, much of the output is
#'   currently only relevant for models fit using the
#'   [`$sample()`][model-method-sample] method.
#'
#'   See the [$summary()][fit-method-summary] for computing similar summaries in
#'   R rather than calling CmdStan's utilites.
#'
#' @param flags An optional character vector of flags (e.g.
#'   `flags = c("--sig_figs=1")`).
#'
#' @seealso [`CmdStanMCMC`], [fit-method-summary]
#'
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic")
#' fit$cmdstan_diagnose()
#' fit$cmdstan_summary()
#' }
#'
cmdstan_summary <- function(flags = NULL) {
  self$runset$run_cmdstan_tool("stansummary", flags = flags)
}
CmdStanFit$set("public", name = "cmdstan_summary", value = cmdstan_summary)

#' @rdname fit-method-cmdstan_summary
cmdstan_diagnose <- function() {
  self$runset$run_cmdstan_tool("diagnose")
}
CmdStanFit$set("public", name = "cmdstan_diagnose", value = cmdstan_diagnose)

#' Save output and data files
#'
#' @name fit-method-save_output_files
#' @aliases fit-method-save_data_file fit-method-save_latent_dynamics_files fit-method-save_profile_files
#'   fit-method-output_files fit-method-data_file fit-method-latent_dynamics_files fit-method-profile_files
#'   save_output_files save_data_file save_latent_dynamics_files save_profile_files
#'   output_files data_file latent_dynamics_files profile_files
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
#' For `$save_profile_files()` everything is the same as for
#' `$save_output_files()` except `"-profile-"` is included in the new
#' file name after `basename`.
#'
#' For `$save_data_file()` no `id` is included in the file name because even
#' with multiple MCMC chains the data file is the same.
#'
#' @return
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
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example()
#' fit$output_files()
#' fit$data_file()
#'
#' # just using tempdir for the example
#' my_dir <- tempdir()
#' fit$save_output_files(dir = my_dir, basename = "banana")
#' fit$save_output_files(dir = my_dir, basename = "tomato", timestamp = FALSE)
#' fit$save_output_files(dir = my_dir, basename = "lettuce", timestamp = FALSE, random = FALSE)
#' }
#'
save_output_files <- function(dir = ".",
                              basename = NULL,
                              timestamp = TRUE,
                              random = TRUE) {
  self$runset$save_output_files(dir, basename, timestamp, random)
}
CmdStanFit$set("public", name = "save_output_files", value = save_output_files)

#' @rdname fit-method-save_output_files
save_latent_dynamics_files <- function(dir = ".",
                                       basename = NULL,
                                       timestamp = TRUE,
                                       random = TRUE) {
  self$runset$save_latent_dynamics_files(dir, basename, timestamp, random)
}
CmdStanFit$set("public", name = "save_latent_dynamics_files", value = save_latent_dynamics_files)

#' @rdname fit-method-save_output_files
save_profile_files <- function(dir = ".",
                               basename = NULL,
                               timestamp = TRUE,
                               random = TRUE) {
  self$runset$save_profile_files(dir, basename, timestamp, random)
}
CmdStanFit$set("public", name = "save_profile_files", value = save_profile_files)

#' @rdname fit-method-save_output_files
save_data_file <- function(dir = ".",
                           basename = NULL,
                           timestamp = TRUE,
                           random = TRUE) {
  self$runset$save_data_file(dir, basename, timestamp, random)
}
CmdStanFit$set("public", name = "save_data_file", value = save_data_file)

#' @rdname fit-method-save_output_files
#' @param include_failed (logical) Should CmdStan runs that failed also be
#'   included? The default is `FALSE.`
output_files <- function(include_failed = FALSE) {
  self$runset$output_files(include_failed)
}
CmdStanFit$set("public", name = "output_files", value = output_files)

#' @rdname fit-method-save_output_files
profile_files <- function(include_failed = FALSE) {
  self$runset$profile_files(include_failed)
}
CmdStanFit$set("public", name = "profile_files", value = profile_files)

#' @rdname fit-method-save_output_files
latent_dynamics_files <- function(include_failed = FALSE) {
  self$runset$latent_dynamics_files(include_failed)
}
CmdStanFit$set("public", name = "latent_dynamics_files", value = latent_dynamics_files)

#' @rdname fit-method-save_output_files
data_file <- function() {
  self$runset$data_file()
}
CmdStanFit$set("public", name = "data_file", value = data_file)


#' Report timing of CmdStan runs
#'
#' @name fit-method-time
#' @aliases time
#' @description Report the run time in seconds. For MCMC additional information
#'   is provided about the run times of individual chains and the warmup and
#'   sampling phases.
#'
#' @return
#' A list with elements
#' * `total`: (scalar) The total run time. For MCMC this may be different than
#' the sum of the chain run times if parallelization was used.
#' * `chains`: (data frame) For MCMC only, timing info for the individual
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
time <- function() {
  self$runset$time()
}
CmdStanFit$set("public", name = "time", value = time)

#' Access console output
#'
#' @name fit-method-output
#' @aliases output
#' @description For MCMC, the `$output()` method returns the stdout and stderr
#'   of all chains as a list of character vectors if `id=NULL`. If the `id`
#'   argument is specified it instead pretty prints the console output for a
#'   single chain.
#'
#'   For optimization and variational inference `$output()` just pretty prints
#'   the console output.
#'
#' @param id (integer) The chain id. Ignored if the model was not fit using
#'   MCMC.
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
output <- function(id = NULL) {
  # MCMC has separate implementation but doc is shared
  # Non-MCMC fit is obtained with one process only so id is ignored
  cat(paste(self$runset$procs$proc_output(1), collapse = "\n"))
}
CmdStanFit$set("public", name = "output", value = output)

#' Extract metadata from CmdStan CSV files
#'
#' @name fit-method-metadata
#' @aliases metadata
#' @description The `$metadata()` method returns a list of information gathered
#'   from the CSV output files, including the CmdStan configuration used when
#'   fitting the model. See **Examples** and [read_cmdstan_csv()].
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
metadata <- function() {
  if (is.null(private$metadata_)) {
    if (!length(self$output_files(include_failed = FALSE))) {
      stop("Fitting failed. Unable to retrieve the metadata.", call. = FALSE)
    }
    private$read_csv_()
  }
  private$metadata_
}
CmdStanFit$set("public", name = "metadata", value = metadata)

#' Extract return codes from CmdStan
#'
#' @name fit-method-return_codes
#' @aliases return_codes
#' @description The `$return_codes()` method returns a vector of return codes
#'   from the CmdStan run(s). A return code of 0 indicates a successful run.
#' @return An integer vector of return codes with length equal to the number of
#'   CmdStan runs (number of chains for MCMC and one otherwise).
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
return_codes <- function() {
  self$runset$procs$return_codes()
}
CmdStanFit$set("public", name = "return_codes", value = return_codes)

#' Return profiling data
#'
#' @name fit-method-profiles
#' @aliases profiles
#' @description The `$profiles()` method returns a list of data frames with
#'   profiling data if any profiling data was written to the profile CSV files.
#'   See [save_profile_files()] to control where the files are saved.
#'
#'   Support for profiling Stan programs is available with CmdStan >= 2.26 and
#'   requires adding profiling statements to the Stan program.
#'
#' @return A list of data frames with profiling data if the profiling CSV files
#'   were created.
#'
#' @seealso [`CmdStanMCMC`], [`CmdStanMLE`], [`CmdStanVB`], [`CmdStanGQ`]
#'
#' @examples
#'
#' \dontrun{
#' # first fit a model using MCMC
#' mcmc_program <- write_stan_file(
#'   'data {
#'     int<lower=0> N;
#'     int<lower=0,upper=1> y[N];
#'   }
#'   parameters {
#'     real<lower=0,upper=1> theta;
#'   }
#'   model {
#'     profile("likelihood") {
#'       y ~ bernoulli(theta);
#'     }
#'   }
#'   generated quantities {
#'     int y_rep[N];
#'     profile("gq") {
#'       y_rep = bernoulli_rng(rep_vector(theta, N));
#'     }
#'   }
#' '
#' )
#' mod_mcmc <- cmdstan_model(mcmc_program)
#'
#' data <- list(N = 10, y = c(1,1,0,0,0,1,0,1,0,0))
#' fit <- mod_mcmc$sample(data = data, seed = 123, refresh = 0)
#'
#' fit$profiles()
#' }
#'
profiles <- function() {
  if (is.null(private$profiles_)) {
    private$profiles_ <- list()
    i <- 1
    for (f in self$profile_files()) {
      private$profiles_[[i]] <- data.table::fread(f, integer64 = "character", data.table = FALSE)
      i <- i + 1
    }
  }  
  private$profiles_
}
CmdStanFit$set("public", name = "profiles", value = profiles)

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
#'  [`$draws()`][fit-method-draws] |  Return posterior draws using formats from the \pkg{posterior} package. |
#'  [`$sampler_diagnostics()`][fit-method-sampler_diagnostics] |  Return sampler diagnostics as a [`draws_array`][posterior::draws_array]. |
#'  [`$lp()`][fit-method-lp] |  Return the total log probability density (`target`). |
#'  [`$inv_metric()`][fit-method-inv_metric] |  Return the inverse metric for each chain. |
#'  [`$init()`][fit-method-init] |  Return user-specified initial values. |
#'  [`$metadata()`][fit-method-metadata] | Return a list of metadata gathered from the CmdStan CSV files. |
#'  [`$num_chains()`][fit-method-num_chains] | Returns the number of MCMC chains. |
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
#'  [`$output()`][fit-method-output]  |  Return the stdout and stderr of all chains or pretty print the output for a single chain. |
#'  [`$time()`][fit-method-time]  |  Report total and chain-specific run times. |
#'  [`$return_codes()`][fit-method-return_codes]  |  Return the return codes from the CmdStan runs. |
#'
CmdStanMCMC <- R6::R6Class(
  classname = "CmdStanMCMC",
  inherit = CmdStanFit,
  public = list(
    # override the CmdStanFit initialize method
    initialize = function(runset) {
      super$initialize(runset)
      if (!length(self$output_files())) {
        warning("No chains finished successfully. Unable to retrieve the fit.",
                call. = FALSE)
      } else {
        if (self$runset$args$validate_csv) {
          fixed_param <- runset$args$method_args$fixed_param
          private$read_csv_(variables = "",
                           sampler_diagnostics = if (!fixed_param) c("treedepth__", "divergent__") else "")
          if (!fixed_param) {
            check_divergences(private$sampler_diagnostics_)
            check_sampler_transitions_treedepth(private$sampler_diagnostics_, private$metadata_)
          }
        }
      }
    },
    # override the CmdStanFit output method
    output = function(id = NULL) {
      if (is.null(id)) {
        self$runset$procs$proc_output()
      } else {
        cat(paste(self$runset$procs$proc_output(id), collapse = "\n"))
      }
    },

    # override the CmdStanFit draws method
    draws = function(variables = NULL, inc_warmup = FALSE, format = getOption("cmdstanr_draws_format", "draws_array")) {
      if (inc_warmup && !private$metadata_$save_warmup) {
        stop("Warmup draws were requested from a fit object without them! ",
             "Please rerun the model with save_warmup = TRUE.", call. = FALSE)
      }
      format <- assert_valid_draws_format(format)
      to_read <- remaining_columns_to_read(
        requested = variables,
        currently_read = posterior::variables(private$draws_),
        all = private$metadata_$variables
      )
      private$draws_ <- maybe_convert_draws_format(private$draws_, format)
      private$warmup_draws_ <- maybe_convert_draws_format(private$warmup_draws_, format)
      private$sampler_diagnostics_ <- maybe_convert_draws_format(private$sampler_diagnostics_, format)
      private$warmup_sampler_diagnostics_ <- maybe_convert_draws_format(private$warmup_sampler_diagnostics_, format)
      if (is.null(to_read) || any(nzchar(to_read))) {
        private$read_csv_(variables = to_read, sampler_diagnostics = "", format = format)
      }
      if (is.null(variables)) {
        variables <- private$metadata_$variables
      } else {
        matching_res <- matching_variables(variables, private$metadata_$variables)
        if (length(matching_res$not_found)) {
          stop("Can't find the following variable(s) in the output: ",
              paste(matching_res$not_found, collapse = ", "), call. = FALSE)
        }
        variables <- matching_res$matching
      }
      if (inc_warmup) {
        posterior::subset_draws(posterior::bind_draws(private$warmup_draws_, private$draws_, along = "iteration"), variable = variables)
      } else {
        posterior::subset_draws(private$draws_, variable = variables)
      }
    }
  ),
  private = list(
    # also inherits draws_ and metadata_ from CmdStanFit
    sampler_diagnostics_ = NULL,
    warmup_sampler_diagnostics_ = NULL,
    warmup_draws_ = NULL,
    inv_metric_ = NULL,
    read_csv_ = function(variables = NULL, sampler_diagnostics = NULL, format = getOption("cmdstanr_draws_format", "draws_array")) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("No chains finished successfully. Unable to retrieve the draws.", call. = FALSE)
      }
      csv_contents <- read_cmdstan_csv(
        files = self$output_files(include_failed = FALSE),
        variables = variables,
        sampler_diagnostics = sampler_diagnostics,
        format = format
      )
      private$inv_metric_ <- csv_contents$inv_metric
      private$metadata_ <- csv_contents$metadata

      if (!is.null(csv_contents$post_warmup_draws)) {
        if (is.null(private$draws_)) {
          private$draws_ <- csv_contents$post_warmup_draws
        } else {
          missing_variables <- posterior::variables(csv_contents$post_warmup_draws)[!(posterior::variables(csv_contents$post_warmup_draws) %in% posterior::variables(private$draws_))]
          private$draws_ <- posterior::bind_draws(
            private$draws_,
            posterior::subset_draws(csv_contents$post_warmup_draws, variable = missing_variables),
            along = "variable"
          )
        }
      }
      if (!is.null(csv_contents$post_warmup_sampler_diagnostics)) {

        if (is.null(private$sampler_diagnostics_)) {
          private$sampler_diagnostics_ <- csv_contents$post_warmup_sampler_diagnostics
        } else {
          missing_variables <- posterior::variables(csv_contents$post_warmup_sampler_diagnostics)[!(posterior::variables(csv_contents$post_warmup_sampler_diagnostics) %in% posterior::variables(private$sampler_diagnostics_))]
          private$sampler_diagnostics_ <- posterior::bind_draws(
            private$sampler_diagnostics_,
            posterior::subset_draws(csv_contents$post_warmup_sampler_diagnostics, variable = missing_variables),
            along = "variable"
          )
        }
      }
      if (!is.null(csv_contents$metadata$save_warmup)
         && csv_contents$metadata$save_warmup) {
        if (!is.null(csv_contents$warmup_draws)) {
          if (is.null(private$warmup_draws_)) {
            private$warmup_draws_ <- csv_contents$warmup_draws
          } else {
            missing_variables <- posterior::variables(csv_contents$warmup_draws)[!(posterior::variables(csv_contents$warmup_draws) %in% posterior::variables(private$warmup_draws_))]
            private$warmup_draws_ <- posterior::bind_draws(
              private$warmup_draws_,
              posterior::subset_draws(csv_contents$warmup_draws, variable = missing_variables),
              along = "variable"
            )
          }
        }
        if (!is.null(csv_contents$warmup_sampler_diagnostics)) {
          if (is.null(private$warmup_sampler_diagnostics_)) {
            private$warmup_sampler_diagnostics_ <- csv_contents$warmup_sampler_diagnostics
          } else {
            missing_variables <- posterior::variables(csv_contents$warmup_sampler_diagnostics)[!(posterior::variables(csv_contents$warmup_sampler_diagnostics) %in% posterior::variables(private$warmup_sampler_diagnostics_))]
            private$warmup_sampler_diagnostics_ <- posterior::bind_draws(
              private$warmup_sampler_diagnostics_,
              posterior::subset_draws(csv_contents$warmup_sampler_diagnostics, variable = missing_variables),
              along = "variable"
            )
          }
        }
      }
      invisible(self)
    }
  )
)

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
#' @param variables (character vector) The name(s) of the variable(s) in the
#'   Stan program containing the pointwise log-likelihood. The default is to
#'   look for `"log_lik"`. This argument is passed to the
#'   [`$draws()`][fit-method-draws] method.
#' @param r_eff (multiple options) How to handle the `r_eff` argument for `loo()`:
#'   * `TRUE` (the default) will automatically call [loo::relative_eff.array()]
#'   to compute the `r_eff` argument to pass to [loo::loo.array()].
#'   * `FALSE` or `NULL` will avoid computing `r_eff` (which can sometimes be slow)
#'   but will result in a warning from the \pkg{loo} package.
#'   * If `r_eff` is anything else, that object will be passed as the `r_eff`
#'   argument to [loo::loo.array()].
#' @param ... Other arguments (e.g., `cores`, `save_psis`, etc.) passed to
#'   [loo::loo.array()].
#'
#' @return The object returned by [loo::loo.array()].
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
#' loo_result <- fit$loo(cores = 2)
#' print(loo_result)
#' }
#'
loo <- function(variables = "log_lik", r_eff = TRUE, ...) {
  require_suggested_package("loo")
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
CmdStanMCMC$set("public", name = "loo", value = loo)

#' Extract sampler diagnostics after MCMC
#'
#' @name fit-method-sampler_diagnostics
#' @aliases sampler_diagnostics
#' @description Extract the values of sampler diagnostics for each iteration and
#'   chain of MCMC.
#'
#' @param inc_warmup (logical) Should warmup draws be included? Defaults to `FALSE`.
#' @param format (string) The draws format to return. See
#'   [draws][fit-method-draws] for details.
#'
#' @return
#' Depends on `format`, but the default is a 3-D
#' [`draws_array`][posterior::draws_array] object (iteration x chain x
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
#'
#' # or specify format to get a data frame instead of calling as_draws_df
#' fit$sampler_diagnostics(format = "df")
#' }
#'
sampler_diagnostics <- function(inc_warmup = FALSE, format = getOption("cmdstanr_draws_format", "draws_array")) {
  if (is.null(private$sampler_diagnostics_) &&
      !length(self$output_files(include_failed = FALSE))) {
    stop("No chains finished successfully. Unable to retrieve the sampler diagnostics.", call. = FALSE)
  }
  to_read <- remaining_columns_to_read(
    requested = NULL,
    currently_read = posterior::variables(private$sampler_diagnostics_),
    all = private$metadata_$sampler_diagnostics
  )
  private$warmup_sampler_diagnostics_ <- maybe_convert_draws_format(private$warmup_sampler_diagnostics_, format)
  private$sampler_diagnostics_ <- maybe_convert_draws_format(private$sampler_diagnostics_, format)
  if (is.null(to_read) || any(nzchar(to_read))) {
    private$read_csv_(variables = "", sampler_diagnostics = NULL, format = format)
  }
  if (inc_warmup) {
    if (!private$metadata_$save_warmup) {
      stop("Warmup sampler diagnostics were requested from a fit object without them! ",
           "Please rerun the model with save_warmup = TRUE.", call. = FALSE)
    }
    posterior::bind_draws(
      private$warmup_sampler_diagnostics_,
      private$sampler_diagnostics_,
      along = "iteration"
    )
  } else {
    private$sampler_diagnostics_
  }
}
CmdStanMCMC$set("public", name = "sampler_diagnostics", value = sampler_diagnostics)

#' Extract inverse metric (mass matrix) after MCMC
#'
#' @name fit-method-inv_metric
#' @aliases inv_metric
#' @description Extract the inverse metric (mass matrix) for each MCMC chain.
#'
#' @param matrix (logical) If a diagonal metric was used, setting `matrix =
#'   FALSE` returns a list containing just the diagonals of the matrices instead
#'   of the full matrices. Setting `matrix = FALSE` has no effect for dense
#'   metrics.
#'
#' @return A list of length equal to the number of MCMC chains. See the `matrix`
#'   argument for details.
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
inv_metric <- function(matrix = TRUE) {
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
}
CmdStanMCMC$set("public", name = "inv_metric", value = inv_metric)

#' Extract number of chains after MCMC
#'
#' @name fit-method-num_chains
#' @aliases num_chains
#' @description The `$num_chains()` method returns the number of MCMC chains.
#' @return An integer.
#'
#' @seealso [`CmdStanMCMC`]
#'
#' @examples
#' \dontrun{
#' fit_mcmc <- cmdstanr_example(chains = 2)
#' fit_mcmc$num_chains()
#' }
#'
num_chains <- function() {
  super$num_procs()
}
CmdStanMCMC$set("public", name = "num_chains", value = num_chains)


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
CmdStanMLE <- R6::R6Class(
  classname = "CmdStanMLE",
  inherit = CmdStanFit,
  public = list(),
  private = list(
    # inherits draws_ and metadata_ slots from CmdStanFit
    read_csv_ = function(format = getOption("cmdstanr_draws_format", "draws_matrix")) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Optimization failed. Unable to retrieve the draws.", call. = FALSE)
      }
      csv_contents <- read_cmdstan_csv(self$output_files(), format = format)
      private$draws_ <- csv_contents$point_estimates
      private$metadata_ <- csv_contents$metadata
      invisible(self)
    }
  )
)

#' Extract (penalized) maximum likelihood estimate after optimization
#'
#' @name fit-method-mle
#' @aliases mle
#' @description The `$mle()` method is only available for [`CmdStanMLE`] objects.
#' It returns the penalized maximum likelihood estimate (posterior mode) as a
#' numeric vector with one element per variable. The returned vector does *not*
#' include `lp__`, the total log probability (`target`) accumulated in the
#' model block of the Stan program, which is available via the
#' [`$lp()`][fit-method-lp] method and also included in the
#' [`$draws()`][fit-method-draws] method.
#'
#' @param variables (character vector) The variables (parameters, transformed
#'   parameters, and generated quantities) to include. If NULL (the default)
#'   then all variables are included.
#'
#' @return A numeric vector. See **Examples**.
#'
#' @seealso [`CmdStanMLE`]
#'
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic", method = "optimize")
#' fit$mle("alpha")
#' fit$mle("beta")
#' fit$mle("beta[2]")
#' }
#'
mle <- function(variables = NULL) {
  x <- self$draws(variables)
  x <- x[, colnames(x) != "lp__"]
  stats::setNames(as.numeric(x), posterior::variables(x))
}
CmdStanMLE$set("public", name = "mle", value = mle)


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
CmdStanVB <- R6::R6Class(
  classname = "CmdStanVB",
  inherit = CmdStanFit,
  public = list(),
  private = list(
    # inherits draws_ and metadata_ slots from CmdStanFit
    read_csv_ = function(format = getOption("cmdstanr_draws_format", "draws_matrix")) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Variational inference failed. Unable to retrieve the draws.", call. = FALSE)
      }
      csv_contents <- read_cmdstan_csv(self$output_files(), format = format)
      private$draws_ <- csv_contents$draws
      private$metadata_ <- csv_contents$metadata
      invisible(self)
    }
  )
)

#' @rdname fit-method-lp
lp_approx <- function() {
  as.numeric(self$draws()[, "lp_approx__"])
}
CmdStanVB$set("public", name = "lp_approx", value = lp_approx)


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
    # override CmdStanFit draws method
    draws = function(variables = NULL, inc_warmup = FALSE, format = getOption("cmdstanr_draws_format", "draws_array")) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Generating quantities for all MCMC chains failed. Unable to retrieve the generated quantities.", call. = FALSE)
      }
      if (inc_warmup) {
        warning("'inc_warmup' is ignored except when used with CmdStanMCMC objects.",
                call. = FALSE)
      }
      format <- assert_valid_draws_format(format)
      to_read <- remaining_columns_to_read(
        requested = variables,
        currently_read = dimnames(private$draws_)$variable,
        all = private$metadata_$variables
      )
      private$draws_ <- maybe_convert_draws_format(private$draws_, format)
      if (is.null(to_read) || any(nzchar(to_read))) {
        private$read_csv_(variables = to_read, format = format)
      }
      if (is.null(variables)) {
        variables <- private$metadata_$variables
      } else {
        matching_res <- matching_variables(variables, private$metadata_$variables)
        if (length(matching_res$not_found)) {
          stop("Can't find the following variable(s) in the output: ",
              paste(matching_res$not_found, collapse = ", "), call. = FALSE)
        }
        variables <- matching_res$matching
      }
      posterior::subset_draws(private$draws_, variable = variables)
    },
    # override CmdStanFit output method
    output = function(id = NULL) {
      if (is.null(id)) {
        self$runset$procs$proc_output()
      } else {
        cat(paste(self$runset$procs$proc_output(id), collapse = "\n"))
      }
    }
  ),
  private = list(
    # inherits draws_ and metadata_ slots from CmdStanFit
    read_csv_ = function(variables = NULL, format = getOption("cmdstanr_draws_format", "draws_array")) {
      if (!length(self$output_files(include_failed = FALSE))) {
        stop("Generating quantities for all MCMC chains failed. Unable to retrieve the generated quantities.", call. = FALSE)
      }
      csv_contents <- read_cmdstan_csv(
        files = self$output_files(include_failed = FALSE),
        variables = variables,
        sampler_diagnostics = "",
        format = format
      )
      private$metadata_ <- csv_contents$metadata
      if (!is.null(csv_contents$generated_quantities)) {
        missing_variables <- posterior::variables(csv_contents$generated_quantities)[!(posterior::variables(csv_contents$generated_quantities) %in% posterior::variables(private$draws_))]
        private$draws_ <-
          posterior::bind_draws(
            private$draws_,
            posterior::subset_draws(csv_contents$generated_quantities, variable = missing_variables),
            along = "variable"
          )
      }
      invisible(self)
    }
  )
)


# CmdStan Diagnose --------------------------------------------------------
#' CmdStanDiagnose objects
#'
#' @name CmdStanDiagnose
#' @family fitted model objects
#' @template seealso-docs
#'
#' @description A `CmdStanDiagnose` object is the object returned by the
#'   [`$diagnose()`][model-method-diagnose] method of a [`CmdStanModel`] object.
#'
#' @section Methods: `CmdStanDiagnose` objects have the following associated
#'   methods:
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$gradients()`][fit-method-gradients] |  Return gradients from diagnostic mode. |
#'  [`$lp()`][fit-method-lp] |  Return the total log probability density (`target`). |
#'  [`$init()`][fit-method-init] |  Return user-specified initial values. |
#'  [`$metadata()`][fit-method-metadata] | Return a list of metadata gathered from the CmdStan CSV files. |
#'  [`$save_output_files()`][fit-method-save_output_files] |  Save output CSV files to a specified location. |
#'  [`$save_data_file()`][fit-method-save_data_file] |  Save JSON data file to a specified location. |
#'
#' @examples
#' \dontrun{
#' test <- cmdstanr_example("logistic", method = "diagnose")
#'
#' # retrieve the gradients
#' test$gradients()
#' }
#'
CmdStanDiagnose <- R6::R6Class(
  classname = "CmdStanDiagnose",
  public = list(
    runset = NULL,
    initialize = function(runset) {
      checkmate::assert_r6(runset, classes = "CmdStanRun")
      self$runset <- runset
      csv_data <- read_cmdstan_csv(self$runset$output_files())
      private$metadata_ <- csv_data$metadata
      private$gradients_ <- csv_data$gradients
      private$lp_ <- csv_data$lp
      invisible(self)
    },
    metadata = function() {
      private$metadata_
    }
  ),
  private = list(
    metadata_ = NULL,
    gradients_ = NULL,
    lp_ = NULL,
    init_ = NULL
  )
)

#' Extract gradients after diagnostic mode
#'
#' @name fit-method-gradients
#' @aliases gradients
#' @description Return the data frame containing the gradients for all
#'   parameters.
#'
#' @return A list of lists. See **Examples**.
#'
#' @seealso [`CmdStanDiagnose`]
#' @inherit CmdStanDiagnose examples
#'
gradients <- function() {
  private$gradients_
}

lp_diagnose <- function() {
  as.numeric(private$lp_)
}

CmdStanDiagnose$set("public", name = "gradients", value = gradients)
CmdStanDiagnose$set("public", name = "lp", value = lp_diagnose)
CmdStanDiagnose$set("public", name = "init", value = init)
CmdStanDiagnose$set("public", name = "save_output_files", value = save_output_files)
CmdStanDiagnose$set("public", name = "output_files", value = output_files)
CmdStanDiagnose$set("public", name = "save_data_file", value = save_data_file)
CmdStanDiagnose$set("public", name = "data_file", value = data_file)
