#' Create a new CmdStanModel object
#'
#' \if{html}{\figure{logo.png}{options: width="25px" alt="https://mc-stan.org/about/logo/"}}
#' The `cmdstan_model()` function creates a new [`CmdStanModel`] object from a
#' file containing a Stan program.
#'
#' @export
#' @param stan_file The path to a `.stan` file containing a Stan program.
#' @param compile Do compilation? The default is `TRUE`. If `FALSE`
#'   compilation can be done later via the [`$compile()`][model-method-compile]
#'   method.
#' @param ... Optionally, additional arguments to pass to the
#'   [`$compile()`][model-method-compile] method if `compile=TRUE`.
#'
#' @return A [`CmdStanModel`] object.
#'
#' @seealso [install_cmdstan()], [cmdstan_path()]
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' library(cmdstanr)
#' library(posterior)
#' library(bayesplot)
#' color_scheme_set("brightblue")
#'
#' # Set path to cmdstan
#' # (Note: if you installed CmdStan via install_cmdstan() with default settings
#' # then setting the path is unnecessary but the default below should still work.
#' # Otherwise use the `path` argument to specify the location of your
#' # CmdStan installation.)
#' set_cmdstan_path(path = NULL)
#'
#' # Create a CmdStanModel object from a Stan program,
#' # here using the example model that comes with CmdStan
#' stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(stan_program)
#' mod$print()
#'
#' # data as a named list (like RStan)
#' stan_data <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
#'
#' # run MCMC using the 'sample' method
#' fit_mcmc <- mod$sample(
#'   data = stan_data,
#'   seed = 123,
#'   chains = 2,
#'   parallel_chains = 2
#' )
#'
#' # Use 'posterior' package for summaries
#' fit_mcmc$summary()
#'
#' # Get posterior draws
#' draws <- fit_mcmc$draws()
#' print(draws)
#'
#' # Convert to data frame using posterior::as_draws_df
#' as_draws_df(draws)
#'
#' # Plot posterior using bayesplot (ggplot2)
#' mcmc_hist(fit_mcmc$draws("theta"))
#'
#' # Call CmdStan's diagnose and stansummary utilities
#' fit_mcmc$cmdstan_diagnose()
#' fit_mcmc$cmdstan_summary()
#'
#' # For models fit using MCMC, if you like working with RStan's stanfit objects
#' # then you can create one with rstan::read_stan_csv()
#'
#' # stanfit <- rstan::read_stan_csv(fit_mcmc$output_files())
#'
#'
#' # Run 'optimize' method to get a point estimate (default is Stan's LBFGS algorithm)
#' # and also demonstrate specifying data as a path to a file instead of a list
#' my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.json")
#' fit_optim <- mod$optimize(data = my_data_file, seed = 123)
#'
#' fit_optim$summary()
#'
#' # Run 'variational' to approximate the posterior (default is meanfield ADVI)
#' fit_vb <- mod$variational(data = stan_data, seed = 123)
#'
#' fit_vb$summary()
#'
#' # Plot approximate posterior using bayesplot
#' mcmc_hist(fit_vb$draws("theta"))
#' }
#'
cmdstan_model <- function(stan_file, compile = TRUE, ...) {
  CmdStanModel$new(stan_file = stan_file, compile = compile, ...)
}


# CmdStanModel -----------------------------------------------------------------

#' CmdStanModel objects
#'
#' @name CmdStanModel
#' @description A `CmdStanModel` object is an [R6][R6::R6Class] object created
#'   by the [cmdstan_model()] function. The object stores the path to a Stan
#'   program and compiled executable (once created), and provides methods for
#'   fitting the model using Stan's algorithms. See the **Details** section for
#'   available methods.
#'
#' @details
#' `CmdStanModel` objects have the following associated methods:
#'
#' \tabular{ll}{
#'  **Method** \tab **Description** \cr
#'  `$code()` \tab Return Stan program as a string. \cr
#'  `$print()` \tab Print readable version of Stan program. \cr
#'  `$stan_file()` \tab Return the file path to the Stan program. \cr
#'  `$exe_file()` \tab Return the file path to the compiled executable. \cr
#'  [`$compile()`][model-method-compile] \tab Compile Stan program. \cr
#'  [`$sample()`][model-method-sample]
#'    \tab Run CmdStan's `"sample"` method, return [`CmdStanMCMC`] object. \cr
#'  [`$optimize()`][model-method-optimize]
#'    \tab Run CmdStan's `"optimize"` method, return [`CmdStanMLE`] object. \cr
#'  [`$variational()`][model-method-variational]
#'    \tab Run CmdStan's `"variational"` method, return [`CmdStanVB`] object. \cr
#' }
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
NULL

CmdStanModel <- R6::R6Class(
  classname = "CmdStanModel",
  private = list(
    stan_file_ = character(),
    exe_file_ = character(),
    cpp_options_ = list(),
    stanc_options_ = list(),
    include_paths_ = NULL,
    precompile_cpp_options_ = NULL,
    precompile_stanc_options_ = NULL,
    precompile_include_paths_ = NULL
  ),
  public = list(
    initialize = function(stan_file, compile, ...) {
      checkmate::assert_file_exists(stan_file, access = "r", extension = "stan")
      checkmate::assert_flag(compile)
      private$stan_file_ <- absolute_path(stan_file)
      args <- list(...)
      cpp_options_exists <- "cpp_options" %in% names(args)
      if (cpp_options_exists) {
        private$precompile_cpp_options_ = args$cpp_options
      }
      stanc_options_exists <- "stanc_options" %in% names(args)
      if (stanc_options_exists) {
        private$precompile_stanc_options_ = args$stanc_options
      }
      include_paths_exists <- "include_paths" %in% names(args)
      if (include_paths_exists) {
        private$precompile_include_paths_ = args$include_paths
      }
      if (compile) {
        self$compile(...)
      }
      invisible(self)
    },
    stan_file = function() private$stan_file_,
    cpp_options = function() private$cpp_options_,
    exe_file = function(path = NULL) {
      if (!is.null(path)) {
        private$exe_file_ <- path
      }
      private$exe_file_
    },
    code = function() {
      # Get Stan code as a string
      readLines(self$stan_file())
    },
    print = function() {
      # Print readable version of Stan code
      cat(self$code(), sep = "\n")
      invisible(self)
    }
  )
)

# CmdStanModel methods -----------------------------------

#' Compile a Stan program or get the Stan code
#'
#' @name model-method-compile
#' @aliases compile
#' @family CmdStanModel methods
#'
#' @description The `$compile()` method of a [`CmdStanModel`] object calls
#'   CmdStan to translate a Stan program to C++ and create a compiled
#'   executable. The resulting files are placed in the same directory as the
#'   Stan program associated with the `CmdStanModel` object. After compilation
#'   the path to the executable can be accesed via the `$exe_file()` method.
#'
#' @section Usage:
#'   ```
#'   $compile(
#'     quiet = TRUE,
#'     include_paths = NULL,
#'     cpp_options = list(),
#'     stanc_options = list(),
#'     force_recompile = FALSE
#'   )
#'   $exe_file()
#'   ```
#'
#' @section Arguments:
#'   Leaving all arguments at their defaults should be fine for most users, but
#'   optional arguments are provided to enable features in CmdStan (and the Stan
#'   Math library). See the CmdStan manual for more details.
#'   * `quiet`: (logical) Should the verbose output from CmdStan during
#'   compilation be suppressed? The default is `TRUE`, but if you encounter an
#'   error we recommend trying again with `quiet=FALSE` to see more of the
#'   output.
#'   * `include_paths`: (character vector) Paths to directories where Stan should
#'   look for files specified in `#include` directives in the Stan program.
#'   * `cpp_options`: (list) Any makefile options to be
#'   used when compiling the model (STAN_THREADS, STAN_MPI, STAN_OPENCL, ...).
#'   Anything you would otherwise write in the make/local file.
#'   * `stanc_options`: (list) Any Stan-to-C++ transpiler options to be
#'     used when compiling the model.
#'   * `force_recompile`: (logical) Should the model be recompiled
#'     even if was not modified since last compiled. The default is `FALSE`.
#'
#' @section Value: This method is called for its side effect of creating the
#'   executable and adding its path to the [`CmdStanModel`] object, but it also
#'   returns the [`CmdStanModel`] object invisibly.
#'
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(stan_program, compile = FALSE)
#' mod$compile()
#' mod$exe_file()
#'
#' stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(stan_program, compile = FALSE)
#' mod$compile(cpp_options = list(stan_threads = TRUE))
#' mod$exe_file()
#' }
#'
NULL

compile_method <- function(quiet = TRUE,
                           include_paths = NULL,
                           cpp_options = list(),
                           stanc_options = list(),
                           force_recompile = FALSE,
                           #deprecated
                           threads = FALSE) {
  if (length(cpp_options) == 0 && !is.null(private$precompile_cpp_options_)) {
    cpp_options = private$precompile_cpp_options_
  }
  if (length(stanc_options) == 0 && !is.null(private$precompile_stanc_options_)) {
    stanc_options = private$precompile_stanc_options_
  }
  if (is.null(include_paths) && !is.null(private$precompile_include_paths_)) {
    include_paths = private$precompile_include_paths_
  }
  # temporary deprecation warnings
  if (isTRUE(threads)) {
    warning("'threads' is deprecated. Please use 'cpp_options = list(stan_threads = TRUE)' instead.")
    cpp_options[["stan_threads"]] <- TRUE
  }

  exe_suffix <- NULL
  if (!is.null(cpp_options$stan_threads)) {
    exe_suffix <- c(exe_suffix, "threads")
  }
  if (!is.null(cpp_options$stan_mpi)) {
    exe_suffix <- c(exe_suffix, "mpi")
  }
  if (!is.null(cpp_options$stan_opencl)) {
    exe_suffix <- c(exe_suffix, "opencl")
  }
  exe_suffix <- paste0(exe_suffix, collapse = "_")
  if (nzchar(exe_suffix)) {
    exe_suffix <- paste0("_", exe_suffix)
  }
  if (all(nzchar(self$exe_file()))) {
    exe <- cmdstan_ext(paste0(strip_ext(self$stan_file()), exe_suffix))
  }

  model_name <- sub(" ", "_", paste0(strip_ext(basename(self$stan_file())), "_model"))

  # compile if the user forced compilation,
  # the executable does not exist or the stan model was changed since last compilation
  if (!file.exists(exe)) {
    force_recompile <- TRUE
  } else if (file.exists(self$stan_file())
             && file.mtime(exe) < file.mtime(self$stan_file())) {
    force_recompile <- TRUE
  }

  if (!force_recompile) {
    message("Model executable is up to date!")
    private$cpp_options_ <- cpp_options
    private$precompile_cpp_options_ <- NULL
    private$precompile_stanc_options_ <- NULL
    private$precompile_include_paths_ <- NULL
    self$exe_file(exe)
    return(invisible(self))
  } else {
    message("Compiling Stan program...")
  }

  temp_stan_file <- tempfile(pattern = "model-", fileext = ".stan")
  file.copy(self$stan_file(), temp_stan_file, overwrite = TRUE)
  tmp_exe <- cmdstan_ext(strip_ext(temp_stan_file)) # adds .exe on Windows

  # add path to the TBB library to the PATH variable to avoid copying the dll file
  if (cmdstan_version() >= "2.21" && os_is_windows()) {
    path_to_TBB <- file.path(cmdstan_path(), "stan", "lib", "stan_math", "lib", "tbb")
    current_path <- Sys.getenv("PATH")
    if (regexpr("path_to_TBB", current_path, perl = TRUE) <= 0) {
      Sys.setenv(PATH = paste0(path_to_TBB, ";", Sys.getenv("PATH")))
    }
  }

  stancflags_val <- ""
  if (!is.null(include_paths)) {
    checkmate::assert_directory_exists(include_paths, access = "r")
    include_paths <- absolute_path(include_paths)
    include_paths <- paste0(include_paths, collapse = ",")
    stancflags_val <- paste0(stancflags_val, " --include_paths=", include_paths, " ")
  }
  if (!is.null(cpp_options$stan_opencl)) {
    stanc_options[["use-opencl"]] <- TRUE
  }
  if (is.null(stanc_options[["name"]])) {
    stanc_options[["name"]] <- model_name
  }
  stanc_built_options = c()
  for (i in seq_len(length(stanc_options))) {
    option_name <- names(stanc_options)[i]
    if (isTRUE(as.logical(stanc_options[[i]]))) {
      stanc_built_options = c(stanc_built_options, paste0("--", option_name))
    } else {
      stanc_built_options = c(stanc_built_options, paste0("--", option_name, "=", "'", stanc_options[[i]], "'"))
    }
  }
  stancflags_val <- paste0("STANCFLAGS += ", stancflags_val, paste0(stanc_built_options, collapse = " "))
  prepare_precompiled(cpp_options, quiet)
  run_log <- processx::run(
    command = make_cmd(),
    args = c(tmp_exe,
             cpp_options_to_compile_flags(cpp_options),
             stancflags_val),
    wd = cmdstan_path(),
    echo_cmd = !quiet,
    echo = !quiet,
    spinner = quiet && interactive(),
    stderr_line_callback = function(x,p) {
        if (!startsWith(x, paste0(make_cmd(), ": *** No rule to make target"))) message(x)
    },
    error_on_status = FALSE
  )
  if (run_log$status != 0) {
    stop("An error occured during compilation! See the message above for more information.",
         call. = FALSE)
  }

  file.copy(tmp_exe, exe, overwrite = TRUE)
  private$cpp_options_ <- cpp_options
  private$precompile_cpp_options_ <- NULL
  private$precompile_stanc_options_ <- NULL
  private$precompile_include_paths_ <- NULL
  private$exe_file_ <- exe
  invisible(self)
}
CmdStanModel$set("public", name = "compile", value = compile_method)


#' Run Stan's MCMC algorithms
#'
#' @name model-method-sample
#' @aliases sample
#' @family CmdStanModel methods
#'
#' @description The `$sample()` method of a [`CmdStanModel`] object runs the
#'   default MCMC algorithm in CmdStan (`algorithm=hmc engine=nuts`), to produce
#'   a set of draws from the posterior distribution of a model conditioned on
#'   some data.
#'
#' @section Usage:
#'   ```
#'   $sample(
#'     data = NULL,
#'     seed = NULL,
#'     refresh = NULL,
#'     init = NULL,
#'     save_latent_dynamics = FALSE,
#'     output_dir = NULL,
#'     chains = 4,
#'     parallel_chains = getOption("mc.cores", 1),
#'     threads_per_chain = NULL,
#'     iter_warmup = NULL,
#'     iter_sampling = NULL,
#'     save_warmup = FALSE,
#'     thin = NULL,
#'     max_treedepth = NULL,
#'     adapt_engaged = TRUE,
#'     adapt_delta = NULL,
#'     step_size = NULL,
#'     metric = NULL,
#'     metric_file = NULL,
#'     inv_metric = NULL,
#'     init_buffer = NULL,
#'     term_buffer = NULL,
#'     window = NULL,
#'     fixed_param = FALSE,
#'     validate_csv = TRUE
#'   )
#'   ```
#'
#' @template model-common-args
#' @section Arguments unique to the `sample` method: In addition to the
#'   arguments above, the `$sample()` method also has its own set of arguments.
#'
#'   The following three arguments are offered by CmdStanR but do not correspond
#'   to arguments in CmdStan:
#'
#'   * `chains`: (positive integer) The number of Markov chains to run. The
#'   default is 4.
#'
#'   * `parallel_chains`: (positive integer) The _maximum_ number of MCMC chains
#'   to run in parallel. If `parallel_chains` is not specified then the default
#'   is to look for the option `"mc.cores"`, which can be set for an entire \R
#'   session by `options(mc.cores=value)`. If the `"mc.cores"` option has not
#'   been set then the default is `1`.
#'
#'   * `threads_per_chain`: (positive integer) If the model was
#'   [compiled][model-method-compile] with threading support, the number of
#'   threads to use in parallelized sections _within_ an MCMC chain (e.g., when
#'   using the Stan functions `reduce_sum()` or `map_rect()`). This is in
#'   contrast with `parallel_chains`, which specifies the number of chains to
#'   run in parallel. The actual number of CPU cores used use is
#'   `parallel_chains*threads_per_chain`. For an example of using threading see
#'   the Stan case study [Reduce Sum: A Minimal
#'   Example](https://mc-stan.org/users/documentation/case-studies/reduce_sum_tutorial.html).
#'
#'
#'   The rest of the arguments correspond to arguments offered by CmdStan,
#'   although some names are slightly different. They are described briefly here
#'   and in greater detail in the CmdStan manual. Arguments left at `NULL`
#'   default to the default used by the installed version of CmdStan.
#'
#'   * `iter_sampling`: (positive integer) The number of post-warmup iterations to
#'   run per chain.
#'   * `iter_warmup`: (positive integer) The number of warmup iterations to run
#'   per chain.
#'   * `save_warmup`: (logical) Should warmup iterations be saved? The default
#'   is `FALSE`. If `save_warmup=TRUE` then you can use
#'   [$draws(inc_warmup=TRUE)][fit-method-draws] to include warmup when
#'   accessing the draws.
#'   * `thin`: (positive integer) The period between saved samples. This should
#'   typically be left at its default (no thinning) unless memory is a problem.
#'   * `max_treedepth`: (positive integer) The maximum allowed tree depth for the
#'   NUTS engine. See the _Tree Depth_ section of the CmdStan manual for more
#'   details.
#'   * `adapt_engaged`: (logical) Do warmup adaptation? The default is `TRUE`.
#'   If a precomputed inverse metric is specified via the `inv_metric` argument
#'   (or `metric_file`) then, if `adapt_engaged=TRUE`, Stan will use the
#'   provided inverse metric just as an initial guess during adaptation. To turn
#'   off adaptation when using a precomputed inverse metric set
#'   `adapt_engaged=FALSE`.
#'   * `adapt_delta`: (real in `(0,1)`) The adaptation target acceptance
#'   statistic.
#'   * `step_size`: (positive real) The _initial_ step size for the discrete
#'   approximation to continuous Hamiltonian dynamics. This is further tuned
#'   during warmup.
#'   * `metric`: (character) One of `"diag_e"`, `"dense_e"`, or `"unit_e"`,
#'   specifying the geometry of the base manifold. See the _Euclidean Metric_
#'   section of the CmdStan documentation for more details. To specify a
#'   precomputed (inverse) metric, see the `inv_metric` argument below.
#'   * `metric_file`: (character) A character vector containing paths to JSON or
#'   Rdump files (one per chain) compatible with CmdStan that contain
#'   precomputed inverse metrics. The `metric_file` argument is inherited from
#'   CmdStan but is confusing in that the entry in JSON or Rdump file(s) must be
#'   named `inv_metric`, referring to the _inverse_ metric. We recommend instead
#'   using CmdStanR's `inv_metric` argument (see below) to specify an inverse
#'   metric directly using a vector or matrix from your \R session.
#'   * `inv_metric`: (vector, matrix) A vector (if `metric='diag_e'`) or a
#'   matrix (if `metric='dense_e'`) for initializing the inverse metric, which
#'   can be used as an alternative to the `metric_file` argument. A vector is
#'   interpreted as a diagonal metric. The inverse metric is usually set to an
#'   estimate of the posterior covariance. See the `adapt_engaged` argument
#'   above for details on (and control over) how specifying a precomputed
#'   inverse metric interacts with adaptation.
#'   * `init_buffer`: (nonnegative integer) Width of initial fast timestep
#'   adaptation interval during warmup.
#'   * `term_buffer`: (nonnegative integer) Width of final fast timestep
#'   adaptation interval during warmup.
#'   * `window`: (nonnegative integer) Initial width of slow timestep/metric
#'   adaptation interval.
#'   * `fixed_param`: (logical) When `TRUE`, call CmdStan with argument
#'   `"algorithm=fixed_param"`. The default is `FALSE`.
#'   * `validate_csv`: (logical) When `TRUE` (the default), validate the
#'   sampling results in the csv files. Disable if you wish to manually read in
#'   the sampling results and validate them.
#'
#' @section Value: The `$sample()` method returns a [`CmdStanMCMC`] object.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
NULL

sample_method <- function(data = NULL,
                          seed = NULL,
                          refresh = NULL,
                          init = NULL,
                          save_latent_dynamics = FALSE,
                          output_dir = NULL,
                          chains = 4,
                          parallel_chains = getOption("mc.cores", 1),
                          threads_per_chain = NULL,
                          iter_warmup = NULL,
                          iter_sampling = NULL,
                          save_warmup = FALSE,
                          thin = NULL,
                          max_treedepth = NULL,
                          adapt_engaged = TRUE,
                          adapt_delta = NULL,
                          step_size = NULL,
                          metric = NULL,
                          metric_file = NULL,
                          inv_metric = NULL,
                          init_buffer = NULL,
                          term_buffer = NULL,
                          window = NULL,
                          fixed_param = FALSE,
                          validate_csv = TRUE,
                          # deprecated
                          cores = NULL,
                          num_cores = NULL,
                          num_chains = NULL,
                          num_warmup = NULL,
                          num_samples = NULL,
                          save_extra_diagnostics = NULL,
                          max_depth = NULL,
                          stepsize = NULL) {

  if (fixed_param) {
    chains <- 1
    parallel_chains <- 1
    save_warmup <- FALSE
  }
  # temporary deprecation warnings
  if (!is.null(cores)) {
    warning("'cores' is deprecated. Please use 'parallel_chains' instead.")
    parallel_chains <- cores
  }
  if (!is.null(num_cores)) {
    warning("'num_cores' is deprecated. Please use 'parallel_chains' instead.")
    cores <- num_cores
  }
  if (!is.null(num_chains)) {
    warning("'num_chains' is deprecated. Please use 'chains' instead.")
    chains <- num_chains
  }
  if (!is.null(num_warmup)) {
    warning("'num_warmup' is deprecated. Please use 'iter_warmup' instead.")
    iter_warmup <- num_warmup
  }
  if (!is.null(num_samples)) {
    warning("'num_samples' is deprecated. Please use 'iter_sampling' instead.")
    iter_sampling <- num_samples
  }
  if (!is.null(max_depth)) {
    warning("'max_depth' is deprecated. Please use 'max_treedepth' instead.")
    max_treedepth <- max_depth
  }
  if (!is.null(stepsize)) {
    warning("'stepsize' is deprecated. Please use 'step_size' instead.")
    step_size <- stepsize
  }
  if (!is.null(save_extra_diagnostics)) {
    warning("'save_extra_diagnostics' is deprecated. Please use 'save_latent_dynamics' instead.")
    save_latent_dynamics <- save_extra_diagnostics
  }
  checkmate::assert_integerish(chains, lower = 1, len = 1)
  checkmate::assert_integerish(parallel_chains, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(threads_per_chain, lower = 1, len = 1, null.ok = TRUE)
  # check if model was not compiled with threading
  if (is.null(self$cpp_options()[["stan_threads"]])) {
    if (!is.null(threads_per_chain)) {
      warning("'threads_per_chain' is set but the model was not compiled with 'cpp_options = list(stan_threads = TRUE)' so 'threads_per_chain' will have no effect!")
      threads_per_chain <- NULL
    }
  } else {
    if (is.null(threads_per_chain)) {
      stop("The model was compiled with 'cpp_options = list(stan_threads = TRUE)' but 'threads_per_chain' was not set!")
    }
  }

  sample_args <- SampleArgs$new(
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    save_warmup = save_warmup,
    thin = thin,
    max_treedepth = max_treedepth,
    adapt_engaged = adapt_engaged,
    adapt_delta = adapt_delta,
    step_size = step_size,
    metric = metric,
    metric_file = metric_file,
    inv_metric = inv_metric,
    init_buffer = init_buffer,
    term_buffer = term_buffer,
    window = window,
    fixed_param = fixed_param
  )
  cmdstan_args <- CmdStanArgs$new(
    method_args = sample_args,
    model_name = strip_ext(basename(self$exe_file())),
    exe_file = self$exe_file(),
    proc_ids = seq_len(chains),
    data_file = process_data(data),
    save_latent_dynamics = save_latent_dynamics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    validate_csv = validate_csv
  )
  cmdstan_procs <- CmdStanMCMCProcs$new(num_procs = chains, parallel_procs = parallel_chains, threads_per_proc = threads_per_chain)
  runset <- CmdStanRun$new(args = cmdstan_args, procs = cmdstan_procs)
  runset$run_cmdstan()
  CmdStanMCMC$new(runset)
}
CmdStanModel$set("public", name = "sample", value = sample_method)


#' Run Stan's optimization algorithms
#'
#' @name model-method-optimize
#' @aliases optimize
#' @family CmdStanModel methods
#'
#' @description The `$optimize()` method of a [`CmdStanModel`] object runs
#'   Stan's optimizer to obtain a posterior mode (penalized maximum likelihood)
#'   estimate.
#'
#' @details CmdStan can find the posterior mode (assuming there is one). If the
#'   posterior is not convex, there is no guarantee Stan will be able to find
#'   the global mode as opposed to a local optimum of log probability. For
#'   optimization, the mode is calculated without the Jacobian adjustment for
#'   constrained variables, which shifts the mode due to the change of
#'   variables. Thus modes correspond to modes of the model as written.
#'
#'   -- [*CmdStan Interface User's Guide*](https://github.com/stan-dev/cmdstan/releases/latest)
#'
#' @section Usage:
#'   ```
#'   $optimize(
#'     data = NULL,
#'     seed = NULL,
#'     refresh = NULL,
#'     init = NULL,
#'     save_latent_dynamics = FALSE,
#'     output_dir = NULL,
#'     algorithm = NULL,
#'     init_alpha = NULL,
#'     iter = NULL
#'   )
#'   ```
#'
#' @template model-common-args
#' @section Arguments unique to the `optimize` method: In addition to the
#'   arguments above, the `$optimize()` method also has its own set of
#'   arguments. These arguments are described briefly here and in greater detail
#'   in the CmdStan manual. Arguments left at `NULL` default to the default used
#'   by the installed version of CmdStan.
#'
#'   * `algorithm`: (string) The optimization algorithm. One of `"lbfgs"`,
#'   `"bfgs"`, or `"newton"`.
#'   * `iter`: (positive integer) The number of iterations.
#'   * `init_alpha`: (nonnegative real) The line search step size for first
#'   iteration. Not applicable if `algorithm="newton"`.
#'
#' @section Value: The `$optimize()` method returns a [`CmdStanMLE`] object.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
NULL

optimize_method <- function(data = NULL,
                            seed = NULL,
                            refresh = NULL,
                            init = NULL,
                            save_latent_dynamics = FALSE,
                            output_dir = NULL,
                            algorithm = NULL,
                            init_alpha = NULL,
                            iter = NULL) {
  optimize_args <- OptimizeArgs$new(
    algorithm = algorithm,
    init_alpha = init_alpha,
    iter = iter
  )
  cmdstan_args <- CmdStanArgs$new(
    method_args = optimize_args,
    model_name = strip_ext(basename(self$exe_file())),
    exe_file = self$exe_file(),
    proc_ids = 1,
    data_file = process_data(data),
    save_latent_dynamics = save_latent_dynamics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir
  )

  cmdstan_procs <- CmdStanProcs$new(num_procs = 1)
  runset <- CmdStanRun$new(args = cmdstan_args, procs = cmdstan_procs)
  runset$run_cmdstan()
  CmdStanMLE$new(runset)
}
CmdStanModel$set("public", name = "optimize", value = optimize_method)


#' Run Stan's variational approximation algorithms
#'
#' @name model-method-variational
#' @aliases variational
#' @family CmdStanModel methods
#'
#' @description The `$variational()` method of a [`CmdStanModel`] object runs
#'   Stan's variational Bayes (ADVI) algorithms.
#'
#' @details CmdStan can fit a variational approximation to the posterior. The
#'   approximation is a Gaussian in the unconstrained variable space. Stan
#'   implements two variational algorithms. The `algorithm="meanfield"` option
#'   uses a fully factorized Gaussian for the approximation. The
#'   `algorithm="fullrank"` option uses a Gaussian with a full-rank covariance
#'   matrix for the approximation.
#'
#'   -- [*CmdStan Interface User's Guide*](https://github.com/stan-dev/cmdstan/releases/latest)
#'
#' @section Usage:
#'   ```
#'   $variational(
#'     data = NULL,
#'     seed = NULL,
#'     refresh = NULL,
#'     init = NULL,
#'     save_latent_dynamics = FALSE,
#'     output_dir = NULL,
#'     algorithm = NULL,
#'     iter = NULL,
#'     grad_samples = NULL,
#'     elbo_samples = NULL,
#'     eta = NULL,
#'     adapt_engaged = NULL,
#'     adapt_iter = NULL,
#'     tol_rel_obj = NULL,
#'     eval_elbo = NULL,
#'     output_samples = NULL
#'   )
#'   ```
#'
#' @template model-common-args
#' @section Arguments unique to the `variational` method: In addition to the
#'   arguments above, the `$variational()` method also has its own set of
#'   arguments. These arguments are described briefly here and in greater detail
#'   in the CmdStan manual. Arguments left at `NULL` default to the default used
#'   by the installed version of CmdStan.
#'
#'   * `algorithm`: (string) The algorithm. Either `"meanfield"` or `"fullrank"`.
#'   * `iter`: (positive integer) The _maximum_ number of iterations.
#'   * `grad_samples`: (positive integer) The number of samples for Monte Carlo
#'   estimate of gradients.
#'   * `elbo_samples`: (positive integer) The number of samples for Monte Carlo
#'   estimate of ELBO (objective function).
#'   * `eta`: (positive real) The step size weighting parameter for adaptive
#'   step size sequence.
#'   * `adapt_engaged`: (logical) Do warmup adaptation?
#'   * `adapt_iter`: (positive integer) The _maximum_ number of adaptation
#'   iterations.
#'   * `tol_rel_obj`: (positive real) Convergence tolerance on the relative norm
#'   of the objective.
#'   * `eval_elbo`: (positive integer) Evaluate ELBO every Nth iteration.
#'   * `output_samples:` (positive integer) Number of posterior samples to draw
#'   and save.
#'
#' @section Value: The `$variational()` method returns a [`CmdStanVB`] object.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
NULL

variational_method <- function(data = NULL,
                               seed = NULL,
                               refresh = NULL,
                               init = NULL,
                               save_latent_dynamics = FALSE,
                               output_dir = NULL,
                               algorithm = NULL,
                               iter = NULL,
                               grad_samples = NULL,
                               elbo_samples = NULL,
                               eta = NULL,
                               adapt_engaged = NULL,
                               adapt_iter = NULL,
                               tol_rel_obj = NULL,
                               eval_elbo = NULL,
                               output_samples = NULL) {
  variational_args <- VariationalArgs$new(
    algorithm = algorithm,
    iter = iter,
    grad_samples = grad_samples,
    elbo_samples = elbo_samples,
    eta = eta,
    adapt_engaged = adapt_engaged,
    adapt_iter = adapt_iter,
    tol_rel_obj = tol_rel_obj,
    eval_elbo = eval_elbo,
    output_samples = output_samples
  )
  cmdstan_args <- CmdStanArgs$new(
    method_args = variational_args,
    model_name = strip_ext(basename(self$exe_file())),
    exe_file = self$exe_file(),
    proc_ids = 1,
    data_file = process_data(data),
    save_latent_dynamics = save_latent_dynamics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir
  )

  cmdstan_procs <- CmdStanProcs$new(num_procs = 1)
  runset <- CmdStanRun$new(args = cmdstan_args, procs = cmdstan_procs)
  runset$run_cmdstan()
  CmdStanVB$new(runset)
}
CmdStanModel$set("public", name = "variational", value = variational_method)
