#' Create a new CmdStanModel object
#'
#' \if{html}{\figure{logo.png}{options: width="25px" alt="https://mc-stan.org/about/logo/"}}
#' The `cmdstan_model()` function creates a new [`CmdStanModel`] object from a
#' file containing a Stan program.
#'
#' @export
#' @param stan_file The path to a `.stan` file containing a Stan program.
#' @param model_name The name of the Stan model.
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
#' # Set path to cmdstan
#' # (Note: if you installed CmdStan via install_cmdstan() with default settings
#' # then setting the path is unnecessary but the default below should still work.
#' # Otherwise use the `path` argument to specify the location of your
#' # CmdStan installation.)
#'
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
#'   num_chains = 2,
#'   num_cores = 2
#' )
#'
#' # Use 'posterior' package for summaries
#' fit_mcmc$summary()
#'
#' # Call CmdStan's diagnose and stansummary utilities
#' fit_mcmc$cmdstan_diagnose()
#' fit_mcmc$cmdstan_summary()
#'
#' # For models fit using MCMC, if you like working with RStan's stanfit objects
#' # then you can create one with rstan::read_stan_csv()
#' if (require(rstan, quietly = TRUE)) {
#'   stanfit <- rstan::read_stan_csv(fit_mcmc$output_files())
#'   print(stanfit)
#' }
#'
#' # Run 'optimize' method to get a point estimate (default is Stan's LBFGS algorithm)
#' # and also demonstrate specifying data as a path to a file instead of a list
#' my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.json")
#' fit_optim <- mod$optimize(data = my_data_file, seed = 123)
#' fit_optim$summary()
#'
#' # Run 'variational' to approximate the posterior (default is meanfield ADVI)
#' fit_vb <- mod$variational(data = stan_data, seed = 123)
#' fit_vb$summary()
#' }
#'
cmdstan_model <- function(stan_file, model_name = NULL, compile = TRUE, ...) {
  CmdStanModel$new(stan_file = stan_file, model_name = model_name, compile = compile, ...)
}


# CmdStanModel -----------------------------------------------------------------

#' CmdStanModel objects
#'
#' @name CmdStanModel
#' @description A `CmdStanModel` object is an [R6][R6::R6] object created by the
#'   [cmdstan_model()] function. The object stores the path to a Stan program
#'   and compiled executable (once created), and provides methods for fitting
#'   the model using Stan's algorithms. See the **Details** section for
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
#'  `$model_name()` \tab Return the model name. \cr
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
    model_name_ = character()
  ),
  public = list(
    initialize = function(stan_file, model_name, compile, ...) {
      checkmate::assert_file_exists(stan_file, access = "r", extension = "stan")
      checkmate::assert_flag(compile)
      private$stan_file_ <- absolute_path(stan_file)
      private$model_name_ <- model_name
      if (compile) {
        self$compile(...)
      }
      invisible(self)
    },
    stan_file = function() private$stan_file_,
    exe_file = function() private$exe_file_,
    model_name = function(name = NULL) {
      if (!is.null(name)) {
        private$model_name_ = name
      }
      private$model_name_
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
#'     threads = FALSE,
#'     opencl = FALSE,
#'     opencl_platform_id = 0,
#'     opencl_device_id = 0,
#'     compiler_flags = NULL,
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
#'     compilation be suppressed? The default is `TRUE`, but if you encounter an
#'     error we recommend trying again with `quiet=FALSE` to see more of the
#'     output.
#'   * `include_paths`: (character vector) Paths to directories where Stan should
#'     look for files specified in `#include` directives in the Stan program.
#'   * `stanc_options`: (character vector) Any Stan-to-C++ transpiler options to be
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
#' }
#'
NULL

compile_method <- function(quiet = TRUE,
                           include_paths = NULL,
                           stanc_options = list(),
                           force_recompile = FALSE) {
  if (is.null(self$model_name())) {
    exe <- cmdstan_ext(strip_ext(self$stan_file()))
    self$model_name(sub(" ", "_", 
                        paste0(strip_ext(basename(self$stan_file())), "_model")))
  } else {
    exe <- cmdstan_ext(file.path(dirname(self$stan_file()), self$model_name()))    
  }

  # compile if the user forced compilation,
  # the executable does not exist or the stan model was changed since last compilation
  if (!file.exists(exe)) {
    force_recompile <- TRUE
  } else if (file.mtime(exe) < file.mtime(self$stan_file())) {
    force_recompile <- TRUE
  }
  
  if (!force_recompile) {
    message("Model executable is up to date!")
    private$exe_file_ <- exe
    return(invisible(self))
  } else {
    message("Compiling Stan program...")
  }

  temp_stan_file <- tempfile(pattern = "model-", fileext = ".stan")
  file.copy(self$stan_file(), temp_stan_file, overwrite = TRUE)
  tmp_exe <- cmdstan_ext(strip_ext(temp_stan_file)) # adds .exe on Windows

  # add path to the build tbb library to the PATH variable to avoid copying the dll file
  if (cmdstan_version() >= "2.21" && os_is_windows()) {
    path_to_TBB <- file.path(cmdstan_path(), "stan", "lib", "stan_math", "lib", "tbb")
    Sys.setenv(PATH = paste0(path_to_TBB, ";", Sys.getenv("PATH")))
  }

  stanc_options[["name"]] <- self$model_name()
  
  stancflags_val <- ""
  if (!is.null(include_paths)) {
    checkmate::assert_directory_exists(include_paths, access = "r")
    include_paths <- absolute_path(include_paths)
    include_paths <- paste0(include_paths, collapse = ",")
    stancflags_val <- paste0(stancflags_val, " --include_paths=", include_paths, " ")
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

  run_log <- processx::run(
    command = make_cmd(),
    args = c(tmp_exe,
             cpp_options_to_compile_flags(.cmdstanr$CPP_OPTIONS),
             stancflags_val),
    wd = cmdstan_path(),
    echo_cmd = !quiet,
    echo = !quiet,
    spinner = quiet,
    stderr_line_callback = function(x,p) { if (!quiet) message(x) },
    error_on_status = TRUE
  )

  file.copy(tmp_exe, exe, overwrite = TRUE)
  private$exe_file_ <- exe
  invisible(self)
}
CmdStanModel$set("public", name = "compile", value = compile_method)


#' Run Stan's MCMC algorithms
#'
#' @name model-method-sample
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
#'     save_extra_diagnostics = FALSE,
#'     output_dir = NULL,
#'     num_chains = 4,
#'     num_cores = getOption("mc.cores", 1),
#'     num_warmup = NULL,
#'     num_samples = NULL,
#'     save_warmup = FALSE,
#'     thin = NULL,
#'     max_depth = NULL,
#'     adapt_engaged = TRUE,
#'     adapt_delta = NULL,
#'     stepsize = NULL,
#'     metric = NULL,
#'     metric_file = NULL,
#'     inv_metric = NULL,
#'     init_buffer = NULL,
#'     term_buffer = NULL,
#'     window = NULL
#'   )
#'   ```
#'
#' @template model-common-args
#' @section Arguments unique to the `sample` method: In addition to the
#'   arguments above, the `$sample()` method also has its own set of arguments.
#'
#'   The following two arguments are offered by CmdStanR but do not correspond
#'   to arguments in CmdStan because all CmdStan arguments pertain to the
#'   execution of a single run only.
#'
#'   * `num_chains`: (positive integer) The number of Markov chains to run. The
#'   default is 4.
#'
#'   * `num_cores`: (positive integer) The maximum number of cores to use for
#'   running parallel chains. If `num_cores` is not specified then the default is
#'   to look for the option `"mc.cores"`,
#'   which can be set for an entire \R session by `options(mc.cores=value)`.
#'   If the `"mc.cores"` option has not been set then the default is `1`.
#'
#'   The rest of the arguments correspond to arguments offered by CmdStan. They
#'   are described briefly here and in greater detail in the CmdStan manual.
#'   Arguments left at `NULL` default to the default used by the installed
#'   version of CmdStan.
#'
#'   * `num_samples`: (positive integer) The number of post-warmup iterations to
#'   run per chain.
#'   * `num_warmup`: (positive integer) The number of warmup iterations to run
#'   per chain.
#'   * `save_warmup`: (logical) Should warmup iterations be saved? The default
#'   is `FALSE`.
#'   * `thin`: (positive integer) The period between saved samples. This should
#'   typically be left at its default (no thinning) unless memory is a problem.
#'   * `max_depth`: (positive integer) The maximum allowed tree depth for the
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
#'   * `stepsize`: (positive real) The _initial_ step size for the discrete
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
#'   * `fixed_param`: (logical) When ``True``, call CmdStan with argument
#'   "algorithm=fixed_param".
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
                          save_extra_diagnostics = FALSE,
                          output_dir = NULL,
                          num_chains = 4,
                          num_cores = getOption("mc.cores", 1),
                          num_warmup = NULL,
                          num_samples = NULL,
                          save_warmup = FALSE,
                          thin = NULL,
                          max_depth = NULL,
                          adapt_engaged = TRUE,
                          adapt_delta = NULL,
                          stepsize = NULL,
                          metric = NULL,
                          metric_file = NULL,
                          inv_metric = NULL,
                          init_buffer = NULL,
                          term_buffer = NULL,
                          window = NULL,
                          fixed_param = FALSE) {

  checkmate::assert_integerish(num_chains, lower = 1, len = 1)
  if (fixed_param) {
    num_chains <- 1
    num_cores <- 1
    save_warmup <- FALSE
  }
  sample_args <- SampleArgs$new(
    num_warmup = num_warmup,
    num_samples = num_samples,
    save_warmup = save_warmup,
    thin = thin,
    max_depth = max_depth,
    adapt_engaged = adapt_engaged,
    adapt_delta = adapt_delta,
    stepsize = stepsize,
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
    run_ids = seq_len(num_chains),
    data_file = process_data(data),
    save_extra_diagnostics = save_extra_diagnostics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir
  )
  cmdstan_procs <- CmdStanProcs$new(num_chains, num_cores)
  runset <- CmdStanRun$new(cmdstan_args, cmdstan_procs)
  runset$run_cmdstan()
  CmdStanMCMC$new(runset)
}
CmdStanModel$set("public", name = "sample", value = sample_method)


#' Run Stan's optimization algorithms
#'
#' @name model-method-optimize
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
#'     save_extra_diagnostics = FALSE,
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
                            save_extra_diagnostics = FALSE,
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
    run_ids = 1,
    data_file = process_data(data),
    save_extra_diagnostics = save_extra_diagnostics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir
  )

  cmdstan_procs <- CmdStanProcs$new(num_runs = 1, num_cores = 1)
  runset <- CmdStanRun$new(cmdstan_args, cmdstan_procs)
  runset$run_cmdstan()

  message(
    "Optimization method is experimental and ",
    "the structure of returned object may change."
  )
  CmdStanMLE$new(runset)
}
CmdStanModel$set("public", name = "optimize", value = optimize_method)


#' Run Stan's variational approximation algorithms
#'
#' @name model-method-variational
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
#'     save_extra_diagnostics = FALSE,
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
#'   * `eta`: (positive real) The stepsize weighting parameter for adaptive
#'   stepsize sequence.
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
                               save_extra_diagnostics = FALSE,
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
    run_ids = 1,
    data_file = process_data(data),
    save_extra_diagnostics = save_extra_diagnostics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir
  )

  cmdstan_procs <- CmdStanProcs$new(num_runs = 1, num_cores = 1)
  runset <- CmdStanRun$new(cmdstan_args, cmdstan_procs)
  runset$run_cmdstan()

  message(
    "Variational method is experimental and ",
    "the structure of returned object may change."
  )
  CmdStanVB$new(runset)
}
CmdStanModel$set("public", name = "variational", value = variational_method)


# Internals ---------------------------------------------------------------

#' Write data to a temporary `.json` file if necessary
#' @noRd
#' @param data If not `NULL`, then either a path to a data file compatible with
#'   CmdStan, or a named list of \R objects to pass to [write_stan_json()].
#' @return Path to data file.
process_data <- function(data) {
  if (is.null(data)) {
    path <- data
  } else if (is.character(data)) {
    path <- absolute_path(data)
  } else if (is.list(data) && !is.data.frame(data)) {
    if (cmdstan_version() >= "2.22") {
      path <- tempfile(pattern = "standata-", fileext = ".json")
      write_stan_json(data = data, file = path)
    } else {
      path <- tempfile(pattern = "standata-", fileext = ".dat")
      if (!requireNamespace("rstan", quietly = TRUE)) {
        stop("For CmdStan < 2.22 the rstan package is required for writing data.")
      }
      rstan::stan_rdump(names(data), file = path, env = list2env(data))
    }
  } else {
    stop("'data' should be a path or a named list.", call. = FALSE)
  }
  path
}
