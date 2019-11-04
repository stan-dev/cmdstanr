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
#'   [`$compile()`][model-method-compile] method. Ignored if `compile=FALSE`.
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
#' # Run sample method (MCMC via Stan's dynamic HMC/NUTS),
#' # specifying data as a named list (like RStan)
#' standata <- list(N = 10, y =c(0,1,0,0,0,0,0,0,0,1))
#' fit_mcmc <- mod$sample(data = standata, seed = 123, num_chains = 2)
#'
#' # Call CmdStan's bin/stansummary
#' fit_mcmc$summary()
#'
#' # Call CmdStan's bin/diagnose
#' fit_mcmc$diagnose()
#'
#' # Run optimization method (default is Stan's LBFGS algorithm)
#' # and also demonstrate specifying data as a path to a file (readable by CmdStan)
#' my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.R")
#' fit_optim <- mod$optimize(data = my_data_file, seed = 123)
#'
#' #' Print estimates
#' fit_optim$summary()
#'
#' # Run variational Bayes method (default is meanfield ADVI)
#' fit_vb <- mod$variational(data = standata, seed = 123)
#'
#' # Call CmdStan's bin/summary
#' fit_vb$summary()
#'
#' # For models fit using MCMC, if you like working with RStan's stanfit objects
#' # then you can create one with rstan::read_stan_csv()
#' if (require(rstan, quietly = TRUE)) {
#'   stanfit <- rstan::read_stan_csv(fit_mcmc$output_files())
#'   print(stanfit)
#' }
#'
#' }
#'
cmdstan_model <- function(stan_file, compile = TRUE, ...) {
  CmdStanModel$new(stan_file = stan_file, compile = compile, ...)
}


# CmdStanModel -----------------------------------------------------------------

#' CmdStanModel objects
#'
#' @name CmdStanModel
#' @description A `CmdStanModel` object is an [R6][R6::R6] object returned by
#'   the [cmdstan_model()] function. The object stores the path to a Stan
#'   program as well as a path to a compiled executable once created, and
#'   provides methods for fitting the model. See **Details** section for
#'   available methods.
#'
#' @details
#' `CmdStanModel` objects have the following associated methods:
#'
#' \tabular{ll}{
#'  **Method** \tab **Description** \cr
#'  code \tab Return Stan program as a string. \cr
#'  print \tab Print readable version of Stan program. \cr
#'  stan_file \tab Return the file path to the Stan program. \cr
#'  exe_file \tab Return the file path to the compiled executable once compiled. \cr
#'  [compile][model-method-compile] \tab Compile Stan program. \cr
#'  [sample][model-method-sample]
#'    \tab Run CmdStan's `"sample"` method, return [`CmdStanMCMC`] object. \cr
#'  [optimize][model-method-optimize]
#'    \tab Run CmdStan's `"optimize"` method, return [`CmdStanMLE`] object. \cr
#'  [variational][model-method-variational]
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
    exe_file_ = character()
  ),
  public = list(
    initialize = function(stan_file, compile, ...) {
      checkmate::assert_file_exists(stan_file, access = "r", extension = "stan")
      checkmate::assert_flag(compile)
      private$stan_file_ <- absolute_path(stan_file)
      if (compile) {
        message("Compiling Stan program...")
        self$compile(...)
      }
      invisible(self)
    },
    stan_file = function() private$stan_file_,
    exe_file = function() private$exe_file_,
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
#' @description The `$compile()` method of a [`CmdStanModel`] object calls CmdStan
#'   to translate a Stan program to C++ and then create a compiled executable.
#'   The resulting files are placed in the same directory as the Stan program
#'   associated with the `CmdStanModel` object. After compilation the path
#'   to the executable can be accesed via the `$exe_file()` method.
#'
#' @section Usage:
#'   ```
#'   $compile(
#'     quiet = TRUE,
#'     threads = FALSE,
#'     opencl = FALSE,
#'     opencl_platform_id = 0,
#'     opencl_device_id = 0,
#'     compiler_flags = NULL
#'   )
#'   $exe_file()
#'   ```
#'
#' @section Arguments:
#'   Leaving all arguments at their defaults should be fine for most users, but
#'   optional arguments are provided to enable new features in CmdStan (and the
#'   Stan Math library). See the CmdStan manual for more details.
#'   * `quiet`: (logical) Should the verbose output from CmdStan during
#'     compilation be suppressed? The default is `TRUE`, but if you encounter an
#'     error we recommend trying again with `quiet=FALSE` to see more of the
#'     output.
#'   * `threads`: (logical) Should the model be compiled with
#'     [threading support](https://github.com/stan-dev/math/wiki/Threading-Support)?
#'     If `TRUE` then `-DSTAN_THREADS` is added to the compiler flags. See
#'     [set_num_threads()] to set the number of threads, which is read by
#'     CmdStan at run-time from an environment variable.
#'   * `opencl`: (logical) Should the model be compiled with OpenCL support enabled?
#'   * `opencl_platform_id`: (nonnegative integer) The ID of the OpenCL platform on which
#'     to run the compiled model.
#'   * `opencl_device_id`: (nonnegative integer) The ID of the OpenCL device on the selected
#'     OpenCL platform on which to run the compiled model.
#'   * `compiler_flags`: (character vector) Any additional compiler flags to be
#'     used when compiling the model.
#'
#' @section Value: This method is called for its side effect of creating the
#'   executable and adding its path to the [`CmdStanModel`] object, but it also
#'   returns the [`CmdStanModel`] object invisibly.
#'
#' @template seealso-docs
#'
#' @examples
#' stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(stan_program, compile = FALSE)
#' mod$compile()
#' mod$exe_file()
#'
NULL

compile_method <- function(quiet = TRUE,
                           threads = FALSE,
                           opencl = FALSE,
                           opencl_platform_id = 0,
                           opencl_device_id = 0,
                           compiler_flags = NULL) {
  exe <- strip_ext(self$stan_file())
  make_local_changed <- set_make_local(threads,
                                       opencl,
                                       opencl_platform_id,
                                       opencl_device_id,
                                       compiler_flags)
  # rebuild main.o and the model if there was a change in make/local
  if (make_local_changed) {
    message("A change in the compiler flags was found. Forcing recompilation.\n")
    build_cleanup(exe, remove_main = TRUE)
  }
  # add path to the build tbb library to the PATH variable to avoid copying the dll file
  if (cmdstan_version() >= "2.21" && os_is_windows()) {
    path_to_TBB <- file.path(cmdstan_path(), "stan", "lib", "stan_math", "lib", "tbb")
    Sys.setenv(PATH = paste0(path_to_TBB, ";", Sys.getenv("PATH")))
  }

  exe <- cmdstan_ext(exe) # adds .exe on Windows
  run_log <- processx::run(
    command = make_cmd(),
    args = exe,
    wd = cmdstan_path(),
    echo_cmd = !quiet,
    echo = !quiet,
    spinner = quiet
  )

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
#'     save_diagnostics = FALSE,
#'     num_chains = NULL,
#'     # num_cores = NULL, # not yet implemented
#'     num_warmup = NULL,
#'     num_samples = NULL,
#'     save_warmup = FALSE,
#'     thin = NULL,
#'     max_depth = NULL,
#'     metric = NULL,
#'     stepsize = NULL,
#'     adapt_engaged = TRUE,
#'     adapt_delta = NULL
#'   )
#'   ```
#'
#' @template model-common-args
#' @section Arguments unique to the `sample` method: In addition to the
#'   arguments above, the `$sample()` method also has its own set of arguments.
#'
#'   The following arguments are offered by CmdStanR but not CmdStan:
#'
#'   * `num_chains`: (positive integer) The number of Markov chains to run. The
#'   default is 4. The chains will run in parallel and be scheduled to free CPU
#'   cores by the OS. Limiting the number of chains that run in parallel is not
#'   yet possible but is in the works. This argument does not correspond to an
#'   argument in CmdStan because all CmdStan arguments pertain to the execution
#'   of a single run only.
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
#'   typically be left at its default (no thinning).
#'   * `adapt_engaged`: (logical) Do warmup adaptation?
#'   * `adapt_delta`: (real in `(0,1)`) The adaptation target acceptance
#'   statistic.
#'   * `stepsize`: (positive real) The _initial_ step size for the discrete
#'   approximation to continuous Hamiltonian dynamics. This is further tuned
#'   during warmup.
#'   * `metric`: (character) The geometry of the base manifold. See the
#'   _Euclidean Metric_ section of the CmdStan documentation for more details.
#'   One of the following:
#'     - A single string from among `"diag_e"`, `"dense_e"`, `"unit_e"`.
#'     - A character vector containing paths to files (one per chain) compatible
#'     with CmdStan that contain precomputed metrics. Each path must be to a
#'     JSON or Rdump file that contains an entry `inv_metric` whose value is
#'     either the diagonal vector or the full covariance matrix. If
#'     `adapt_engaged=TRUE`, Stan will use the provided metric just as an
#'     initial guess during adaptation. To turn off adaptation when using a
#'     precomputed metric set `adapt_engaged=FALSE`.
#'   * `max_depth`: (positive integer) The maximum allowed tree depth for the
#'   NUTS engine. See the _Tree Depth_ section of the CmdStan manual for more
#'   details.
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
                          save_diagnostics = FALSE,
                          num_chains = 4,
                          # num_cores = NULL, # TODO
                          num_warmup = NULL,
                          num_samples = NULL,
                          save_warmup = FALSE,
                          thin = NULL,
                          adapt_engaged = TRUE,
                          adapt_delta = NULL,
                          metric = NULL,
                          stepsize = NULL,
                          max_depth = NULL) {
  # cleanup any background processes on error or interrupt
  on.exit(
    {
      if (exists("procs")) {
        lapply(procs, function(x) { x$kill_tree() })
      }
    }, add = TRUE
  )
  num_chains <- num_chains %||% 1
  checkmate::assert_integerish(num_chains, lower = 1)

  sample_args <- SampleArgs$new(
    num_warmup = num_warmup,
    num_samples = num_samples,
    save_warmup = save_warmup,
    thin = thin,
    max_depth = max_depth,
    metric = metric,
    stepsize = stepsize,
    adapt_engaged = adapt_engaged,
    adapt_delta = adapt_delta
  )
  cmdstan_args <- CmdStanArgs$new(
    method_args = sample_args,
    model_name = strip_ext(basename(self$exe_file())),
    exe_file = self$exe_file(),
    run_ids = seq_len(num_chains),
    data_file = process_data(data),
    save_diagnostics = save_diagnostics,
    seed = seed,
    init = init,
    refresh = refresh
  )

  runset <- RunSet$new(args = cmdstan_args, num_runs = num_chains)
  procs <- list()
  cat("Running MCMC with", num_chains, "chain(s) ...\n")
  for (chain_id in runset$run_ids()) {
    procs[[chain_id]] <- processx::process$new(
      command = runset$command(),
      args = runset$command_args()[[chain_id]],
      wd = dirname(self$exe_file()),
      echo_cmd = TRUE,
      stdout = "|",
      stderr = "|"
    )
    runset$mark_chain_start(chain_id)
  }
  while (any_chain_alive(procs, runset)) {
    processx::poll(procs, 100)
    for (chain_id in runset$run_ids()) {
      output <- procs[[chain_id]]$read_output_lines()
      runset$process_sample_output(output, chain_id)
    }
  }
  if (num_chains > 1) {
    num_failed_chains <- num_chains - sum(runset$chain_state() == 5)
    if (num_failed_chains == 0) {
        if (num_chains == 2) {
          cat("Both chains finished succesfully.\n")
        } else {
          cat(paste0("All ", num_chains," chains finished succesfully.\n"))
        }
        cat(paste0("Mean execution time: ",
                   format(round(mean(runset$time()$total_time),1), nsmall = 1),
                   " seconds"))
    } else {
      if (num_failed_chains == num_chains) {
        cat("All chains finished unexpectedly!\n")
      } else {
        cat(num_failed_chains, "chain(s) finished unexpectedly!\n")
        cat("The remaining chains had a mean execution time of",
            format(round(mean(runset$time()$total_time), 1), nsmall = 1),
            "seconds")
      }
    }
  }
  CmdStanMCMC$new(runset) # see fit.R
}
CmdStanModel$set("public", name = "sample", value = sample_method)

any_chain_alive <-function(procs, runset) {
  alive <- FALSE
  for (id in runset$run_ids()) {
    if (procs[[id]]$is_alive()) {
      alive <- TRUE
    }
    if ((runset$chain_state(id) < 5) && !procs[[id]]$is_alive()) {
      #if the chain just finished make sure we process all
      output <- procs[[id]]$read_output_lines()
      runset$process_sample_output(output, id)
      runset$mark_chain_stop(id)
    }
  }
  alive
}

#' Run Stan's optimization algorithms
#'
#' @name model-method-optimize
#' @family CmdStanModel methods
#'
#' @description The `$optimize()` method of a [`CmdStanModel`] object runs
#'   Stan's optimizer.
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
#'     save_diagnostics = FALSE,
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
#'   * `init_alpha`: (non-negative real) The line search step size for first
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
                            save_diagnostics = FALSE,
                            algorithm = NULL,
                            init_alpha = NULL,
                            iter = NULL) {

  warning(
    "Optimization method is experimental and ",
    "the structure of returned object may change.",
    call. = FALSE
  )

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
    save_diagnostics = save_diagnostics,
    seed = seed,
    init = init,
    refresh = refresh
  )

  runset <- RunSet$new(args = cmdstan_args, num_runs = 1)
  run_log <- processx::run(
    command = runset$command(),
    args = runset$command_args()[[1]],
    wd = dirname(self$exe_file()),
    echo_cmd = FALSE,
    echo = TRUE
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
#'     save_diagnostics = FALSE,
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
                               save_diagnostics = FALSE,
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

  warning(
    "Variational inference method is experimental and ",
    "the structure of returned object may change.",
    call. = FALSE
  )

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
    save_diagnostics = save_diagnostics,
    seed = seed,
    init = init,
    refresh = refresh
  )

  runset <- RunSet$new(args = cmdstan_args, num_runs = 1)
  run_log <- processx::run(
    command = runset$command(),
    args = runset$command_args()[[1]],
    wd = dirname(self$exe_file()),
    echo_cmd = FALSE,
    echo = TRUE
  )
  CmdStanVB$new(runset)
}
CmdStanModel$set("public", name = "variational", value = variational_method)


# Internals ---------------------------------------------------------------

#' Write data to a temporary `.json` file if necessary
#' @noRd
#' @param data If not `NULL`, then either a path to a data file compatible with
#'   CmdStan, or a named list of \R objects in the style that RStan uses.
#' @return Path to data file.
process_data <- function(data) {
  if (is.null(data)) {
    path <- data
  } else if (is.character(data)) {
    path <- absolute_path(data)
  } else if (is.list(data) && !is.data.frame(data)) {
    path <- tempfile(pattern = "standata-", fileext = ".json")
    write_stan_json(
      data = data,
      file = path
    )
  } else {
    stop("'data' should be a path or a named list.", call. = FALSE)
  }
  path
}
