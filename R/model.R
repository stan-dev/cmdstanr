#' Create a new CmdStanModel object
#'
#' \if{html}{\figure{logo.png}{options: width="25px" alt="https://mc-stan.org/about/logo/"}}
#' The `cmdstan_model()` function creates a new [`CmdStanModel`] object from a
#' file containing a Stan program.
#'
#' @export
#' @param stan_file Path to Stan program.
#' @return A [`CmdStanModel`] object.
#'
#' @seealso [install_cmdstan()], [cmdstan_path()]
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' # Set path to cmdstan
#' # Note: if you installed CmdStan via install_cmdstan() with default settings
#' # then default below should work. Otherwise use the `path` argument to
#' # specify the location of your CmdStan installation.
#'
#' set_cmdstan_path(path = NULL)
#'
#' # Create a CmdStan model object from a Stan program,
#' # here using the example model that comes with CmdStan
#' stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(stan_program)
#' mod$print()
#'
#' # Compile to create executable
#' mod$compile()
#'
#' # Run sample method (MCMC via Stan's dynamic HMC/NUTS),
#' # specifying data as a named list (like RStan)
#' standata <- list(N = 10, y =c(0,1,0,0,0,0,0,0,0,1))
#' fit_mcmc <- mod$sample(data = standata, seed = 123, num_chains = 2)
#'
#' # Call CmdStan's bin/summary
#' fit_mcmc$summary()
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
cmdstan_model <- function(stan_file) {
  CmdStanModel$new(stan_file = stan_file)
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
    initialize = function(stan_file) {
      checkmate::assert_file_exists(stan_file, access = "r", extension = "stan")
      private$stan_file_ <- absolute_path(stan_file)
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
#' @description The `compile` method of a [`CmdStanModel`] object calls CmdStan
#'   to translate a Stan program to C++ and call the C++ compiler. The resulting
#'   files are placed in the same directory as the Stan program.
#'
#' @section Usage:
#'   ```
#'   $compile()
#'   ```
#'
#' @section Value: The `compile` method returns the [`CmdStanModel`] object
#'   invisibly.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
NULL

compile_method <- function() { # TODO: add compiler options?
  exe <- strip_ext(self$stan_file())
  exe <- cmdstan_ext(exe) # adds .exe on Windows
  run_log <- processx::run(
    command = "make",
    args = exe,
    wd = cmdstan_path(),
    echo_cmd = TRUE,
    echo = TRUE
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
#' @description The `sample` method of a [`CmdStanModel`] object runs the default
#'   MCMC algorithm in CmdStan (`algorithm=hmc engine=nuts`), to produce a set
#'   of draws from the posterior distribution of a model conditioned on some
#'   data.
#'
#' @section Usage:
#'   ```
#'   $sample(
#'     num_chains = 1,
#'   # num_cores = NULL, # not yet available
#'     data = NULL,
#'     num_warmup = NULL,
#'     num_samples = NULL,
#'     save_warmup = FALSE,
#'     thin = NULL,
#'     refresh = NULL,
#'     init = NULL,
#'     seed = NULL,
#'     max_depth = NULL,
#'     metric = NULL,
#'     stepsize = NULL,
#'     adapt_engaged = NULL,
#'     adapt_delta = NULL
#'   )
#'   ```
#'
#' @template model-common-args
#' @section Arguments unique to the `sample` method: In addition to the
#'   arguments above, the `sample` method also has its own set of arguments.
#'   These arguments are described briefly here and in greater detail in the
#'   CmdStan manual. Arguments left at `NULL` default to the default used by the
#'   installed version of CmdStan.
#'   * `num_samples`: (positive integer) The number of sampling iterations.
#'   * `num_warmup`: (positive integer) The number of warmup iterations.
#'   * `save_warmup`: (logical) Should warmup iterations also be streamed
#'     to the output?
#'   * `thin`: (positive integer) The period between saved samples. This should
#'     typically be left at its default (no thinning).
#'   * `adapt_engaged`: (logical) Do warmup adaptation?
#'   * `adapt_delta`: (real in `(0,1)`) The adaptation target acceptance
#'     statistic.
#'   * `stepsize`: (positive real) The _initial_ step size for the discrete
#'     approximation to continuous Hamiltonian dynamics. This is further tuned
#'     during warmup.
#'   * `metric`: (character) The geometry of the base manifold. One of the
#'     following:
#'      - A single string from among `"diag_e"`, `"dense_e"`, `"unit_e"`;
#'      - A character vector containing paths to files (one per chain)
#'        compatible with CmdStan that contain precomputed metrics.
#'        Each path must be to a JSON or Rdump file that contains an entry
#'        `inv_metric` whose value is either the diagonal vector or the full
#'        covariance matrix.
#'
#'     If you want to turn off adaptation when using a precomuted metric set
#'     `adapt_engaged=FALSE`, otherwise it will use the precomputed metric just
#'     as an initial guess during adaptation. See the _Euclidean Metric_ section
#'     of the CmdStan manual for more details on these options.
#'
#'   * `max_depth`: (positive integer) The maximum allowed tree depth. See the
#'     _Tree Depth_ section of the CmdStan manual for more details.
#'
#' @section Value: The `sample` method returns a [`CmdStanMCMC`] object.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
NULL

sample_method <- function(data = NULL,
                          seed = NULL,
                          refresh = NULL,
                          init = NULL,
                          num_chains = NULL, # TODO: CmdStan does 1 chain, but should this default to 4?
                          # num_cores = NULL,
                          num_warmup = NULL,
                          num_samples = NULL,
                          save_warmup = FALSE, # TODO: document this
                          thin = NULL,
                          max_depth = NULL,
                          metric = NULL,
                          stepsize = NULL,
                          adapt_engaged = NULL,
                          adapt_delta = NULL) {

  num_chains <- num_chains %||% 1
  checkmate::assert_integerish(num_chains, lower = 1)
  chain_ids <- seq_len(num_chains)

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
    run_ids = chain_ids,
    data_file = process_data(data),
    seed = seed,
    init = init,
    refresh = refresh
  )

  runset <- RunSet$new(args = cmdstan_args, num_runs = num_chains)
  csv_files <- runset$output_files()
  for (chain in chain_ids) { # FIXME: allow parallelization
    run_log <- processx::run(
      command = cmdstan_args$compose_command(),
      args = cmdstan_args$compose_all_args(chain, csv_files[chain]),
      wd = dirname(self$exe_file()),
      echo_cmd = FALSE,
      echo = TRUE
    )
  }
  CmdStanMCMC$new(runset) # see fit.R
}
CmdStanModel$set("public", name = "sample", value = sample_method)


#' Run Stan's optimization algorithms
#'
#' @name model-method-optimize
#' @family CmdStanModel methods
#'
#' @description The `optimize` method of a [`CmdStanModel`] object runs Stan's
#'   optimizer.
#'
#' @details CmdStan can find the posterior mode (assuming there is one). If the
#'   posterior is not convex, there is no guarantee Stan will be able to find
#'   the global mode as opposed to a local optimum of log probability. For
#'   optimization, the mode is calculated without the Jacobian adjustment for
#'   con- strained variables, which shifts the mode due to the change of
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
#'     algorithm = NULL,
#'     init_alpha = NULL,
#'     iter = NULL
#'   )
#'   ```
#'
#' @template model-common-args
#' @section Arguments unique to the `optimize` method: In addition to the
#'   arguments above, the `optimize` method also has its own set of arguments.
#'   These arguments are described briefly here and in greater detail in the
#'   CmdStan manual. Arguments left at `NULL` default to the default used by the
#'   installed version of CmdStan.
#'   * `algorithm`: (string) The optimization algorithm. One of
#'     `"lbfgs"`, `"bfgs"`, or `"newton"`.
#'   * `iter`: (positive integer) The number of iterations.
#'   * `init_alpha`: (non-negative real) The line search step size for first
#'      iteration. Not applicable if `algorithm="newton"`.
#'
#' @section Value: The `optimize` method returns a [`CmdStanMLE`] object.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
NULL

optimize_method <- function(data = NULL,
                            seed = NULL,
                            refresh = NULL,
                            init = NULL,
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
    seed = seed,
    init = init,
    refresh = refresh
  )

  runset <- RunSet$new(args = cmdstan_args, num_runs = 1)
  run_log <- processx::run(
    command = cmdstan_args$compose_command(),
    args = cmdstan_args$compose_all_args(idx = 1, runset$output_files()[1]),
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
#' @description The `variational` method of a [`CmdStanModel`] object runs
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
#'   arguments above, the `variational` method also has its own set of
#'   arguments. These arguments are described briefly here and in greater detail
#'   in the CmdStan manual. Arguments left at `NULL` default to the default used
#'   by the installed version of CmdStan.
#'   * `algorithm`: (string) The algorithm. Either `"meanfield"` or `"fullrank"`.
#'   * `iter`: (positive integer) The _maximum_ number of iterations.
#'   * `grad_samples`: (positive integer) The number of samples for Monte Carlo
#'     estimate of gradients.
#'   * `elbo_samples`: (positive integer) The number of samples for Monte Carlo
#'     estimate of ELBO (objective function).
#'   * `eta`: (positive real) The stepsize weighting parameter for adaptive
#'     stepsize sequence.
#'   * `adapt_engaged`: (logical) Do warmup adaptation?
#'   * `adapt_iter`: (positive integer) The _maximum_ number of adaptation
#'     iterations.
#'   * `tol_rel_obj`: (positive real) Convergence tolerance on the relative norm
#'     of the objective.
#'   * `eval_elbo`: (positive integer) Evaluate ELBO every Nth iteration.
#'   * `output_samples:` (positive integer) Number of posterior samples to
#'     draw and save.
#'
#' @section Value: The `variational` method returns a [`CmdStanVB`] object.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
NULL

variational_method <- function(data = NULL,
                               seed = NULL,
                               refresh = NULL,
                               init = NULL,
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
    seed = seed,
    init = init,
    refresh = refresh
  )

  runset <- RunSet$new(args = cmdstan_args, num_runs = 1)
  run_log <- processx::run(
    command = cmdstan_args$compose_command(),
    args = cmdstan_args$compose_all_args(idx = 1, runset$output_files()[1]),
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
    path <- write_json(data)
  } else {
    stop("'data' should be a path or a named list.", call. = FALSE)
  }
  path
}
