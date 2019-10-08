#' Create a new CmdStanModel object
#'
#' \if{html}{\figure{logo.png}{options: width="25px" alt="https://mc-stan.org/about/logo/"}}
#' The `cmdstan_model()` function creates a new [`CmdStanModel`] object from a
#' file containing a Stan program.
#'
#' @export
#' @param stan_file Path to Stan program.
#' @return An [`CmdStanModel`] object.
#'
#' @seealso [cmdstan_path()], [install_cmdstan()]
#'
#' @examples
#' \dontrun{
#' set_cmdstan_path("/Users/jgabry/Documents/Stan/cmdstan-2.20.0")
#' my_stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(stan_file = my_stan_program)
#' mod$print()
#' mod$compile()
#'
#' # specify data as a named list (like RStan)
#' standata <- list(N = 10, y =c(0,1,0,0,0,0,0,0,0,1))
#' fit <- mod$sample(data = standata, seed = 123, num_chains = 2)
#' fit$summary()
#'
#' # specify data as a path to a file (like CmdStan)
#' my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.R")
#' fit2 <- mod$sample(data = my_data_file, seed = 123)
#' fit2$summary()
#'
#' # can also create a stanfit object using rstan package
#' # stanfit <- rstan::read_stan_csv(fit$output_files)
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
#'   provides methods for fitting the model. See the **Details** section for
#'   available methods.
#'
#' @details
#' `CmdStanModel` objects have the following associated methods:
#' \describe{
#'   \item{`code()`}{
#'   Returns the Stan program located at `stan_file` as a string.
#'   }
#'   \item{`print()`}{
#'   Prints a more readable version of the Stan program returned by `code()`.
#'   }
#'   \item{`compile()`}{
#'   Compiles the Stan program. Translates the Stan code to C++, then calls the
#'   C++ compiler. The resulting files are placed in the same directory as
#'   `stan_file`.
#'   }
#'   \item{`sample(data = NULL, ...)`}{
#'   Run the default MCMC algorithm in CmdStan (`algorithm=hmc engine=nuts`), to
#'   produce a set of draws from the posterior distribution of a model
#'   conditioned on some data.
#'   Arguments:
#'   * `data`: If not `NULL`, then either a path to a data file compatible with
#'     CmdStan or a named list of \R objects like for RStan.
#'   * `...`: Arguments to pass to CmdStan to control sampling.
#'     TODO: enumerate these instead of `...`.
#'
#'   Return: a [`CmdStanMCMC`] object created from the csv files written by
#'   CmdStan. Those csv files are written to the same directory as `stan_file`.
#'   }
#' }
#'
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
      private$stan_file_ <- repair_path(stan_file)
      invisible(self)
    },
    exe_file = function() private$exe_file_,
    stan_file = function() private$stan_file_,
    code = function() {
      # Get Stan code as a string
      readLines(self$stan_file())
    },
    print = function() {
      # Print readable version of Stan code
      cat(self$code(), sep = "\n")
      invisible(self)
    },

    compile = function() { # TODO: add compiler options?
      # Compile Stan program
      private$exe_file_ <- compile_stan_program(self$stan_file())
      invisible(self)
    },

    sample = function(data = NULL,
                      num_chains = NULL, # TODO: should this default to 1 or 4?
                      # num_cores = NULL,
                      num_warmup = NULL,
                      num_samples = NULL,
                      save_warmup = FALSE, # TODO: document this
                      thin = NULL,
                      refresh = NULL,
                      init = NULL,
                      seed = NULL,
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
          echo_cmd = TRUE,
          echo = TRUE
        )
      }
      CmdStanMCMC$new(runset) # see fit.R
    },

    optimize = function(data = NULL,
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
        echo_cmd = TRUE,
        echo = TRUE
      )
      CmdStanMLE$new(runset)
    }
  )
)


# internals for CmdStanModel methods ---------------------------------------------

#' Compile Stan program
#' @noRd
#' @param stan_file Path to Stan program.
#' @return Path to executable.
compile_stan_program <- function(stan_file) {
  prog <- strip_ext(stan_file)
  prog <- cmdstan_ext(prog) # adds .exe on Windows
  run_log <- processx::run(
    command = "make",
    args = prog,
    wd = cmdstan_path(),
    echo_cmd = TRUE,
    echo = TRUE
  )
  prog
}

#' Write data to a temporary `.data.R` file if necessary
#' @noRd
#' @param data If not `NULL`, then either a path to a data file compatible with
#'   CmdStan, or a named list of \R objects in the style that RStan uses.
#' @return Path to data file.
process_data <- function(data) {
  if (is.null(data)) {
    path <- absolute_path(".")
  } else if (is.character(data)) {
    path <- absolute_path(data)
  } else if (is.list(data) && !is.data.frame(data)) {
    path <- write_rdump(data)
  } else {
    stop("'data' should be a path or a named list.", call. = FALSE)
  }
  path
}

