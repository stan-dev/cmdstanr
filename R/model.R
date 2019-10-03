#' Create a new CmdStanModel object
#'
#' @export
#' @param stan_file Path to Stan program.
#' @return An [R6][R6::R6] `CmdStanModel` object. See **Details**.
#'
#' @details A `CmdStanModel` object stores the path to a Stan program as well as a
#'   path to a compiled executable once created. The following methods are
#'   available:
#' \describe{
#'   \item{`code()`}{
#'   Returns the Stan program located at `stan_file` as a string.
#'   }
#'   \item{`print()`}{
#'   Prints a more readable version of the Stan program returned by `code()`.
#'   }
#'   \item{`compile()`}{
#'   Compiles the Stan program. Translates the Stan code to C++, then calls the
#'   C++ compiler.
#'   }
#'   \item{`sample(data = NULL, ...)`}{
#'   Run the default MCMC algorithm in CmdStan (`algorithm=hmc engine=nuts`), to
#'   produce a set of draws from the posterior distribution of a model
#'   conditioned on some data. Arguments:
#'   * `data`: If not `NULL`, then either a path to a data file compatible with
#'     CmdStan or a named list of \R objects like for RStan.
#'   * `...`: Arguments to pass to CmdStan to control sampling.
#'     TODO: enumerate these instead of `...`.
#'   Returns a `CmdStanFit` object.
#'   }
#' }
#'
#' @examples
#' set_cmdstan_path("/Users/jgabry/Documents/Stan/cmdstan-2.20.0")
#' my_stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(stan_file = my_stan_program)
#' mod$print()
#' mod$compile()
#'
#' # specify data as a named list (like RStan)
#' standata <- list(N = 10, y =c(0,1,0,0,0,0,0,0,0,1))
#' fit <- mod$sample(data = standata, seed = 123)
#' fit$summary()
#'
#' # specify data as a path to a file (like CmdStan)
#' my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.R")
#' fit2 <- mod$sample(data = my_data_file, seed = 123)
#' fit2$summary()
#'
#' # can create a stanfit object using rstan package
#' # stanfit <- rstan::read_stan_csv(fit$output_files)
#'
cmdstan_model <- function(stan_file) {
  CmdStanModel$new(stan_file = stan_file)
}


# CmdStanModel -----------------------------------------------------------------
CmdStanModel <- R6::R6Class(
  classname = "CmdStanModel",
  public = list(
    stan_file = character(),
    exe_file = character(),
    initialize = function(stan_file) {
      if (!file.exists(stan_file)) {
        stop("'stan_file' does not exist.", call. = FALSE)
      } else if (!has_stan_ext(stan_file)) {
        stop("'stan_file' must have '.stan' extension.", call. = FALSE)
      }
      self$stan_file <- stan_file
      invisible(self)
    },
    code = function() {
      # Get Stan code as a string
      readLines(self$stan_file)
    },
    print = function() {
      # Print readable version of Stan code
      cat(self$code(), sep = "\n")
      invisible(self)
    },
    compile = function() { # TODO: add compiler options?
      # Compile Stan program
      self$exe_file <- compile_stan_program(self$stan_file)
      invisible(self)
    },
    sample = function(data = NULL, ...) { # TODO: enumerate args instead of using `...`
      if (!length(self$exe_file)) {
        stop("Can't find executable. Try running the compile() method before sample().",
             call. = FALSE)
      }
      data_file <- write_data(data)
      output_files <- sample_hmc_nuts(self$exe_file, data_file = data_file, ...)
      CmdStanFit$new(output_files) # see stanfit.R
    }
  )
)


# internals for CmdStanModel methods ---------------------------------------------

#' Check for .stan file extension
#' @noRd
#' @param stan_file Path to Stan program.
#' @return T/F
has_stan_ext <- function(stan_file) {
  stopifnot(is.character(stan_file))
  file <- basename(stan_file)
  isTRUE(substr(file, nchar(file) - 4, nchar(file)) == ".stan")
}

#' Strip the .stan file extension from a path
#' @noRd
#' @param stan_file Path to Stan program.
#' @return `stan_file` but without the suffix `.stan`
strip_stan_ext <- function(stan_file) {
  substr(stan_file, 1, nchar(stan_file) - 5)
}

#' Compile Stan program
#' @noRd
#' @param stan_file Path to Stan program.
#' @return Path to executable.
compile_stan_program <- function(stan_file) {
  prog <- strip_cmdstan_path(stan_file) # relative path
  prog <- strip_stan_ext(prog)

  # using base::system()
  # cmd <- paste0("cd ", cmdstan_path(), " && make ", cmdstan_ext(prog))
  # system(cmd)

  out <- processx::run(
    command = "make",
    args = cmdstan_ext(prog),
    wd = cmdstan_path(),
    echo_cmd = TRUE,
    echo = TRUE
  )

  # return full path to exe
  exe_file <- strip_stan_ext(stan_file)
  cmdstan_ext(exe_file)
}

#' Either return Write data to a temporary `.data.R` file
#' @noRd
#' @param data If not `NULL`, then either a path to a data file compatible with
#'   CmdStan, or a named list of \R objects in the style that RStan uses.
#' @return Path to data file.
write_data <- function(data) {
  if (is.null(data)) {
    path <- ""
  } else if (is.character(data)) {
    path <- data
  } else if (is.list(data) && !is.data.frame(data)) {
    path <- write_rdump(data)
  } else {
    stop("'data' should be a path or a named list.", call. = FALSE)
  }
  path
}

#' Run the HMC-NUTS sampler (`algorithm=hmc engine=nuts`)
#'
#' @noRd
#' @param exe_file Path to compiled object.
#' @param data_file Path to data file.
#' @return Path(s) to output csv file(s).
#'
sample_hmc_nuts <- function(exe_file,
                            data_file = "",
                            refresh = 100,
                            seed = -1,
                            num_chains = 4,
                            num_samples = 1000,
                            num_warmup = 1000,
                            save_warmup = FALSE,
                            adapt_engaged = TRUE,
                            adapt_delta = 0.8,
                            stepsize = 1,
                            stepsize_jitter = 0,
                            metric = "diag_e",
                            # metric_file = "",
                            max_depth = 10) {

  # FIXME: this is just a placeholder until we implement something more modular
  # like in cmdstanpy.

  cmd <- paste0(
    exe_file,
    " sample",
      " thin=1",
      " num_samples=", num_samples,
      " num_warmup=", num_warmup,
      " save_warmup=", as.integer(save_warmup),
      " adapt",
        " engaged=", as.integer(adapt_engaged),
        " delta=", adapt_delta,
      " algorithm=hmc",
        " engine=nuts",
          " max_depth=", max_depth,
        " metric=", metric,
        # " metric_file=", metric_file,
        " stepsize=", stepsize,
        " stepsize_jitter=", stepsize_jitter,
    " data",
      " file=", data_file,
    " random",
      " seed=", seed,
    " id=", make_id(num_chains),
    " output",
      " file=", paste0(exe_file, "-samples", make_id(num_chains), ".csv"),
      " refresh=", refresh
  )

  if (num_chains > 1) {
    if (os_is_windows()) {
      cmd <- paste0("for /l %x in (1, 1, ", num_chains,")",
                    "do start /b ", cmd)
    } else {
      cmd <- paste0("for i in {1..", num_chains, "}\n",
                    "do\n", cmd, " &\n", "done")
    }
  }

  ## Run CmdStan
  system(cmd)

  output_files <- paste0(exe_file, "-samples", seq_len(num_chains), ".csv")
  invisible(output_files)
}

make_id <- function(num_chains) {
  if (num_chains == 1) {
    return(1)
  }
  if (os_is_windows()) "%x" else "$i"
}

