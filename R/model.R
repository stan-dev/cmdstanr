#' Create a new CmdStanModel object
#'
#' \if{html}{\figure{logo.png}{options: width="25px" alt="https://mc-stan.org/about/logo/"}}
#' Create a new [`CmdStanModel`] object from a file containing a Stan program.
#'
#' @export
#' @param stan_file The path to a `.stan` file containing a Stan program. The
#'   helper function [write_stan_file()] is provided for cases when it is
#'   more convenient to specify the Stan program as a string.
#' @param compile Do compilation? The default is `TRUE`. If `FALSE`
#'   compilation can be done later via the [`$compile()`][model-method-compile]
#'   method.
#' @param ... Optionally, additional arguments to pass to the
#'   [`$compile()`][model-method-compile] method if `compile=TRUE`.
#'
#' @return A [`CmdStanModel`] object.
#'
#' @seealso [install_cmdstan()], [`$compile()`][model-method-compile],
#'   [`$check_syntax()`][model-method-check_syntax]
#'
#'
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' library(cmdstanr)
#' library(posterior)
#' library(bayesplot)
#' color_scheme_set("brightblue")
#'
#' # Set path to CmdStan
#' # (Note: if you installed CmdStan via install_cmdstan() with default settings
#' # then setting the path is unnecessary but the default below should still work.
#' # Otherwise use the `path` argument to specify the location of your
#' # CmdStan installation.)
#' set_cmdstan_path(path = NULL)
#'
#' # Create a CmdStanModel object from a Stan program,
#' # here using the example model that comes with CmdStan
#' file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(file)
#' mod$print()
#'
#' # Data as a named list (like RStan)
#' stan_data <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
#'
#' # Run MCMC using the 'sample' method
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
#'
#' # Run 'variational' method to approximate the posterior (default is meanfield ADVI)
#' fit_vb <- mod$variational(data = stan_data, seed = 123)
#'
#' fit_vb$summary()
#'
#' # Plot approximate posterior using bayesplot
#' mcmc_hist(fit_vb$draws("theta"))
#'
#'
#' # Specifying initial values as a function
#' fit_mcmc_w_init_fun <- mod$sample(
#'   data = stan_data,
#'   seed = 123,
#'   chains = 2,
#'   refresh = 0,
#'   init = function() list(theta = runif(1))
#' )
#' fit_mcmc_w_init_fun_2 <- mod$sample(
#'   data = stan_data,
#'   seed = 123,
#'   chains = 2,
#'   refresh = 0,
#'   init = function(chain_id) {
#'     # silly but demonstrates optional use of chain_id
#'     list(theta = 1 / (chain_id + 1))
#'   }
#' )
#' fit_mcmc_w_init_fun_2$init()
#'
#' # Specifying initial values as a list of lists
#' fit_mcmc_w_init_list <- mod$sample(
#'   data = stan_data,
#'   seed = 123,
#'   chains = 2,
#'   refresh = 0,
#'   init = list(
#'     list(theta = 0.75), # chain 1
#'     list(theta = 0.25)  # chain 2
#'   )
#' )
#' fit_optim_w_init_list <- mod$optimize(
#'   data = stan_data,
#'   seed = 123,
#'   init = list(
#'     list(theta = 0.75)
#'   )
#' )
#' fit_optim_w_init_list$init()
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
#'   fitting the model using Stan's algorithms.
#'
#' @section Methods: `CmdStanModel` objects have the following associated
#'   methods, many of which have their own (linked) documentation pages:
#'
#'  ## Stan code
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  `$stan_file()` | Return the file path to the Stan program. |
#'  `$code()` | Return Stan program as a string. |
#'  `$print()`|  Print readable version of Stan program. |
#'  [`$check_syntax()`][model-method-check_syntax]  |  Check Stan syntax without having to compile. |
#'
#'  ## Compilation
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$compile()`][model-method-compile]  |  Compile Stan program. |
#'  [`$exe_file()`][model-method-compile] |  Return the file path to the compiled executable. |
#'  [`$hpp_file()`][model-method-compile] |  Return the file path to the `.hpp` file containing the generated C++ code. |
#'  [`$save_hpp_file()`][model-method-compile] |  Save the `.hpp` file containing the generated C++ code. |
#'
#'  ## Model fitting
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$sample()`][model-method-sample] |  Run CmdStan's `"sample"` method, return [`CmdStanMCMC`] object. |
#'  [`$sample_mpi()`][model-method-sample_mpi] |  Run CmdStan's `"sample"` method with [MPI](https://mc-stan.org/math/mpi.html), return [`CmdStanMCMC`] object. |
#'  [`$optimize()`][model-method-optimize] |  Run CmdStan's `"optimize"` method, return [`CmdStanMLE`] object. |
#'  [`$variational()`][model-method-variational] |  Run CmdStan's `"variational"` method, return [`CmdStanVB`] object. |
#'  [`$generate_quantities()`][model-method-generate-quantities] |  Run CmdStan's `"generate quantities"` method, return [`CmdStanGQ`] object. |
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
CmdStanModel <- R6::R6Class(
  classname = "CmdStanModel",
  private = list(
    stan_file_ = character(),
    exe_file_ = character(),
    hpp_file_ = character(),
    dir_ = NULL,
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
      check_stanc_options(args$stanc_options)
      private$precompile_cpp_options_ <- args$cpp_options %||% list()
      private$precompile_stanc_options_ <- args$stanc_options %||% list()
      private$precompile_include_paths_ <- args$include_paths
      private$dir_ <- args$dir

      if (compile) {
        self$compile(...)
      }
      invisible(self)
    },

    code = function() {
      readLines(self$stan_file())
    },
    print = function() {
      cat(self$code(), sep = "\n")
      invisible(self)
    },
    stan_file = function() {
      private$stan_file_
    },
    exe_file = function(path = NULL) {
      if (!is.null(path)) {
        private$exe_file_ <- path
      }
      private$exe_file_
    },
    cpp_options = function() {
      private$cpp_options_
    },
    hpp_file = function() {
      if (!length(private$hpp_file_)) {
        stop("The .hpp file does not exists. Please (re)compile the model.", call. = FALSE)
      }
      private$hpp_file_
    },
    save_hpp_file = function(dir = NULL) {
      if (is.null(dir)) {
        dir <- dirname(private$stan_file_)
      }
      checkmate::assert_directory_exists(dir, access = "r")
      new_hpp_loc <- file.path(dir, paste0(strip_ext(basename(private$stan_file_)), ".hpp"))
      file.copy(self$hpp_file(), new_hpp_loc, overwrite = TRUE)
      file.remove(self$hpp_file())
      message("Moved .hpp file and set internal path to new location:\n",
              "- ", new_hpp_loc)
      private$hpp_file_ <- new_hpp_loc
      invisible(private$hpp_file_)
    }
  )
)

# CmdStanModel methods -----------------------------------

#' Compile a Stan program
#'
#' @name model-method-compile
#' @aliases compile
#' @family CmdStanModel methods
#'
#' @description The `$compile()` method of a [`CmdStanModel`] object checks the
#'   syntax of the Stan program, translates the program to C++, and creates a
#'   compiled executable. To just check the syntax of a Stan program without
#'   compiling it use the [`$check_syntax()`][model-method-check_syntax] method
#'   instead.
#'
#'   In most cases the user does not need to explicitly call the `$compile()`
#'   method as compilation will occur when calling [cmdstan_model()]. However it
#'   is possible to set `compile=FALSE` in the call to `cmdstan_model()` and
#'   subsequently call the `$compile()` method directly.
#'
#'   After compilation, the paths to the executable and the `.hpp` file
#'   containing the generated C++ code are available via the `$exe_file()` and
#'   `$hpp_file()` methods. The default is to create the executable in the same
#'   directory as the Stan program and to write the generated C++ code in a
#'   temporary directory. To save the C++ code to a non-temporary location use
#'   `$save_hpp_file(dir)`.
#'
#' @param quiet (logical) Should the verbose output from CmdStan during
#'   compilation be suppressed? The default is `TRUE`, but if you encounter an
#'   error we recommend trying again with `quiet=FALSE` to see more of the
#'   output.
#' @param dir (string) The path to the directory in which to store the CmdStan
#'   executable (or `.hpp` file if using `$save_hpp_file()`). The default is the
#'   same location as the Stan program.
#' @param pedantic (logical) Should pedantic mode be turned on? The default is
#'   `FALSE`. Pedantic mode attempts to warn you about potential issues in your
#'   Stan program beyond syntax errors. For details see the [*Pedantic mode*
#'   chapter](https://mc-stan.org/docs/reference-manual/pedantic-mode.html) in
#'   the Stan Reference Manual. **Note:** to do a pedantic check for a model
#'   that is already compiled use the
#'   [`$check_syntax()`][model-method-check_syntax] method instead.
#' @param include_paths (character vector) Paths to directories where Stan
#'   should look for files specified in `#include` directives in the Stan
#'   program.
#' @param cpp_options (list) Any makefile options to be used when compiling the
#'   model (`STAN_THREADS`, `STAN_MPI`, `STAN_OPENCL`, etc.). Anything you would
#'   otherwise write in the `make/local` file.
#' @param stanc_options (list) Any Stan-to-C++ transpiler options to be used
#'   when compiling the model. See the **Examples** section below as well as the
#'   `stanc` chapter of the CmdStan Guide for more details on available options:
#'   https://mc-stan.org/docs/cmdstan-guide/stanc.html.
#' @param force_recompile (logical) Should the model be recompiled even if was
#'   not modified since last compiled. The default is `FALSE`.
#' @param threads Deprecated and will be removed in a future release. Please
#'   turn on threading via `cpp_options = list(stan_threads = TRUE)` instead.
#'
#' @return The `$compile()` method is called for its side effect of creating the
#'   executable and adding its path to the [`CmdStanModel`] object, but it also
#'   returns the [`CmdStanModel`] object invisibly.
#'
#'   After compilation, the `$exe_file()`, `$hpp_file()`, and `$save_hpp_file()`
#'   methods can be used and return file paths.
#'
#' @seealso The [`$check_syntax()`][model-method-check_syntax] method to check
#'   Stan syntax or enable pedantic model without compiling.
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#'
#' # by default compilation happens when cmdstan_model() is called.
#' # to delay compilation until calling the $compile() method set compile=FALSE
#' mod <- cmdstan_model(file, compile = FALSE)
#' mod$compile()
#' mod$exe_file()
#'
#' # turn on threading support (for using functions that support within-chain parallelization)
#' mod$compile(force_recompile = TRUE, cpp_options = list(stan_threads = TRUE))
#' mod$exe_file()
#'
#' # turn on pedantic mode (new in Stan v2.24)
#' file_pedantic <- write_stan_file("
#' parameters {
#'   real sigma;  // pedantic mode will warn about missing <lower=0>
#' }
#' model {
#'   sigma ~ exponential(1);
#' }
#' ")
#' mod <- cmdstan_model(file_pedantic, pedantic = TRUE)
#'
#' }
#'
compile <- function(quiet = TRUE,
                    dir = NULL,
                    pedantic = FALSE,
                    include_paths = NULL,
                    cpp_options = list(),
                    stanc_options = list(),
                    force_recompile = FALSE,
                    #deprecated
                    threads = FALSE) {
  if (length(cpp_options) == 0 && !is.null(private$precompile_cpp_options_)) {
    cpp_options <- private$precompile_cpp_options_
  }
  if (length(stanc_options) == 0 && !is.null(private$precompile_stanc_options_)) {
    stanc_options <- private$precompile_stanc_options_
  }
  check_stanc_options(stanc_options)
  if (is.null(include_paths) && !is.null(private$precompile_include_paths_)) {
    include_paths <- private$precompile_include_paths_
  }
  if (is.null(dir) && !is.null(private$dir_)) {
    dir <- absolute_path(private$dir_)
  } else if (!is.null(dir)) {
    dir <- absolute_path(dir)
  }
  if (!is.null(dir)) {
    dir <- repair_path(dir)
    checkmate::assert_directory_exists(dir, access = "rw")
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

  if (is.null(dir)) {
    exe_base <- self$stan_file()
  } else {
    exe_base <- file.path(dir, basename(self$stan_file()))
  }
  exe <- cmdstan_ext(paste0(strip_ext(exe_base), exe_suffix))
  if (dir.exists(exe)) {
    stop("There is a subfolder matching the model name in the same folder as the model! Please remove or rename the subfolder and try again.", call. = FALSE)
  }
  model_name <- sub(" ", "_", paste0(strip_ext(basename(self$stan_file())), "_model"))

  # compile if:
  # - the user forced compilation,
  # - the executable does not exist
  # - the stan model was changed since last compilation
  if (!file.exists(exe)) {
    force_recompile <- TRUE
  } else if (file.exists(self$stan_file())
             && file.mtime(exe) < file.mtime(self$stan_file())) {
    force_recompile <- TRUE
  }

  if (!force_recompile) {
    if (interactive()) {
      message("Model executable is up to date!")
    }
    private$cpp_options_ <- cpp_options
    private$precompile_cpp_options_ <- NULL
    private$precompile_stanc_options_ <- NULL
    private$precompile_include_paths_ <- NULL
    self$exe_file(exe)
    return(invisible(self))
  } else {
    if (interactive()) {
      message("Compiling Stan program...")
    }
  }

  temp_stan_file <- tempfile(pattern = "model-", fileext = ".stan")
  file.copy(self$stan_file(), temp_stan_file, overwrite = TRUE)
  temp_file_no_ext <- strip_ext(temp_stan_file)
  tmp_exe <- cmdstan_ext(temp_file_no_ext) # adds .exe on Windows
  if(os_is_windows()) {
    tmp_exe <- utils::shortPathName(tmp_exe)
  }
  private$hpp_file_ <- paste0(temp_file_no_ext, ".hpp")

  # add path to the TBB library to the PATH variable to avoid copying the dll file
  if (cmdstan_version() >= "2.21" && os_is_windows()) {
    path_to_TBB <- file.path(cmdstan_path(), "stan", "lib", "stan_math", "lib", "tbb")
    current_path <- Sys.getenv("PATH")
    if (!grepl(path_to_TBB, current_path, perl = TRUE)) {
      Sys.setenv(PATH = paste0(path_to_TBB, ";", Sys.getenv("PATH")))
    }
  }

  stancflags_val <- ""
  if (!is.null(include_paths)) {
    checkmate::assert_directory_exists(include_paths, access = "r")
    include_paths <- absolute_path(include_paths)
    include_paths <- paste0(include_paths, collapse = ",")
    if (cmdstan_version() >= "2.24") {
      include_paths_flag <- " --include-paths="
    } else {
      include_paths_flag <- " --include_paths="
    }
    stancflags_val <- paste0(stancflags_val, include_paths_flag, include_paths, " ")
  }

  if (pedantic) {
    stanc_options[["warn-pedantic"]] <- TRUE
  }

  if (!is.null(cpp_options$stan_opencl)) {
    stanc_options[["use-opencl"]] <- TRUE
  }
  if (is.null(stanc_options[["name"]])) {
    stanc_options[["name"]] <- model_name
  }
  stanc_built_options <- c()
  for (i in seq_len(length(stanc_options))) {
    option_name <- names(stanc_options)[i]
    if (isTRUE(as.logical(stanc_options[[i]]))) {
      stanc_built_options <- c(stanc_built_options, paste0("--", option_name))
    } else if (is.null(option_name) || !nzchar(option_name)) {
      stanc_built_options <- c(stanc_built_options, paste0("--", stanc_options[[i]]))
    } else {
      stanc_built_options <- c(stanc_built_options, paste0("--", option_name, "=", "'", stanc_options[[i]], "'"))
    }
  }
  stancflags_val <- paste0("STANCFLAGS += ", stancflags_val, paste0(stanc_built_options, collapse = " "))
  run_log <- processx::run(
    command = make_cmd(),
    args = c(tmp_exe,
             cpp_options_to_compile_flags(cpp_options),
             stancflags_val),
    wd = cmdstan_path(),
    echo = !quiet || is_verbose_mode(),
    echo_cmd = is_verbose_mode(),
    spinner = quiet && interactive(),
    stderr_line_callback = function(x,p) {
      if (!startsWith(x, paste0(make_cmd(), ": *** No rule to make target"))) {
        message(x)
      }
      if (grepl("PCH file", x)) {
        warning(
          "CmdStan's precompiled header (PCH) files may need to be rebuilt.\n",
          "If your model failed to compile please run rebuild_cmdstan().\n",
          "If the issue persists please open a bug report.",
          call. = FALSE
        )
      }
    },
    error_on_status = FALSE
  )
  if (run_log$status != 0) {
    stop("An error occured during compilation! See the message above for more information.",
         call. = FALSE)
  }

  file.copy(tmp_exe, exe, overwrite = TRUE)
  private$exe_file_ <- exe
  private$cpp_options_ <- cpp_options
  private$precompile_cpp_options_ <- NULL
  private$precompile_stanc_options_ <- NULL
  private$precompile_include_paths_ <- NULL
  invisible(self)
}
CmdStanModel$set("public", name = "compile", value = compile)

#' Check syntax of a Stan program
#'
#' @name model-method-check_syntax
#' @aliases check_syntax
#' @family CmdStanModel methods
#'
#' @description The `$check_syntax()` method of a [`CmdStanModel`] object
#'   checks the Stan program for syntax errors and returns `TRUE` (invisibly) if
#'   parsing succeeds. If invalid syntax in found an error is thrown.
#'
#' @param pedantic (logical) Should pedantic mode be turned on? The default is
#'   `FALSE`. Pedantic mode attempts to warn you about potential issues in your
#'   Stan program beyond syntax errors. For details see the [*Pedantic mode*
#'   chapter](https://mc-stan.org/docs/reference-manual/pedantic-mode.html) in
#'   the Stan Reference Manual.
#' @param include_paths (character vector) Paths to directories where Stan
#'   should look for files specified in `#include` directives in the Stan
#'   program.
#' @param stanc_options (list) Any other Stan-to-C++ transpiler options to be
#'   used when compiling the model. See the documentation for the
#'   [`$compile()`][model-method-compile] method for details.
#' @param quiet (logical) Should informational messages be suppressed? The
#'   default is `FALSE`, which will print a message if the Stan program is valid
#'   or the compiler error message if there are syntax errors. If `TRUE`, only
#'   the error message will be printed.
#'
#' @return The `$check_syntax()` method returns `TRUE` (invisibly) if the model
#'   is valid.
#'
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' file <- write_stan_file("
#' data {
#'   int N;
#'   int y[N];
#' }
#' parameters {
#'   // should have <lower=0> but omitting to demonstrate pedantic mode
#'   real lambda;
#' }
#' model {
#'   y ~ poisson(lambda);
#' }
#' ")
#' mod <- cmdstan_model(file, compile = FALSE)
#'
#' # the program is syntactically correct, however...
#' mod$check_syntax()
#'
#' # pedantic mode will warn that lambda should be constrained to be positive
#' # and that lambda has no prior distribution
#' mod$check_syntax(pedantic = TRUE)
#' }
#'
check_syntax <- function(pedantic = FALSE,
                         include_paths = NULL,
                         stanc_options = list(),
                         quiet = FALSE) {
  if (length(stanc_options) == 0 && !is.null(private$precompile_stanc_options_)) {
    stanc_options <- private$precompile_stanc_options_
  }
  if (is.null(include_paths) && !is.null(private$precompile_include_paths_)) {
    include_paths <- private$precompile_include_paths_
  }

  model_name <- sub(" ", "_", paste0(strip_ext(basename(self$stan_file())), "_model"))

  temp_hpp_file <- tempfile(pattern = "model-", fileext = ".hpp")
  stanc_options[["o"]] <- temp_hpp_file

  if (pedantic) {
    stanc_options[["warn-pedantic"]] <- TRUE
  }

  stancflags_val <- NULL
  if (!is.null(include_paths)) {
    checkmate::assert_directory_exists(include_paths, access = "r")
    include_paths <- absolute_path(include_paths)
    include_paths <- paste0(include_paths, collapse = ",")
    if (cmdstan_version() >= "2.24") {
      include_paths_flag <- " --include-paths="
    } else {
      include_paths_flag <- " --include_paths="
    }
    stancflags_val <- trimws(paste0(include_paths_flag, include_paths, " "))
  }

  if (is.null(stanc_options[["name"]])) {
    stanc_options[["name"]] <- model_name
  }
  stanc_built_options = c()
  for (i in seq_len(length(stanc_options))) {
    option_name <- names(stanc_options)[i]
    if (isTRUE(as.logical(stanc_options[[i]]))) {
      stanc_built_options <- c(stanc_built_options, paste0("--", option_name))
    } else if (is.null(option_name) || !nzchar(option_name)) {
      stanc_built_options <- c(stanc_built_options, paste0("--", stanc_options[[i]]))
    } else {
      stanc_built_options <- c(stanc_built_options, paste0("--", option_name, "=", stanc_options[[i]]))
    }
  }

  run_log <- processx::run(
    command = stanc_cmd(),
    args = c(self$stan_file(), stanc_built_options, stancflags_val),
    wd = cmdstan_path(),
    echo = is_verbose_mode(),
    echo_cmd = is_verbose_mode(),
    spinner = quiet && interactive(),
    stdout_line_callback = function(x,p) {
      if (!quiet) cat(x)
    },
    stderr_line_callback = function(x,p) {
      message(x)
    },
    error_on_status = FALSE
  )
  if (run_log$status != 0) {
    stop("Syntax error found! See the message above for more information.",
         call. = FALSE)
  }
  if (!quiet) {
    message("Stan program is syntactically correct");
  }
  invisible(TRUE)
}
CmdStanModel$set("public", name = "check_syntax", value = check_syntax)

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
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the
#'   [CmdStan User’s Guide](https://mc-stan.org/docs/cmdstan-guide/)
#'   for more details.
#'
#' @template model-common-args
#' @template model-sample-args
#' @param cores,num_cores,num_chains,num_warmup,num_samples,save_extra_diagnostics,max_depth,stepsize
#'   Deprecated and will be removed in a future release.
#'
#' @return A [`CmdStanMCMC`] object.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
sample <- function(data = NULL,
                   seed = NULL,
                   refresh = NULL,
                   init = NULL,
                   save_latent_dynamics = FALSE,
                   output_dir = NULL,
                   output_basename = NULL,
                   sig_figs = NULL,
                   chains = 4,
                   parallel_chains = getOption("mc.cores", 1),
                   chain_ids = seq_len(chains),
                   threads_per_chain = NULL,
                   opencl_ids = NULL,
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
                   show_messages = TRUE,
                   # deprecated
                   cores = NULL,
                   num_cores = NULL,
                   num_chains = NULL,
                   num_warmup = NULL,
                   num_samples = NULL,
                   save_extra_diagnostics = NULL,
                   max_depth = NULL,
                   stepsize = NULL) {
  # temporary deprecation warnings
  if (!is.null(cores)) {
    warning("'cores' is deprecated. Please use 'parallel_chains' instead.")
    parallel_chains <- cores
  }
  if (!is.null(num_cores)) {
    warning("'num_cores' is deprecated. Please use 'parallel_chains' instead.")
    parallel_chains <- num_cores
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

  if (fixed_param) {
    chains <- 1
    parallel_chains <- 1
    save_warmup <- FALSE
  }

  checkmate::assert_integerish(chains, lower = 1, len = 1)
  checkmate::assert_integerish(parallel_chains, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(threads_per_chain, lower = 1, len = 1, null.ok = TRUE)
  checkmate::assert_integerish(chain_ids, lower = 1, len = chains, unique = TRUE, null.ok = FALSE)
  if (is.null(self$cpp_options()[["stan_threads"]])) {
    if (!is.null(threads_per_chain)) {
      warning("'threads_per_chain' is set but the model was not compiled with ",
              "'cpp_options = list(stan_threads = TRUE)' so 'threads_per_chain' will have no effect!",
              call. = FALSE)
      threads_per_chain <- NULL
    }
  } else {
    if (is.null(threads_per_chain)) {
      stop("The model was compiled with 'cpp_options = list(stan_threads = TRUE)' ",
           "but 'threads_per_chain' was not set!",
           call. = FALSE)
    }
  }
  check_opencl(self$cpp_options(), opencl_ids)
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
    proc_ids = chain_ids,
    data_file = process_data(data),
    save_latent_dynamics = save_latent_dynamics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    validate_csv = validate_csv,
    opencl_ids = opencl_ids
  )
  cmdstan_procs <- CmdStanMCMCProcs$new(
    num_procs = chains,
    parallel_procs = parallel_chains,
    threads_per_proc = threads_per_chain,
    show_stderr_messages = show_messages
  )
  runset <- CmdStanRun$new(args = cmdstan_args, procs = cmdstan_procs)
  runset$run_cmdstan()
  CmdStanMCMC$new(runset)
}
CmdStanModel$set("public", name = "sample", value = sample)

#' Run Stan's MCMC algorithms with MPI
#'
#' @name model-method-sample_mpi
#' @aliases sample_mpi
#' @family CmdStanModel methods
#'
#' @description The `$sample_mpi()` method of a [`CmdStanModel`] object is
#'   identical to the `$sample()` method but with support for
#'   [MPI](https://mc-stan.org/math/mpi.html). The target audience for MPI are
#'   those with large computer clusters. For other users, the
#'   [`$sample()`][model-method-sample] method provides both parallelization of
#'   chains and threading support for within-chain parallelization.
#'
#'   In order to use MPI with Stan, an MPI implementation must be
#'   installed. For Unix systems the most commonly used implementations are
#'   MPICH and OpenMPI. The implementations provide an MPI C++ compiler wrapper
#'   (for example mpicxx), which is required to compile the model.
#'
#'   An example of compiling with MPI:
#'   ```
#'   mpi_options = list(STAN_MPI=TRUE, CXX="mpicxx", TBB_CXX_TYPE="gcc")
#'   mod = cmdstan_model("model.stan", cpp_options = mpi_options)
#'   ```
#'   The C++ options that must be supplied to the
#'   [compile][model-method-compile] call are:
#'   - `STAN_MPI`: Enables the use of MPI with Stan if `TRUE`.
#'   - `CXX`: The name of the MPI C++ compiler wrapper. Typically `"mpicxx"`.
#'   - `TBB_CXX_TYPE`: The C++ compiler the MPI wrapper wraps. Typically `"gcc"`
#'   on Linux and `"clang"` on macOS.
#'
#'   In the call to the `$sample_mpi()` method it is also possible to provide
#'   the name of the MPI launcher (`mpi_cmd`, defaulting to `"mpiexec"`) and any
#'   other MPI launch arguments (`mpi_args`). In most cases, it is enough to
#'   only define the number of processes. To use `n_procs` processes specify
#'   `mpi_args = list("n" = n_procs)`.
#'
#' @inheritParams model-method-sample
#' @param mpi_cmd (character vector) The MPI launcher used for launching MPI
#'   processes. The default launcher is `"mpiexec"`.
#' @param mpi_args (list) A list of arguments to use when launching MPI
#'   processes. For example, `mpi_args = list("n" = 4)` launches the executable
#'   as `mpiexec -n 4 model_executable`, followed by CmdStan arguments for the
#'   model executable.
#'
#' @return A [`CmdStanMCMC`] object.
#'
#' @template seealso-docs
#' @seealso The Stan Math Library's MPI documentation
#'   ([mc-stan.org/math/mpi](https://mc-stan.org/math/mpi.html)) for more
#'   details on MPI support in Stan.
#'
#' @examples
#' \dontrun{
#' # mpi_options <- list(STAN_MPI=TRUE, CXX="mpicxx", TBB_CXX_TYPE="gcc")
#' # mod <- cmdstan_model("model.stan", cpp_options = mpi_options)
#' # fit <- mod$sample_mpi(..., mpi_args = list("n" = 4))
#' }
#'
sample_mpi <- function(data = NULL,
                       mpi_cmd = "mpiexec",
                       mpi_args = NULL,
                       seed = NULL,
                       refresh = NULL,
                       init = NULL,
                       save_latent_dynamics = FALSE,
                       output_dir = NULL,
                       output_basename = NULL,
                       chains = 1,
                       chain_ids = seq_len(chains),
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
                       sig_figs = NULL,
                       validate_csv = TRUE,
                       show_messages = TRUE) {
  if (fixed_param) {
    chains <- 1
    save_warmup <- FALSE
  }

  checkmate::assert_integerish(chains, lower = 1, len = 1)
  checkmate::assert_integerish(chain_ids, lower = 1, len = chains, unique = TRUE, null.ok = FALSE)
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
    proc_ids = chain_ids,
    data_file = process_data(data),
    save_latent_dynamics = save_latent_dynamics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    output_basename = output_basename,
    validate_csv = validate_csv,
    sig_figs = sig_figs
  )
  cmdstan_procs <- CmdStanMCMCProcs$new(
    num_procs = chains,
    parallel_procs = 1,
    show_stderr_messages = show_messages
  )
  runset <- CmdStanRun$new(args = cmdstan_args, procs = cmdstan_procs)
  runset$run_cmdstan_mpi(mpi_cmd, mpi_args)
  CmdStanMCMC$new(runset)
}
CmdStanModel$set("public", name = "sample_mpi", value = sample_mpi)

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
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the
#'   [CmdStan User’s Guide](https://mc-stan.org/docs/cmdstan-guide/)
#'   for more details.
#'
#' @details CmdStan can find the posterior mode (assuming there is one). If the
#'   posterior is not convex, there is no guarantee Stan will be able to find
#'   the global mode as opposed to a local optimum of log probability. For
#'   optimization, the mode is calculated without the Jacobian adjustment for
#'   constrained variables, which shifts the mode due to the change of
#'   variables. Thus modes correspond to modes of the model as written.
#'
#'   -- [*CmdStan User's Guide*](https://mc-stan.org/docs/cmdstan-guide/)
#'
#' @template model-common-args
#' @param threads (positive integer) If the model was
#'   [compiled][model-method-compile] with threading support, the number of
#'   threads to use in parallelized sections (e.g., when
#'   using the Stan functions `reduce_sum()` or `map_rect()`).
#' @param iter (positive integer) The maximum number of iterations.
#' @param algorithm (string) The optimization algorithm. One of `"lbfgs"`,
#'   `"bfgs"`, or `"newton"`. The control parameters below are only available
#'   for `"lbfgs"` and `"bfgs`. For their default values and more details see
#'   the CmdStan User's Guide. The default values can also be obtained by
#'   running `cmdstanr_example(method="optimize")$metadata()`.
#' @param init_alpha (positive real) The initial step size parameter.
#' @param tol_obj (positive real) Convergence tolerance on changes in objective function value.
#' @param tol_rel_obj (positive real) Convergence tolerance on relative changes in objective function value.
#' @param tol_grad (positive real) Convergence tolerance on the norm of the gradient.
#' @param tol_rel_grad (positive real) Convergence tolerance on the relative norm of the gradient.
#' @param tol_param (positive real) Convergence tolerance on changes in parameter value.
#' @param history_size (positive integer) The size of the history used when
#'   approximating the Hessian. Only available for L-BFGS.
#'
#' @return A [`CmdStanMLE`] object.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
optimize <- function(data = NULL,
                     seed = NULL,
                     refresh = NULL,
                     init = NULL,
                     save_latent_dynamics = FALSE,
                     output_dir = NULL,
                     output_basename = NULL,
                     sig_figs = NULL,
                     threads = NULL,
                     opencl_ids = NULL,
                     algorithm = NULL,
                     init_alpha = NULL,
                     iter = NULL,
                     tol_obj = NULL,
                     tol_rel_obj = NULL,
                     tol_grad = NULL,
                     tol_rel_grad = NULL,
                     tol_param = NULL,
                     history_size = NULL) {
  checkmate::assert_integerish(threads, lower = 1, len = 1, null.ok = TRUE)
  if (is.null(self$cpp_options()[["stan_threads"]])) {
    if (!is.null(threads)) {
      warning("'threads' is set but the model was not compiled with ",
              "'cpp_options = list(stan_threads = TRUE)' so 'threads' will have no effect!",
              call. = FALSE)
      threads <- NULL
    }
  } else {
    if (is.null(threads)) {
      stop("The model was compiled with 'cpp_options = list(stan_threads = TRUE)' ",
           "but 'threads' was not set!",
           call. = FALSE)
    }
  }
  check_opencl(self$cpp_options(), opencl_ids)
  optimize_args <- OptimizeArgs$new(
    algorithm = algorithm,
    init_alpha = init_alpha,
    iter = iter,
    tol_obj = tol_obj,
    tol_rel_obj = tol_rel_obj,
    tol_grad = tol_grad,
    tol_rel_grad = tol_rel_grad,
    tol_param = tol_param,
    history_size = history_size
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
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = opencl_ids
  )

  cmdstan_procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stdout_messages = (is.null(refresh) || refresh != 0),
    threads_per_proc = threads
  )
  runset <- CmdStanRun$new(args = cmdstan_args, procs = cmdstan_procs)
  runset$run_cmdstan()
  CmdStanMLE$new(runset)
}
CmdStanModel$set("public", name = "optimize", value = optimize)


#' Run Stan's variational approximation algorithms
#'
#' @name model-method-variational
#' @aliases variational
#' @family CmdStanModel methods
#'
#' @description The `$variational()` method of a [`CmdStanModel`] object runs
#'   Stan's variational Bayes (ADVI) algorithms.
#'
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the
#'   [CmdStan User’s Guide](https://mc-stan.org/docs/cmdstan-guide/)
#'   for more details.
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
#' @template model-common-args
#' @param threads (positive integer) If the model was
#'   [compiled][model-method-compile] with threading support, the number of
#'   threads to use in parallelized sections (e.g., when using the Stan
#'   functions `reduce_sum()` or `map_rect()`).
#' @param algorithm (string) The algorithm. Either `"meanfield"` or
#'   `"fullrank"`.
#' @param iter (positive integer) The _maximum_ number of iterations.
#' @param grad_samples (positive integer) The number of samples for Monte Carlo
#'   estimate of gradients.
#' @param elbo_samples (positive integer) The number of samples for Monte Carlo
#'   estimate of ELBO (objective function).
#' @param eta (positive real) The step size weighting parameter for adaptive
#'   step size sequence.
#' @param adapt_engaged (logical) Do warmup adaptation?
#' @param adapt_iter (positive integer) The _maximum_ number of adaptation
#'   iterations.
#' @param tol_rel_obj (positive real) Convergence tolerance on the relative norm
#'   of the objective.
#' @param eval_elbo (positive integer) Evaluate ELBO every Nth iteration.
#' @param output_samples (positive integer) Number of approximate posterior
#'   samples to draw and save.
#'
#' @return A [`CmdStanVB`] object.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
variational <- function(data = NULL,
                        seed = NULL,
                        refresh = NULL,
                        init = NULL,
                        save_latent_dynamics = FALSE,
                        output_dir = NULL,
                        output_basename = NULL,
                        sig_figs = NULL,
                        threads = NULL,
                        opencl_ids = NULL,
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
  checkmate::assert_integerish(threads, lower = 1, len = 1, null.ok = TRUE)
  if (is.null(self$cpp_options()[["stan_threads"]])) {
    if (!is.null(threads)) {
      warning("'threads' is set but the model was not compiled with ",
              "'cpp_options = list(stan_threads = TRUE)' so 'threads' will have no effect!",
              call. = FALSE)
      threads <- NULL
    }
  } else {
    if (is.null(threads)) {
      stop("The model was compiled with 'cpp_options = list(stan_threads = TRUE)' ",
           "but 'threads' was not set!",
           call. = FALSE)
    }
  }
  check_opencl(self$cpp_options(), opencl_ids)
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
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = opencl_ids
  )

  cmdstan_procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stdout_messages = (is.null(refresh) || refresh != 0),
    threads_per_proc = threads
  )
  runset <- CmdStanRun$new(args = cmdstan_args, procs = cmdstan_procs)
  runset$run_cmdstan()
  CmdStanVB$new(runset)
}
CmdStanModel$set("public", name = "variational", value = variational)

#' Run Stan's standalone generated quantities method
#'
#' @name model-method-generate-quantities
#' @aliases generate_quantities
#' @family CmdStanModel methods
#'
#' @description The `$generate_quantities()` method of a [`CmdStanModel`] object
#'   runs Stan's standalone generated quantities to obtain generated quantities
#'   based on previously fitted parameters.
#'
#' @inheritParams model-method-sample
#' @param fitted_params (multiple options) The parameter draws to use. One of
#'   the following:
#'  * A [CmdStanMCMC] or [CmdStanVB] fitted model object.
#'  * A [posterior::draws_array] (for MCMC) or [posterior::draws_matrix] (for
#'  VB) object returned by CmdStanR's [`$draws()`][fit-method-draws] method.
#'  * A character vector of paths to CmdStan CSV output files.
#'
#' @return A [`CmdStanGQ`] object.
#'
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' # first fit a model using MCMC
#' mcmc_program <- write_stan_file(
#'   "data {
#'     int<lower=0> N;
#'     int<lower=0,upper=1> y[N];
#'   }
#'   parameters {
#'     real<lower=0,upper=1> theta;
#'   }
#'   model {
#'     y ~ bernoulli(theta);
#'   }"
#' )
#' mod_mcmc <- cmdstan_model(mcmc_program)
#'
#' data <- list(N = 10, y = c(1,1,0,0,0,1,0,1,0,0))
#' fit_mcmc <- mod_mcmc$sample(data = data, seed = 123, refresh = 0)
#'
#' # stan program for standalone generated quantities
#' # (could keep model block, but not necessary so removing it)
#' gq_program <- write_stan_file(
#'   "data {
#'     int<lower=0> N;
#'     int<lower=0,upper=1> y[N];
#'   }
#'   parameters {
#'     real<lower=0,upper=1> theta;
#'   }
#'   generated quantities {
#'     int y_rep[N] = bernoulli_rng(rep_vector(theta, N));
#'   }"
#' )
#'
#' mod_gq <- cmdstan_model(gq_program)
#' fit_gq <- mod_gq$generate_quantities(fit_mcmc, data = data, seed = 123)
#' str(fit_gq$draws())
#'
#' library(posterior)
#' as_draws_df(fit_gq$draws())
#' }
#'
generate_quantities <- function(fitted_params,
                                data = NULL,
                                seed = NULL,
                                output_dir = NULL,
                                output_basename = NULL,
                                sig_figs = NULL,
                                parallel_chains = getOption("mc.cores", 1),
                                threads_per_chain = NULL,
                                opencl_ids = NULL) {
  checkmate::assert_integerish(parallel_chains, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(threads_per_chain, lower = 1, len = 1, null.ok = TRUE)
  if (is.null(self$cpp_options()[["stan_threads"]])) {
    if (!is.null(threads_per_chain)) {
      warning("'threads_per_chain' is set but the model was not compiled with ",
              "'cpp_options = list(stan_threads = TRUE)' so 'threads_per_chain' will have no effect!",
              call. = FALSE)
      threads_per_chain <- NULL
    }
  } else {
    if (is.null(threads_per_chain)) {
      stop("The model was compiled with 'cpp_options = list(stan_threads = TRUE)' ",
           "but 'threads_per_chain' was not set!",
           call. = FALSE)
    }
  }
  check_opencl(self$cpp_options(), opencl_ids)
  fitted_params <- process_fitted_params(fitted_params)
  chains <- length(fitted_params)
  generate_quantities_args <- GenerateQuantitiesArgs$new(
    fitted_params = fitted_params
  )
  cmdstan_args <- CmdStanArgs$new(
    method_args = generate_quantities_args,
    model_name = strip_ext(basename(self$exe_file())),
    exe_file = self$exe_file(),
    proc_ids = seq_len(chains),
    data_file = process_data(data),
    seed = seed,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = opencl_ids
  )
  cmdstan_procs <- CmdStanGQProcs$new(
    num_procs = chains,
    parallel_procs = parallel_chains,
    threads_per_proc = threads_per_chain
  )
  runset <- CmdStanRun$new(args = cmdstan_args, procs = cmdstan_procs)
  runset$run_cmdstan()
  CmdStanGQ$new(runset)
}
CmdStanModel$set("public", name = "generate_quantities", value = generate_quantities)


check_opencl <- function(cpp_options, opencl_ids) {
  if (is.null(cpp_options[["stan_opencl"]])
      && !is.null(opencl_ids)) {
     stop("'opencl_ids' is set but the model was not compiled with for use with OpenCL.",
           "\nRecompile the model with the 'cpp_options = list(stan_opencl = TRUE)'",
           call. = FALSE)   
  }
}