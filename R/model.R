#' Create a new CmdStanModel object
#'
#' @description \if{html}{\figure{logo.png}{options: width="25px"}}
#'   Create a new [`CmdStanModel`] object from a file containing a Stan program
#'   or from an existing Stan executable. The [`CmdStanModel`] object stores the
#'   path to a Stan program and compiled executable (once created), and provides
#'   methods for fitting the model using Stan's algorithms.
#'
#'   See the `compile` and `...` arguments for control over whether and how
#'   compilation happens.
#'
#' @export
#' @param stan_file (string) The path to a `.stan` file containing a Stan
#'   program. The helper function [write_stan_file()] is provided for cases when
#'   it is more convenient to specify the Stan program as a string. If
#'   `stan_file` is not specified then `exe_file` must be specified.
#' @param exe_file (string) The path to an existing Stan model executable. Can
#'   be provided instead of or in addition to `stan_file` (if `stan_file` is
#'   omitted some `CmdStanModel` methods like `$code()` and `$print()` will not
#'   work). This argument can only be used with CmdStan 2.27+.
#' @param compile (logical) Do compilation? The default is `TRUE`. If `FALSE`
#'   compilation can be done later via the [`$compile()`][model-method-compile]
#'   method.
#' @param ... Optionally, additional arguments to pass to the
#'   [`$compile()`][model-method-compile] method if `compile=TRUE`. These
#'   options include specifying the directory for saving the executable, turning
#'   on pedantic mode, specifying include paths, configuring C++ options, and
#'   more. See [`$compile()`][model-method-compile] for details.
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
cmdstan_model <- function(stan_file = NULL, exe_file = NULL, compile = TRUE, ...) {
  if (cmdstan_version() < "2.27.0" && !is.null(exe_file)) {
    stop("'exe_file' argument is only supported with CmdStan 2.27 and newer.", call. = FALSE)
  }
  if (is.null(exe_file) && is.null(stan_file)) {
    stop("Unable to create a `CmdStanModel` object. Both 'stan_file' and 'exe_file' are undefined.", call. = FALSE)
  }
  CmdStanModel$new(stan_file = stan_file, exe_file = exe_file, compile = compile, ...)
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
#'  `$code()` | Return Stan program as a character vector. |
#'  `$print()`|  Print readable version of Stan program. |
#'  [`$check_syntax()`][model-method-check_syntax]  |  Check Stan syntax without having to compile. |
#'  [`$format()`][model-method-format]  |  Format and canonicalize the Stan model code. |
#'
#'  ## Compilation
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$compile()`][model-method-compile]  |  Compile Stan program. |
#'  [`$exe_file()`][model-method-compile] |  Return the file path to the compiled executable. |
#'  [`$hpp_file()`][model-method-compile] |  Return the file path to the `.hpp` file containing the generated C++ code. |
#'  [`$save_hpp_file()`][model-method-compile] |  Save the `.hpp` file containing the generated C++ code. |
#'  [`$expose_functions()`][model-method-expose_functions] |  Expose Stan functions for use in R. |
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
    stan_code_ = character(),
    model_name_ = character(),
    exe_file_ = character(),
    hpp_file_ = character(),
    model_methods_env_ = NULL,
    dir_ = NULL,
    cpp_options_ = list(),
    stanc_options_ = list(),
    include_paths_ = NULL,
    using_user_header_ = FALSE,
    precompile_cpp_options_ = NULL,
    precompile_stanc_options_ = NULL,
    precompile_include_paths_ = NULL,
    variables_ = NULL
  ),
  public = list(
    functions = NULL,
    initialize = function(stan_file = NULL, exe_file = NULL, compile, ...) {
      args <- list(...)
      private$dir_ <- args$dir
      self$functions <- new.env()
      self$functions$compiled <- FALSE
      if (!is.null(stan_file)) {
        assert_file_exists(stan_file, access = "r", extension = "stan")
        checkmate::assert_flag(compile)
        private$stan_file_ <- absolute_path(stan_file)
        private$stan_code_ <- readLines(stan_file)
        private$model_name_ <- sub(" ", "_", strip_ext(basename(private$stan_file_)))
        private$precompile_cpp_options_ <- args$cpp_options %||% list()
        private$precompile_stanc_options_ <- assert_valid_stanc_options(args$stanc_options) %||% list()
        if (!is.null(args$user_header) || !is.null(args$cpp_options[["USER_HEADER"]]) ||
            !is.null(args$cpp_options[["user_header"]])) {
          private$using_user_header_ <- TRUE
        }
        if (is.null(args$include_paths) && any(grepl("#include" , private$stan_code_))) {
          private$precompile_include_paths_ <- dirname(stan_file)
        } else {
          private$precompile_include_paths_ <- args$include_paths
        }
      }
      if (!is.null(exe_file)) {
        ext <- if (os_is_windows() && !os_is_wsl()) "exe" else ""
        private$exe_file_ <- repair_path(absolute_path(exe_file))
        if (is.null(stan_file)) {
          assert_file_exists(private$exe_file_, access = "r", extension = ext)
          private$model_name_ <- sub(" ", "_", strip_ext(basename(private$exe_file_)))
        }
      }
      if (!is.null(stan_file) && compile) {
        self$compile(...)
      }
      if (length(self$exe_file()) > 0 && file.exists(self$exe_file())) {
        cpp_options <- model_compile_info(self$exe_file())
        for (cpp_option_name in names(cpp_options)) {
          if (cpp_option_name != "stan_version" &&
              (!is.logical(cpp_options[[cpp_option_name]]) || isTRUE(cpp_options[[cpp_option_name]]))) {
            private$cpp_options_[[cpp_option_name]] <- cpp_options[[cpp_option_name]]
          }
        }
      }
      invisible(self)
    },
    include_paths = function() {
      if (length(self$exe_file()) > 0 && file.exists(self$exe_file())) {
        return(private$include_paths_)
      } else {
        return(private$precompile_include_paths_)
      }
    },
    code = function() {
      if (length(private$stan_code_) == 0) {
        warning("'$code()' will return NULL because the 'CmdStanModel' was not created with a Stan file.", call. = FALSE)
        return(NULL)
      }
      private$stan_code_
    },
    print = function() {
      if (length(private$stan_code_) == 0) {
        stop("'$print()' cannot be used because the 'CmdStanModel' was not created with a Stan file.", call. = FALSE)
      }
      cat(self$code(), sep = "\n")
      invisible(self)
    },
    stan_file = function() {
      private$stan_file_
    },
    has_stan_file = function() {
      length(self$stan_file()) > 0
    },
    model_name = function() {
      private$model_name_
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
      assert_dir_exists(dir, access = "r")
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
#'   without compiling it or for a model that is already compiled the
#'   [`$check_syntax()`][model-method-check_syntax] method can be used instead.
#' @param include_paths (character vector) Paths to directories where Stan
#'   should look for files specified in `#include` directives in the Stan
#'   program.
#' @param user_header (string) The path to a C++ file (with a .hpp extension)
#'   to compile with the Stan model.
#' @param cpp_options (list) Any makefile options to be used when compiling the
#'   model (`STAN_THREADS`, `STAN_MPI`, `STAN_OPENCL`, etc.). Anything you would
#'   otherwise write in the `make/local` file. For an example of using threading
#'   see the Stan case study
#'   [Reduce Sum: A Minimal Example](https://mc-stan.org/users/documentation/case-studies/reduce_sum_tutorial.html).
#' @param stanc_options (list) Any Stan-to-C++ transpiler options to be used
#'   when compiling the model. See the **Examples** section below as well as the
#'   `stanc` chapter of the CmdStan Guide for more details on available options:
#'   https://mc-stan.org/docs/cmdstan-guide/stanc.html.
#' @param force_recompile (logical) Should the model be recompiled even if was
#'   not modified since last compiled. The default is `FALSE`. Can also be set
#'   via a global `cmdstanr_force_recompile` option.
#' @param compile_model_methods (logical) Compile additional model methods
#'   (`log_prob()`, `grad_log_prob()`, `constrain_variables()`,
#'   `unconstrain_variables()`).
#' @param compile_hessian_method (logical) Should the (experimental) `hessian()` method be
#'   be compiled with the model methods?
#' @param compile_standalone (logical) Should functions in the Stan model be
#'   compiled for use in R? If `TRUE` the functions will be available via the
#'   `functions` field in the compiled model object. This can also be done after
#'   compilation using the
#'   [`$expose_functions()`][model-method-expose_functions] method.
#'
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
                    user_header = NULL,
                    cpp_options = list(),
                    stanc_options = list(),
                    force_recompile = getOption("cmdstanr_force_recompile", default = FALSE),
                    compile_model_methods = FALSE,
                    compile_hessian_method = FALSE,
                    compile_standalone = FALSE,
                    #deprecated
                    threads = FALSE) {
  if (length(self$stan_file()) == 0) {
    stop("'$compile()' cannot be used because the 'CmdStanModel' was not created with a Stan file.", call. = FALSE)
  }
  assert_stan_file_exists(self$stan_file())
  if (length(cpp_options) == 0 && !is.null(private$precompile_cpp_options_)) {
    cpp_options <- private$precompile_cpp_options_
  }
  if (length(stanc_options) == 0 && !is.null(private$precompile_stanc_options_)) {
    stanc_options <- private$precompile_stanc_options_
  }
  stanc_options <- assert_valid_stanc_options(stanc_options)
  if (is.null(include_paths) && !is.null(private$precompile_include_paths_)) {
    include_paths <- private$precompile_include_paths_
  }
  private$include_paths_ <- include_paths
  if (is.null(dir) && !is.null(private$dir_)) {
    dir <- absolute_path(private$dir_)
  } else if (!is.null(dir)) {
    dir <- absolute_path(dir)
  }
  if (!is.null(dir)) {
    dir <- repair_path(dir)
    assert_dir_exists(dir, access = "rw")
    if (length(self$exe_file()) != 0) {
      private$exe_file_ <- file.path(dir, basename(self$exe_file()))
    }
  }

  # temporary deprecation warnings
  if (isTRUE(threads)) {
    warning("'threads' is deprecated. Please use 'cpp_options = list(stan_threads = TRUE)' instead.")
    cpp_options[["stan_threads"]] <- TRUE
  }

  if (length(self$exe_file()) == 0) {
    if (is.null(dir)) {
      exe_base <- self$stan_file()
    } else {
      exe_base <- file.path(dir, basename(self$stan_file()))
    }
    exe <- cmdstan_ext(strip_ext(exe_base))
    if (dir.exists(exe)) {
      stop("There is a subfolder matching the model name in the same folder as the model! Please remove or rename the subfolder and try again.", call. = FALSE)
    }
  } else {
    exe <- self$exe_file()
  }

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

  if (os_is_wsl() && (compile_model_methods || compile_standalone)) {
    warning("Additional model methods and standalone functions are not ",
            "currently available with WSL CmdStan and will not be compiled",
            call. = FALSE)
    compile_model_methods <- FALSE
    compile_standalone <- FALSE
    compile_hessian_method <- FALSE
  }

  temp_stan_file <- tempfile(pattern = "model-", fileext = ".stan")
  file.copy(self$stan_file(), temp_stan_file, overwrite = TRUE)
  temp_file_no_ext <- strip_ext(temp_stan_file)
  tmp_exe <- cmdstan_ext(temp_file_no_ext) # adds .exe on Windows
  if (os_is_windows() && !os_is_wsl()) {
    tmp_exe <- utils::shortPathName(tmp_exe)
  }
  private$hpp_file_ <- paste0(temp_file_no_ext, ".hpp")

  stancflags_val <- include_paths_stanc3_args(include_paths)

  if (pedantic) {
    stanc_options[["warn-pedantic"]] <- TRUE
  }

  if (isTRUE(cpp_options$stan_opencl)) {
    stanc_options[["use-opencl"]] <- TRUE
  }
  if (!is.null(user_header)) {
    cpp_options[["USER_HEADER"]] <- wsl_safe_path(user_header)
    stanc_options[["allow-undefined"]] <- TRUE
  }
  if (!is.null(cpp_options[["USER_HEADER"]])) {
    cpp_options[["USER_HEADER"]] <- wsl_safe_path(absolute_path(cpp_options[["USER_HEADER"]]))
  }
  if (!is.null(cpp_options[["user_header"]])) {
    cpp_options[["user_header"]] <- wsl_safe_path(absolute_path(cpp_options[["user_header"]]))
  }
  if (is.null(stanc_options[["name"]])) {
    stanc_options[["name"]] <- paste0(self$model_name(), "_model")
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
  stancflags_combined <- stanc_built_options
  stancflags_local <- get_cmdstan_flags("STANCFLAGS")
  if (stancflags_local != "") {
    stancflags_combined <- c(stancflags_combined, stancflags_local)
  }
  stancflags_standalone <- c("--standalone-functions", stancflags_val, stancflags_combined)
  self$functions$hpp_code <- get_standalone_hpp(temp_stan_file, stancflags_standalone)
  self$functions$external <- !is.null(user_header)
  if (compile_standalone) {
    expose_stan_functions(self$functions, !quiet)
  }
  stancflags_val <- paste0("STANCFLAGS += ", stancflags_val, paste0(" ", stancflags_combined, collapse = " "))
  withr::with_path(
    c(
      toolchain_PATH_env_var(),
      tbb_path()
    ),
    run_log <- wsl_compatible_run(
      command = make_cmd(),
      args = c(wsl_safe_path(tmp_exe),
              cpp_options_to_compile_flags(cpp_options),
              stancflags_val),
      wd = cmdstan_path(),
      echo = !quiet || is_verbose_mode(),
      echo_cmd = is_verbose_mode(),
      spinner = quiet && interactive(),
      stderr_callback = function(x, p) {
        if (!startsWith(x, paste0(make_cmd(), ": *** No rule to make target"))) {
          message(x)
        }
        if (grepl("PCH file", x) || grepl("precompiled header", x) || grepl(".hpp.gch", x) ) {
          warning(
            "CmdStan's precompiled header (PCH) files may need to be rebuilt.\n",
            "If your model failed to compile please run rebuild_cmdstan().\n",
            "If the issue persists please open a bug report.",
            call. = FALSE
          )
        }
        if (grepl("No space left on device", x) || grepl("error in backend: IO failure on output stream", x)) {
          warning(
            "The C++ compiler ran out of disk space and was unable to build the executables for your model!\n",
            "See the above error for more details.",
            call. = FALSE
          )
        }
        if (os_is_macos()) {
          if (R.version$arch == "aarch64"
              && grepl("but the current translation unit is being compiled for target", x)) {
            warning(
              "The C++ compiler has errored due to incompatibility between the x86 and ",
              "Apple Silicon architectures.\n",
              "If you are running R inside an IDE (RStudio, VSCode, ...), ",
              "make sure the IDE is a native Apple Silicon app.\n",
              call. = FALSE
            )
          }
        }
      },
      error_on_status = FALSE
    )
  )
  if (is.na(run_log$status) || run_log$status != 0) {
    stop("An error occured during compilation! See the message above for more information.",
         call. = FALSE)
  }
  if (file.exists(exe)) {
    file.remove(exe)
  }
  file.copy(tmp_exe, exe, overwrite = TRUE)
  if (os_is_wsl()) {
    res <- processx::run(
      command = "wsl",
      args = c("chmod", "+x", wsl_safe_path(exe)),
      error_on_status = FALSE
    )
  }
  private$exe_file_ <- exe
  private$cpp_options_ <- cpp_options
  private$precompile_cpp_options_ <- NULL
  private$precompile_stanc_options_ <- NULL
  private$precompile_include_paths_ <- NULL
  private$model_methods_env_ <- new.env()
  suppressWarnings(private$model_methods_env_$hpp_code_ <- readLines(private$hpp_file_, warn = FALSE))
  if (compile_model_methods) {
    expose_model_methods(env = private$model_methods_env_,
                          verbose = !quiet,
                          hessian = compile_hessian_method)
  }
  invisible(self)
}
CmdStanModel$set("public", name = "compile", value = compile)

#' Input and output variables of a Stan program
#'
#' @name model-method-variables
#' @aliases variables
#' @family CmdStanModel methods
#'
#' @description The `$variables()` method of a [`CmdStanModel`] object returns
#'   a list, each element representing a Stan model block: `data`, `parameters`,
#'   `transformed_parameters` and `generated_quantities`.
#'
#'   Each element contains a list of variables, with each variables represented
#'   as a list with infromation on its scalar type (`real` or `int`) and
#'   number of dimensions.
#'
#'   `transformed data` is not included, as variables in that block are not
#'   part of the model's input or output.
#'
#' @return The `$variables()` returns a list with information on input and
#'   output variables for each of the Stan model blocks.
#'
#' @examples
#' \dontrun{
#' file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#'
#' # create a `CmdStanModel` object, compiling the model is not required
#' mod <- cmdstan_model(file, compile = FALSE)
#'
#' mod$variables()
#'
#' }
#'
variables <- function() {
  if (cmdstan_version() < "2.27.0") {
    stop("$variables() is only supported for CmdStan 2.27 or newer.", call. = FALSE)
  }
  if (length(self$stan_file()) == 0) {
    stop("'$variables()' cannot be used because the 'CmdStanModel' was not created with a Stan file.", call. = FALSE)
  }
  assert_stan_file_exists(self$stan_file())
  if (is.null(private$variables_) && file.exists(self$stan_file())) {
    private$variables_ <- model_variables(
      stan_file = self$stan_file(),
      include_paths = self$include_paths(),
      allow_undefined = private$using_user_header_
    )
  }
  private$variables_
}
CmdStanModel$set("public", name = "variables", value = variables)

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
  if (length(self$stan_file()) == 0) {
    stop("'$check_syntax()' cannot be used because the 'CmdStanModel' was not created with a Stan file.", call. = FALSE)
  }
  assert_stan_file_exists(self$stan_file())
  if (length(stanc_options) == 0 && !is.null(private$precompile_stanc_options_)) {
    stanc_options <- private$precompile_stanc_options_
  }
  if (is.null(include_paths) && !is.null(self$include_paths())) {
    include_paths <- self$include_paths()
  }

  temp_hpp_file <- tempfile(pattern = "model-", fileext = ".hpp")
  stanc_options[["o"]] <- wsl_safe_path(temp_hpp_file)

  if (pedantic) {
    stanc_options[["warn-pedantic"]] <- TRUE
  }

  stancflags_val <- include_paths_stanc3_args(include_paths)

  if (is.null(stanc_options[["name"]])) {
    stanc_options[["name"]] <- paste0(self$model_name(), "_model")
  }
  stanc_built_options <- c()
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

  withr::with_path(
    c(
      toolchain_PATH_env_var(),
      tbb_path()
    ),
    run_log <- wsl_compatible_run(
      command = stanc_cmd(),
      args = c(wsl_safe_path(self$stan_file()), stanc_built_options, stancflags_val),
      wd = cmdstan_path(),
      echo = is_verbose_mode(),
      echo_cmd = is_verbose_mode(),
      spinner = quiet && interactive(),
      stderr_callback = function(x, p) {
        message(x)
      },
      error_on_status = FALSE
    )
  )
  cat(run_log$stdout)
  if (is.na(run_log$status) || run_log$status != 0) {
    stop("Syntax error found! See the message above for more information.",
         call. = FALSE)
  }
  if (!quiet) {
    message("Stan program is syntactically correct")
  }
  invisible(TRUE)
}
CmdStanModel$set("public", name = "check_syntax", value = check_syntax)

#' Run stanc's auto-formatter on the model code.
#'
#' @name model-method-format
#' @aliases format
#' @family CmdStanModel methods
#'
#' @description The `$format()` method of a [`CmdStanModel`] object
#'   runs stanc's auto-formatter on the model code. Either saves the formatted
#'   model directly back to the file or prints it for inspection.
#'
#' @param overwrite_file (logical) Should the formatted code be written back
#'   to the input model file. The default is `FALSE`.
#' @param canonicalize (list or logical) Defines whether or not the compiler
#'   should 'canonicalize' the Stan model, removing things like deprecated syntax.
#'   Default is `FALSE`. If `TRUE`, all canonicalizations are run. You can also
#'   supply a list of strings which represent options. In that case the options
#'   are passed to stanc (new in Stan 2.29). See the [User's guide section](https://mc-stan.org/docs/stan-users-guide/stanc-pretty-printing.html#canonicalizing)
#'   for available canonicalization options.
#' @param backup (logical) If `TRUE`, create stanfile.bak backups before
#'   writing to the file. Disable this option if you're sure you have other
#'   copies of the file or are using a version control system like Git. Defaults
#'   to `TRUE`. The value is ignored if `overwrite_file = FALSE`.
#' @param max_line_length (integer) The maximum length of a line when formatting.
#'   The default is `NULL`, which defers to the default line length of stanc.
#' @param quiet (logical) Should informational messages be suppressed? The
#'   default is `FALSE`.
#'
#' @return The `$format()` method returns `TRUE` (invisibly) if the model
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
#'   real                     lambda;
#' }
#' model {
#'   target +=
#'  poisson_lpmf(y | lambda);
#' }
#' ")
#' mod <- cmdstan_model(file, compile = FALSE)
#' mod$format(canonicalize = TRUE)
#' }
#'
format <- function(overwrite_file = FALSE,
                   canonicalize = FALSE,
                   backup = TRUE,
                   max_line_length = NULL,
                   quiet = FALSE) {
  if (cmdstan_version() < "2.29.0" && !is.null(max_line_length)) {
    stop(
      "'max_line_length' is only supported with CmdStan 2.29.0 or newer.",
      call. = FALSE
    )
  }
  if (cmdstan_version() < "2.29.0" && !is.logical(canonicalize)) {
    stop(
      "A list can be supplied to the 'canonicalize' argument with CmdStan 2.29.0 or newer.",
      call. = FALSE
    )
  }
  if (length(self$stan_file()) == 0) {
    stop(
      "'$format()' cannot be used because the 'CmdStanModel'",
      " was not created with a Stan file.", call. = FALSE
    )
  }
  assert_stan_file_exists(self$stan_file())
  checkmate::assert_integerish(
    max_line_length,
    lower = 1, len = 1, null.ok = TRUE
  )
  stanc_options <- private$precompile_stanc_options_
  stancflags_val <- include_paths_stanc3_args(self$include_paths())
  stanc_options["auto-format"] <- TRUE
  if (!is.null(max_line_length)) {
    stanc_options["max-line-length"] <- max_line_length
  }
  if (isTRUE(canonicalize)) {
    stanc_options["print-canonical"] <- TRUE
    if (cmdstan_version() < "2.29.0") {
      stanc_options["auto-format"] <- NULL
    }
  } else if (is.list(canonicalize) && length(canonicalize) > 0){
    stanc_options["canonicalize"] <- paste0(canonicalize, collapse = ",")
  }
  stanc_built_options <- c()
  for (i in seq_len(length(stanc_options))) {
    option_name <- names(stanc_options)[i]
    if (isTRUE(as.logical(stanc_options[[i]])) && !is.numeric(stanc_options[[i]])) {
      stanc_built_options <- c(stanc_built_options, paste0("--", option_name))
    } else if (is.null(option_name) || !nzchar(option_name)) {
      stanc_built_options <- c(
        stanc_built_options,
        paste0("--", stanc_options[[i]])
      )
    } else {
      stanc_built_options <- c(
        stanc_built_options,
        paste0("--", option_name, "=", stanc_options[[i]])
      )
    }
  }
  withr::with_path(
    c(
      toolchain_PATH_env_var(),
      tbb_path()
    ),
    run_log <- wsl_compatible_run(
      command = stanc_cmd(),
      args = c(wsl_safe_path(self$stan_file()), stanc_built_options,
                stancflags_val),
      wd = cmdstan_path(),
      echo = is_verbose_mode(),
      echo_cmd = is_verbose_mode(),
      spinner = FALSE,
      stderr_callback = function(x, p) {
        message(x)
      },
      error_on_status = FALSE
    )
  )
  if (is.na(run_log$status) || run_log$status != 0) {
    stop("Syntax error found! See the message above for more information.",
         call. = FALSE)
  }
  out_file <- ""
  if (isTRUE(overwrite_file)) {
    if (backup) {
      backup_file <- paste0(self$stan_file(), ".bak-", base::format(Sys.time(), "%Y%m%d%H%M%S"))
      file.copy(self$stan_file(), backup_file)
      if (!quiet) {
        message(
          "Old version of the model stored to ",
          backup_file,
          "."
        )
      }
    }
    out_file <- self$stan_file()
  }
  cat(run_log$stdout, file = out_file, sep = "\n")
  if (isTRUE(overwrite_file)) {
    private$stan_code_ <- readLines(self$stan_file())
  }

  invisible(TRUE)
}
CmdStanModel$set("public", name = "format", value = format)

#' Run Stan's MCMC algorithms
#'
#' @name model-method-sample
#' @aliases sample
#' @family CmdStanModel methods
#'
#' @description The `$sample()` method of a [`CmdStanModel`] object runs Stan's
#'   main Markov chain Monte Carlo algorithm.
#'
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the
#'   [CmdStan User’s Guide](https://mc-stan.org/docs/cmdstan-guide/)
#'   for more details.
#'
#'   After model fitting any diagnostics specified via the `diagnostics`
#'   argument will be checked and warnings will be printed if warranted.
#'
#' @template model-common-args
#' @template model-sample-args
#' @param cores,num_cores,num_chains,num_warmup,num_samples,save_extra_diagnostics,max_depth,stepsize,validate_csv
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
                   show_messages = TRUE,
                   show_exceptions = TRUE,
                   diagnostics = c("divergences", "treedepth", "ebfmi"),
                   # deprecated
                   cores = NULL,
                   num_cores = NULL,
                   num_chains = NULL,
                   num_warmup = NULL,
                   num_samples = NULL,
                   validate_csv = NULL,
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
  if (!is.null(validate_csv)) {
    warning("'validate_csv' is deprecated. Please use 'diagnostics' instead.")
    if (is.logical(validate_csv)) {
      if (validate_csv) {
        diagnostics <- c("divergences", "treedepth", "ebfmi")
      } else {
        diagnostics <- NULL
      }
    }
  }

  if (cmdstan_version() >= "2.27.0" && !fixed_param) {
    if (self$has_stan_file() && file.exists(self$stan_file())) {
      if (!is.null(self$variables()) && length(self$variables()$parameters) == 0) {
        stop("Model contains no parameters. Please use 'fixed_param = TRUE'.", call. = FALSE)
      }
    }
  }
  if (fixed_param) {
    save_warmup <- FALSE
  }
  procs <- CmdStanMCMCProcs$new(
    num_procs = checkmate::assert_integerish(chains, lower = 1, len = 1),
    parallel_procs = checkmate::assert_integerish(parallel_chains, lower = 1, null.ok = TRUE),
    threads_per_proc = assert_valid_threads(threads_per_chain, self$cpp_options(), multiple_chains = TRUE),
    show_stderr_messages = show_exceptions,
    show_stdout_messages = show_messages
  )
  model_variables <- NULL
  if (is_variables_method_supported(self)) {
    model_variables <- self$variables()
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
    fixed_param = fixed_param,
    diagnostics = diagnostics
  )
  args <- CmdStanArgs$new(
    method_args = sample_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = self$functions,
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = checkmate::assert_integerish(chain_ids, lower = 1, len = chains, unique = TRUE, null.ok = FALSE),
    data_file = process_data(data, model_variables),
    save_latent_dynamics = save_latent_dynamics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = assert_valid_opencl(opencl_ids, self$cpp_options()),
    model_variables = model_variables
  )
  runset <- CmdStanRun$new(args, procs)
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
#' @param mpi_cmd (string) The MPI launcher used for launching MPI
#'   processes. The default launcher is `"mpiexec"`.
#' @param mpi_args (list) A list of arguments to use when launching MPI
#'   processes. For example, `mpi_args = list("n" = 4)` launches the executable
#'   as `mpiexec -n 4 model_executable`, followed by CmdStan arguments for the
#'   model executable.
#' @param validate_csv Deprecated. Use `diagnostics` instead.
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
                       show_messages = TRUE,
                       show_exceptions = TRUE,
                       diagnostics = c("divergences", "treedepth", "ebfmi"),
                       # deprecated
                       validate_csv = TRUE) {

  if (!is.null(validate_csv)) {
    warning("'validate_csv' is deprecated. Please use 'diagnostics' instead.")
    if (is.logical(validate_csv)) {
      if (validate_csv) {
        diagnostics <- c("divergences", "treedepth", "ebfmi")
      } else {
        diagnostics <- NULL
      }
    }
  }

  if (fixed_param) {
    chains <- 1
    save_warmup <- FALSE
  }
  procs <- CmdStanMCMCProcs$new(
    num_procs = checkmate::assert_integerish(chains, lower = 1, len = 1),
    parallel_procs = 1,
    show_stderr_messages = show_exceptions,
    show_stdout_messages = show_messages
  )
  model_variables <- NULL
  if (is_variables_method_supported(self)) {
    model_variables <- self$variables()
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
    fixed_param = fixed_param,
    diagnostics = diagnostics
  )
  args <- CmdStanArgs$new(
    method_args = sample_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = self$functions,
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = checkmate::assert_integerish(chain_ids, lower = 1, len = chains, unique = TRUE, null.ok = FALSE),
    data_file = process_data(data, model_variables),
    save_latent_dynamics = save_latent_dynamics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    model_variables = model_variables
  )
  runset <- CmdStanRun$new(args, procs)
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
#'   Stan's optimizer to obtain a (penalized) maximum likelihood estimate or a
#'   maximum a posteriori estimate (if `jacobian=TRUE`). See the
#'   [Maximum Likelihood Estimation](https://mc-stan.org/docs/cmdstan-guide/maximum-likelihood-estimation.html)
#'   section of the CmdStan User's Guide for more details.
#'
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the [CmdStan User’s
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/) for more details on the
#'   default arguments. The default values can also be obtained by checking the
#'   metadata of an example model, e.g.,
#'   `cmdstanr_example(method="optimize")$metadata()`.
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
#' @param jacobian (logical) Whether or not to use the Jacobian adjustment for
#'   constrained variables. By default this is `FALSE`, meaning optimization
#'   yields the (regularized) maximum likelihood estimate. Setting it to `TRUE`
#'   yields the maximum a posteriori estimate. See the
#'   [Maximum Likelihood Estimation](https://mc-stan.org/docs/cmdstan-guide/maximum-likelihood-estimation.html)
#'   section of the CmdStan User's Guide for more details.
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
                     jacobian = FALSE,
                     init_alpha = NULL,
                     iter = NULL,
                     tol_obj = NULL,
                     tol_rel_obj = NULL,
                     tol_grad = NULL,
                     tol_rel_grad = NULL,
                     tol_param = NULL,
                     history_size = NULL) {
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stdout_messages = (is.null(refresh) || refresh != 0),
    threads_per_proc = assert_valid_threads(threads, self$cpp_options())
  )
  model_variables <- NULL
  if (is_variables_method_supported(self)) {
    model_variables <- self$variables()
  }
  optimize_args <- OptimizeArgs$new(
    algorithm = algorithm,
    jacobian = jacobian,
    init_alpha = init_alpha,
    iter = iter,
    tol_obj = tol_obj,
    tol_rel_obj = tol_rel_obj,
    tol_grad = tol_grad,
    tol_rel_grad = tol_rel_grad,
    tol_param = tol_param,
    history_size = history_size
  )
  args <- CmdStanArgs$new(
    method_args = optimize_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = self$functions,
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = 1,
    data_file = process_data(data, model_variables),
    save_latent_dynamics = save_latent_dynamics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = assert_valid_opencl(opencl_ids, self$cpp_options()),
    model_variables = model_variables
  )
  runset <- CmdStanRun$new(args, procs)
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
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stdout_messages = (is.null(refresh) || refresh != 0),
    threads_per_proc = assert_valid_threads(threads, self$cpp_options())
  )
  model_variables <- NULL
  if (is_variables_method_supported(self)) {
    model_variables <- self$variables()
  }
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
  args <- CmdStanArgs$new(
    method_args = variational_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = self$functions,
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = 1,
    data_file = process_data(data, model_variables),
    save_latent_dynamics = save_latent_dynamics,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = assert_valid_opencl(opencl_ids, self$cpp_options()),
    model_variables = model_variables
  )
  runset <- CmdStanRun$new(args, procs)
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
#' NOTE: if you plan on making many calls to `$generate_quantities()` then the
#' most efficient option is to pass the paths of the CmdStan CSV output files
#' (this avoids CmdStanR having to rewrite the draws contained in the fitted
#' model object to CSV each time). If you no longer have the CSV files you can
#' use [draws_to_csv()] once to write them and then pass the resulting file
#' paths to `$generate_quantities()` as many times as needed.
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
  fitted_params_files <- process_fitted_params(fitted_params)
  procs <- CmdStanGQProcs$new(
    num_procs = length(fitted_params_files),
    parallel_procs = checkmate::assert_integerish(parallel_chains, lower = 1, null.ok = TRUE),
    threads_per_proc = assert_valid_threads(threads_per_chain, self$cpp_options(), multiple_chains = TRUE)
  )
  model_variables <- NULL
  if (is_variables_method_supported(self)) {
    model_variables <- self$variables()
  }
  gq_args <- GenerateQuantitiesArgs$new(fitted_params = fitted_params_files)
  args <- CmdStanArgs$new(
    method_args = gq_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = self$functions,
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = seq_along(fitted_params_files),
    data_file = process_data(data, model_variables),
    seed = seed,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = assert_valid_opencl(opencl_ids, self$cpp_options()),
    model_variables = model_variables
  )
  runset <- CmdStanRun$new(args, procs)
  runset$run_cmdstan()
  CmdStanGQ$new(runset)
}
CmdStanModel$set("public", name = "generate_quantities", value = generate_quantities)

#' Run Stan's diagnose method
#'
#' @name model-method-diagnose
#' @aliases diagnose
#' @family CmdStanModel methods
#'
#' @description The `$diagnose()` method of a [`CmdStanModel`] object
#'   runs Stan's basic diagnostic feature that will calculate the gradients
#'   of the initial state and compare them with gradients calculated by
#'   finite differences. Discrepancies between the two indicate that there is
#'   a problem with the model or initial states or else there is a bug in Stan.
#'
#' @inheritParams model-method-sample
#' @param epsilon (positive real) The finite difference step size. Default
#'   value is 1e-6.
#' @param error (positive real)  The error threshold. Default value is 1e-6.
#'
#' @return A [`CmdStanDiagnose`] object.
#'
#' @template seealso-docs
#' @inherit CmdStanDiagnose examples
#'
diagnose <- function(data = NULL,
                     seed = NULL,
                     init = NULL,
                     output_dir = NULL,
                     output_basename = NULL,
                     epsilon = NULL,
                     error = NULL) {
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stdout_messages = FALSE,
    show_stderr_messages = TRUE
  )
  model_variables <- NULL
  if (is_variables_method_supported(self)) {
    model_variables <- self$variables()
  }
  diagnose_args <- DiagnoseArgs$new(
    epsilon = epsilon,
    error = error
  )
  args <- CmdStanArgs$new(
    method_args = diagnose_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = self$functions,
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = 1,
    data_file = process_data(data, model_variables),
    seed = seed,
    init = init,
    output_dir = output_dir,
    output_basename = output_basename,
    model_variables = model_variables
  )
  runset <- CmdStanRun$new(args, procs)
  runset$run_cmdstan()

  CmdStanDiagnose$new(runset)
}
CmdStanModel$set("public", name = "diagnose", value = diagnose)

#' Expose Stan functions to R
#'
#' @name model-method-expose_functions
#' @aliases expose_functions fit-method-expose_functions
#' @family CmdStanModel methods
#'
#' @description The `$expose_functions()` method of a [`CmdStanModel`] object
#'   will compile the functions in the Stan program's `functions` block and
#'   expose them for use in \R. This can also be specified via the
#'   `compile_standalone` argument to the [`$compile()`][model-method-compile]
#'   method.
#'
#'   This method is also available for fitted model objects ([`CmdStanMCMC`], [`CmdStanVB`], etc.).
#'   See **Examples**.
#'
#'   Note: there may be many compiler warnings emitted during compilation but
#'   these can be ignored so long as they are warnings and not errors.
#'
#' @param global (logical) Should the functions be added to the Global
#'   Environment? The default is `FALSE`, in which case the functions are
#'   available via the `functions` field of the R6 object.
#' @param verbose (logical) Should detailed information about generated code be
#'   printed to the console? Defaults to `FALSE`.
#' @template seealso-docs
#' @examples
#' \dontrun{
#' stan_file <- write_stan_file(
#'  "
#'  functions {
#'    real a_plus_b(real a, real b) {
#'      return a + b;
#'    }
#'  }
#'  parameters {
#'    real x;
#'  }
#'  model {
#'    x ~ std_normal();
#'  }
#'  "
#' )
#' mod <- cmdstan_model(stan_file)
#' mod$expose_functions()
#' mod$functions$a_plus_b(1, 2)
#'
#' fit <- mod$sample(refresh = 0)
#' fit$expose_functions() # already compiled because of above but this would compile them otherwise
#' fit$functions$a_plus_b(1, 2)
#' }
#'
#'
expose_functions = function(global = FALSE, verbose = FALSE) {
  expose_stan_functions(self$functions, global, verbose)
  invisible(NULL)
}
CmdStanModel$set("public", name = "expose_functions", value = expose_functions)



# internal ----------------------------------------------------------------

assert_valid_opencl <- function(opencl_ids, cpp_options) {
  if (is.null(cpp_options[["stan_opencl"]])
      && !is.null(opencl_ids)) {
    stop("'opencl_ids' is set but the model was not compiled with for use with OpenCL.",
         "\nRecompile the model with 'cpp_options = list(stan_opencl = TRUE)'",
         call. = FALSE)
  }
  invisible(opencl_ids)
}

assert_valid_threads <- function(threads, cpp_options, multiple_chains = FALSE) {
  threads_arg <- if (multiple_chains) "threads_per_chain" else "threads"
  checkmate::assert_integerish(threads, .var.name = threads_arg,
                               null.ok = TRUE, lower = 1, len = 1)
  if (is.null(cpp_options[["stan_threads"]]) || !isTRUE(cpp_options[["stan_threads"]])) {
    if (!is.null(threads)) {
      warning(
        "'", threads_arg, "' is set but the model was not compiled with ",
        "'cpp_options = list(stan_threads = TRUE)' ",
        "so '", threads_arg, "' will have no effect!",
        call. = FALSE
      )
      threads <- NULL
    }
  } else if (isTRUE(cpp_options[["stan_threads"]]) && is.null(threads)) {
    stop(
      "The model was compiled with 'cpp_options = list(stan_threads = TRUE)' ",
      "but '", threads_arg, "' was not set!",
      call. = FALSE
    )
  }
  invisible(threads)
}

assert_valid_stanc_options <- function(stanc_options) {
  i <- 1
  names <- names(stanc_options)
  for (s in stanc_options) {
    if (!is.null(names[i]) && nzchar(names[i])) {
      name <- names[i]
    } else {
      name <- s
    }
    if (startsWith(name, "--")) {
      stop("No leading hyphens allowed in stanc options (", name, "). ",
           "Use options without leading hyphens, for example ",
           "`stanc_options = list('allow-undefined')`",
           call. = FALSE)
    }
    i <- i + 1
  }
  invisible(stanc_options)
}

assert_stan_file_exists <- function(stan_file) {
  if (!file.exists(stan_file)) {
    stop("The Stan file used to create the `CmdStanModel` object does not exist.", call. = FALSE)
  }
}

cpp_options_to_compile_flags <- function(cpp_options) {
  if (length(cpp_options) == 0) {
    return(NULL)
  }
  cpp_built_options <- c()
  for (i in seq_along(cpp_options)) {
    option_name <- names(cpp_options)[i]
    if (is.null(option_name) || !nzchar(option_name)) {
      cpp_built_options <- c(cpp_built_options, cpp_options[[i]])
    } else {
      cpp_built_options <- c(cpp_built_options, paste0(toupper(option_name), "=", cpp_options[[i]]))
    }
  }
  cpp_built_options
}

include_paths_stanc3_args <- function(include_paths = NULL) {
  stancflags <- NULL
  if (!is.null(include_paths)) {
    assert_dir_exists(include_paths, access = "r")
    include_paths <- sapply(absolute_path(include_paths), wsl_safe_path)
    paths_w_space <- grep(" ", include_paths)
    include_paths[paths_w_space] <- paste0("'", include_paths[paths_w_space], "'")
    include_paths <- paste0(include_paths, collapse = ",")
    if (cmdstan_version() >= "2.24") {
      include_paths_flag <- "--include-paths="
    } else {
      include_paths_flag <- "--include_paths="
    }
    stancflags <- paste0(stancflags, include_paths_flag, include_paths)
  }
  stancflags
}

model_variables <- function(stan_file, include_paths = NULL, allow_undefined = FALSE) {
  if (allow_undefined) {
    allow_undefined_arg <- "--allow-undefined"
  } else {
    allow_undefined_arg <- NULL
  }
  out_file <- tempfile(fileext = ".json")
  run_log <- wsl_compatible_run(
    command = stanc_cmd(),
    args = c(wsl_safe_path(stan_file),
              "--info",
              include_paths_stanc3_args(include_paths),
              allow_undefined_arg),
    wd = cmdstan_path(),
    echo = FALSE,
    echo_cmd = FALSE,
    stdout = out_file,
    error_on_status = TRUE
  )
  variables <- jsonlite::read_json(out_file, na = "null")
  variables$data <- variables$inputs
  variables$inputs <- NULL
  variables$transformed_parameters <- variables[["transformed parameters"]]
  variables[["transformed parameters"]] <- NULL
  variables$generated_quantities <- variables[["generated quantities"]]
  variables[["generated quantities"]] <- NULL
  variables$functions <- NULL
  variables$distributions <- NULL
  variables
}

model_compile_info <- function(exe_file) {
  info <- NULL
  if (cmdstan_version() > "2.26.1") {
    withr::with_path(
      c(
        toolchain_PATH_env_var(),
        tbb_path()
      ),
      ret <- wsl_compatible_run(
        command = wsl_safe_path(exe_file),
        args = "info",
        error_on_status = FALSE
      )
    )
    if (ret$status == 0) {
      info <- list()
      info_raw <- strsplit(strsplit(ret$stdout, "\n")[[1]], "=")
      for (key_val in info_raw) {
        if (length(key_val) > 1) {
          key_val <- trimws(key_val)
          val <- key_val[2]
          if (!is.na(as.logical(val))) {
            val <- as.logical(val)
          }
          info[[toupper(key_val[1])]] <- val
        }
      }
      info[["STAN_VERSION"]] <- paste0(info[["STAN_VERSION_MAJOR"]], ".", info[["STAN_VERSION_MINOR"]], ".", info[["STAN_VERSION_PATCH"]])
      info[["STAN_VERSION_MAJOR"]] <- NULL
      info[["STAN_VERSION_MINOR"]] <- NULL
      info[["STAN_VERSION_PATCH"]] <- NULL
    }
  }
  info
}

is_variables_method_supported <- function(mod) {
  cmdstan_version() >= "2.27.0" && mod$has_stan_file() && file.exists(mod$stan_file())
}
