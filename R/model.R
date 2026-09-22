#' Create a new CmdStanModel object
#'
#' @description \if{html}{\figure{logo.svg}{options: width=25}}
#'   Create a new [`CmdStanModel`] object from a file containing a Stan program
#'   or from an existing Stan executable. The [`CmdStanModel`] object stores the
#'   path to a Stan program and compiled executable (once created), and provides
#'   methods for fitting the model using Stan's algorithms.
#'
#'   If a Stan program with a pre-existing executable beside it is passed to
#'   `cmdstan_model()` it checks whether the executable was built with the
#'   options supplied and rebuilds it if not. If just an executable is provided,
#'   then it uses the executable as is.
#'
#'   `compile_stan_file()` runs the same build, or the same check of an
#'   existing executable, but returns the path to the executable instead of a
#'   `CmdStanModel` object. It is for code that builds an executable in one
#'   place and uses it in another, for example an R package that compiles its
#'   Stan programs when it is installed and later calls
#'   `cmdstan_model(exe_file = )`.
#'
#' @export
#' @param stan_file (string) The path to a `.stan` file containing a Stan
#'   program. The helper function [write_stan_file()] is provided for cases when
#'   it is more convenient to specify the Stan program as a string. If
#'   `stan_file` is not specified then `exe_file` must be specified. Use `dir`
#'   to choose where the executable for a Stan program is built.
#' @param exe_file (string) The path to an existing Stan model executable to use
#'   instead of a Stan program. The executable is used as is: none of the build
#'   arguments below can be specified. Some `CmdStanModel` methods like
#'   `$code()` and `$print()` also will not work. `stan_file` and `exe_file`
#'   cannot both be supplied.
#' @param quiet (logical) Should the verbose output from CmdStan during
#'   compilation be suppressed? The default is `TRUE`, but if you encounter an
#'   error we recommend trying again with `quiet=FALSE` to see more of the
#'   output.
#' @param dir (string) The path to the directory in which to store the CmdStan
#'   executable. The default is the same location as the Stan program.
#' @param pedantic (logical) Should pedantic mode be turned on? The default is
#'   `FALSE`. Pedantic mode attempts to warn you about potential issues in your
#'   Stan program beyond syntax errors. For details see the [*Pedantic mode*
#'   section](https://mc-stan.org/docs/stan-users-guide/pedantic-mode.html) in
#'   the Stan User's Guide. The check runs whether or not the executable is
#'   rebuilt. To check a program without building it use
#'   [`check_syntax_stan_file()`][model-method-check_syntax].
#' @param include_paths (character vector) Paths to directories where Stan
#'   should look for files specified in `#include` directives in the Stan
#'   program. Relative paths are resolved against the working directory when
#'   the model object is created and stored as absolute paths, so subsequent
#'   changes to the working directory do not affect them. When the program
#'   contains `#include` directives and no paths are given, the program's own
#'   directory is used.
#' @param user_header (string) The path to a C++ file (with a `.hpp` extension)
#'   to compile with the Stan model.
#' @param cpp_options (list) Any makefile options to be used when compiling the
#'   model (`stan_threads`, `stan_mpi`, `stan_opencl`, etc.), written as
#'   `list(NAME = value)`. Each entry is an assignment you could make in the
#'   `make/local` file, so `list(CXXFLAGS = "-O3")` rather than `"-O3"`.
#'   Every entry must be named with a `Make` variable name, in any casing.
#'   Setting an option to `FALSE` or `NULL` passes an empty assignment such as
#'   `STAN_THREADS=`. That empties the variable for this build, which turns a
#'   switch off, and overrides whatever `make/local` sets. See
#'   [stan_build_info()] for an example of setting options and checking what
#'   the executable was built with, and the Stan case study [Reduce Sum: A
#'   Minimal
#'   Example](https://mc-stan.org/users/documentation/case-studies/reduce_sum_tutorial.html)
#'   for using threading.
#' @param stanc_options (list) Any Stan-to-C++ transpiler options to be used
#'   when compiling the model. A flag is given by name without the leading
#'   hyphens, as `list("O1")` or `list(O1 = TRUE)`, and an option that takes a
#'   value as `list(option = "value")`. See [stan_build_info()] for an example
#'   and the [`stanc` chapter of the CmdStan User's
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/stanc.html) for the
#'   available options. Options that CmdStanR sets from its own arguments
#'   cannot be passed here: `include-paths` (use `include_paths`),
#'   `warn-pedantic` (`pedantic`), `allow-undefined` (`user_header`),
#'   `use-opencl` (`cpp_options = list(stan_opencl = TRUE)`) and `name` (taken
#'   from the name of the Stan file).
#' @param force_recompile (logical) Should the model be recompiled even if the
#'   executable was built from this program with these options? The default,
#'   `NULL`, defers to the `cmdstanr_force_recompile` global option, and to
#'   `FALSE` when that is unset. Use `force_recompile=TRUE` for changes
#'   CmdStanR cannot see: a changed toolchain, a CmdStan modified in place, a
#'   header the user header includes, or a makefile that `make/local` includes.
#'
#' @return `cmdstan_model()` returns a [`CmdStanModel`] object.
#'   `compile_stan_file()` returns the path to the executable.
#'
#' @seealso [install_cmdstan()],
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
#' # Print with line numbers. This can be set globally using the
#' # `cmdstanr_print_line_numbers` option.
#' mod$print(line_numbers = TRUE)
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
#' # Check sampling diagnostics
#' fit_mcmc$diagnostic_summary()
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
#' # Run 'optimize' method to get a point estimate (default is Stan's LBFGS algorithm)
#' # and also demonstrate specifying data as a path to a file instead of a list
#' my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.json")
#' fit_optim <- mod$optimize(data = my_data_file, seed = 123)
#' fit_optim$summary()
#'
#' # Run 'optimize' again with 'jacobian=TRUE' and then draw from Laplace approximation
#' # to the posterior
#' fit_optim <- mod$optimize(data = my_data_file, jacobian = TRUE)
#' fit_laplace <- mod$laplace(data = my_data_file, mode = fit_optim, draws = 2000)
#' fit_laplace$summary()
#'
#' # Run 'variational' method to use ADVI to approximate posterior
#' fit_vb <- mod$variational(data = stan_data, seed = 123)
#' fit_vb$summary()
#' mcmc_hist(fit_vb$draws("theta"))
#'
#' # Run the Pathfinder variational inference method
#' fit_pf <- mod$pathfinder(data = stan_data, seed = 123)
#' fit_pf$summary()
#' mcmc_hist(fit_pf$draws("theta"))
#'
#' # Run 'pathfinder' again with more paths, fewer draws per path,
#' # better covariance approximation, and fewer LBFGSs iterations
#' fit_pf <- mod$pathfinder(data = stan_data, num_paths=10, single_path_draws=40,
#'                          history_size=50, max_lbfgs_iters=100)
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
cmdstan_model <- function(stan_file = NULL,
                          exe_file = NULL,
                          quiet = TRUE,
                          dir = NULL,
                          pedantic = FALSE,
                          include_paths = NULL,
                          user_header = NULL,
                          cpp_options = NULL,
                          stanc_options = NULL,
                          force_recompile = NULL) {
  if (is.null(exe_file) && is.null(stan_file)) {
    stop(
      "Unable to create a `CmdStanModel` object. ",
      "Both 'stan_file' and 'exe_file' are undefined.",
      call. = FALSE
    )
  }
  if (!is.null(exe_file) && !is.null(stan_file)) {
    stop(
      "`stan_file` and `exe_file` cannot both be supplied. ",
      "Use `dir` to choose where the executable for a Stan program is built.",
      call. = FALSE
    )
  }
  CmdStanModel$new(
    stan_file = stan_file,
    exe_file = exe_file,
    quiet = quiet,
    dir = dir,
    pedantic = pedantic,
    include_paths = include_paths,
    user_header = user_header,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile
  )
}

#' @rdname cmdstan_model
#' @export
compile_stan_file <- function(stan_file,
                              quiet = TRUE,
                              dir = NULL,
                              pedantic = FALSE,
                              include_paths = NULL,
                              user_header = NULL,
                              cpp_options = NULL,
                              stanc_options = NULL,
                              force_recompile = NULL) {
  assert_file_exists(
    stan_file, access = "r", extension = c("stan", "stanfunctions")
  )
  built <- build_executable(
    resolve_path(stan_file),
    dir = dir,
    include_paths = include_paths,
    user_header = user_header,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    pedantic = pedantic,
    force_recompile = force_recompile,
    quiet = quiet
  )
  built$exe_file
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
#'  ## The Stan program
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$stan_file()`][model-method-model-info] | Return the file path to the Stan program. |
#'  [`$has_stan_file()`][model-method-model-info] | Check whether the model was created with a Stan file. |
#'  [`$model_name()`][model-method-model-info] | Return the model name. |
#'  [`$code()`][model-method-model-info] | Return Stan program as a character vector. |
#'  [`$print()`][model-method-model-info] | Print readable version of Stan program. |
#'  [`$include_paths()`][model-method-model-info] | Return the Stan include paths. |
#'  [`$variables()`][model-method-variables] | Return the input and output variables of the program, by block. |
#'  [`$check_syntax()`][model-method-check_syntax]  |  Check Stan syntax without having to compile. |
#'  [`$format()`][model-method-format]  |  Format and canonicalize the Stan model code. |
#'
#'  ## The executable
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$exe_file()`][model-method-model-info] |  Return the file path to the compiled executable. |
#'  [`$build_info()`][model-method-build_info] |  Report how the executable was built, from its build record. |
#'  [`$cmdstan_version()`][model-method-model-info] | Return the CmdStan version that built the executable. |
#'  [`$cpp_options()`][model-method-model-info] | Return the C++ options associated with the model. |
#'  [`$user_header()`][model-method-model-info] | Return the path to the user header, if the model has one. |
#'  [`$hpp_file()`][model-method-model-info] |  Return the file path to the `.hpp` file containing the generated C++ code. |
#'  [`$save_hpp_file()`][model-method-model-info] |  Save the `.hpp` file containing the generated C++ code. |
#'  [`$cmdstan_defaults()`][model-method-cmdstan_defaults] |  Get CmdStan default argument values for a method. |
#'  [`$expose_functions()`][model-method-expose_functions] |  Expose Stan functions for use in R. |
#'
#'  ## Diagnostics
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$diagnose()`][model-method-diagnose] |  Run CmdStan's `"diagnose"` method to test gradients, return [`CmdStanDiagnose`] object. |
#'
#'  ## Model fitting
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$sample()`][model-method-sample] |  Run CmdStan's `"sample"` method, return [`CmdStanMCMC`] object. |
#'  [`$sample_mpi()`][model-method-sample_mpi] |  Run CmdStan's `"sample"` method with [MPI](https://mc-stan.org/math/md_doxygen_2parallelism__support_2mpi__parallelism.html), return [`CmdStanMCMC`] object. |
#'  [`$optimize()`][model-method-optimize] |  Run CmdStan's `"optimize"` method, return [`CmdStanMLE`] object. |
#'  [`$laplace()`][model-method-laplace] |  Run CmdStan's `"laplace"` method, return [`CmdStanLaplace`] object. |
#'  [`$variational()`][model-method-variational] |  Run CmdStan's `"variational"` method, return [`CmdStanVB`] object. |
#'  [`$pathfinder()`][model-method-pathfinder] |  Run CmdStan's `"pathfinder"` method, return [`CmdStanPathfinder`] object. |
#'  [`$generate_quantities()`][model-method-generate-quantities] |  Run CmdStan's `"generate quantities"` method, return [`CmdStanGQ`] object. |
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
CmdStanModel <- R6::R6Class(
  classname = "CmdStanModel",
  private = list(
    # What the object was built from or adopted with. After construction only
    # `hpp_file_` changes (when `$save_hpp_file()` moves the file) and
    # `model_methods_env_` gets filled in when the methods compile.
    stan_file_ = character(),
    stan_code_ = character(),
    model_name_ = character(),
    include_paths_ = NULL,
    user_header_ = NULL,
    exe_file_ = character(),
    executable_hash_ = NULL,
    record_ = NULL,
    reported_features_ = NULL,
    cmdstan_version_ = NULL,
    variables_ = NULL,
    hpp_file_ = character(),
    model_methods_env_ = NULL,
    # Every method that runs the executable, and $expose_functions(), calls
    # this first. It checks that the executable is the one this object was
    # built with and, for a model with a Stan file, that nothing it was built
    # from has changed. $build_info() doesn't call it since it reads whatever is
    # on disk now.
    assert_current = function() {
      exe <- private$exe_file_
      if (!self$has_stan_file()) {
        if (!file.exists(exe)) {
          stop_stale_executable(
            paste0("The executable at '", exe, "' no longer exists.")
          )
        }
        if (!identical(hash_file(exe), private$executable_hash_)) {
          stop_stale_executable(paste0(
            "The executable at '", exe,
            "' changed after this model was created."
          ))
        }
        return(invisible(self))
      }
      assert_stan_file_exists(private$stan_file_)
      current <- read_current_build(
        private$stan_file_, private$include_paths_, private$user_header_, exe
      )
      reasons <- assess_build(
        list(
          configuration = private$record_$configuration,
          executable_hash = private$executable_hash_
        ),
        current
      )
      if (length(reasons) > 0) {
        stop_stale_executable(c(
          "The executable is out of date:",
          paste0("  - ", rebuild_reasons(reasons, current)),
          "Run cmdstan_model() to rebuild it."
        ))
      }
      invisible(self)
    },
    # The C++ for the standalone functions, generated from the Stan file the
    # first time it's needed, after assert_current() checks the file is the one
    # the executable was built from. Generating it in the constructor would run
    # stanc for a feature most models never use. Empty for a model without a
    # Stan file.
    standalone_functions = function() {
      if (self$has_stan_file() && is.null(self$functions$hpp_code)) {
        configuration <- private$record_$configuration
        self$functions$hpp_code <- get_standalone_hpp(
          private$stan_file_,
          c("--standalone-functions",
            include_paths_stanc3_args(
              private$include_paths_, direct_call = TRUE
            ),
            unlist(configuration[c(
              "stanc_options", "stanc_options_added",
              "stanc_options_from_make"
            )], use.names = FALSE))
        )
      }
      self$functions
    }
  ),
  public = list(
    functions = NULL,
    initialize = function(stan_file = NULL,
                          exe_file = NULL,
                          quiet = TRUE,
                          dir = NULL,
                          pedantic = FALSE,
                          include_paths = NULL,
                          user_header = NULL,
                          cpp_options = NULL,
                          stanc_options = NULL,
                          force_recompile = NULL) {
      self$functions <- new.env()
      self$functions$compiled <- FALSE
      if (!is.null(stan_file)) {
        assert_file_exists(stan_file, access = "r", extension = c("stan", "stanfunctions"))
        private$stan_file_ <- resolve_path(stan_file)
        private$stan_code_ <- readLines(stan_file)
        private$model_name_ <- model_name_from_path(private$stan_file_)
        built <- build_executable(
          private$stan_file_,
          dir = dir,
          include_paths = include_paths,
          user_header = user_header,
          cpp_options = cpp_options,
          stanc_options = stanc_options,
          pedantic = pedantic,
          force_recompile = force_recompile,
          quiet = quiet
        )
        private$exe_file_ <- built$exe_file
        private$include_paths_ <- built$include_paths
        private$variables_ <- variables_from_info(built$info)
        private$hpp_file_ <- tempfile(pattern = "model-", fileext = ".hpp")
        writeLines(built$hpp_code, private$hpp_file_)
        private$model_methods_env_ <- new.env()
        private$model_methods_env_$hpp_code_ <- built$hpp_code
        facts <- facts_from_record(built$record)
      } else {
        assert_no_build_args_for_exe_only(
          cpp_options, stanc_options, include_paths, user_header,
          force_recompile, pedantic, dir
        )
        ext <- if (os_is_windows() && !os_is_wsl()) "exe" else ""
        assert_file_exists(exe_file, access = "r", extension = ext)
        private$exe_file_ <- resolve_path(exe_file)
        private$model_name_ <- model_name_from_path(private$exe_file_)
        facts <- adopt_executable(private$exe_file_)
      }
      private$record_ <- facts$record
      private$executable_hash_ <- facts$executable_hash
      private$reported_features_ <- facts$reported_features
      private$cmdstan_version_ <- facts$version
      private$user_header_ <-
        facts$record$dependencies[["user_header"]][["built_from"]]
      invisible(self)
    },
    include_paths = function() {
      private$include_paths_
    },
    code = function() {
      if (length(private$stan_code_) == 0) {
        warning("'$code()' will return NULL because the 'CmdStanModel' was not created with a Stan file.", call. = FALSE)
        return(NULL)
      }
      private$stan_code_
    },
    print = function(line_numbers = getOption("cmdstanr_print_line_numbers", FALSE)) {
      if (length(private$stan_code_) == 0) {
        stop("'$print()' cannot be used because the 'CmdStanModel' was not created with a Stan file.", call. = FALSE)
      }
      lines <- self$code()
      if (line_numbers) {
        line_num_indent <- nchar(as.character(length(lines)))
        line_nums <- vapply(seq_along(lines), function(y) {
          paste0(
            rep(" ", line_num_indent - nchar(as.character(y))), y, collapse = ""
          )
        }, character(1))
        lines <- paste(paste(line_nums, lines, sep = ": "), collapse = "\n")
      }
      cat(lines, sep = "\n")
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
    exe_file = function() {
      private$exe_file_
    },
    cmdstan_version = function() {
      private$cmdstan_version_
    },
    cpp_options = function() {
      private$record_$configuration$cpp_options %||%
        structure(list(), names = character())
    },
    user_header = function() {
      private$user_header_
    },
    hpp_file = function() {
      if (!self$has_stan_file()) {
        stop("'$hpp_file()' cannot be used because the 'CmdStanModel' was not created with a Stan file.", call. = FALSE)
      }
      private$hpp_file_
    },
    save_hpp_file = function(dir = NULL) {
      hpp_file <- self$hpp_file()
      if (is.null(dir)) {
        dir <- dirname(private$stan_file_)
      }
      assert_dir_exists(dir, access = "r")
      new_hpp_loc <- file.path(dir, paste0(strip_ext(basename(private$stan_file_)), ".hpp"))
      file.copy(hpp_file, new_hpp_loc, overwrite = TRUE)
      file.remove(hpp_file)
      message("Moved .hpp file and set internal path to new location:\n",
              "- ", new_hpp_loc)
      private$hpp_file_ <- new_hpp_loc
      invisible(private$hpp_file_)
    }
  )
)

# CmdStanModel information methods ---------------------------------------------

#' Access information from a `CmdStanModel` object
#'
#' @name model-method-model-info
#' @family CmdStanModel methods
#'
#' @description These methods access information stored in a [`CmdStanModel`]
#'   object, print its Stan program, and manage paths to its executable and
#'   generated C++ file. For how the executable was built, see the
#'   [`$build_info()`][model-method-build_info] method, which has its own page.
#'
#'   ```
#'   stan_file()
#'   has_stan_file()
#'   code()
#'   print(line_numbers = getOption("cmdstanr_print_line_numbers", FALSE))
#'   model_name()
#'   exe_file()
#'   include_paths()
#'   cmdstan_version()
#'   cpp_options()
#'   user_header()
#'   hpp_file()
#'   save_hpp_file(dir = NULL)
#'   ```
#'
#' @param line_numbers (logical) Should line numbers be printed? The default is
#'   `getOption("cmdstanr_print_line_numbers", FALSE)`.
#' @param dir (string) The directory in which to save the `.hpp` file. The
#'   default is the directory containing the Stan program.
#'
#' @return
#' * `$stan_file()` returns a path as a string, or `character(0)` if the model
#'   was created without a Stan file.
#' * `$has_stan_file()` returns `TRUE` if the model was created with a Stan file
#'   and `FALSE` otherwise.
#' * `$code()` returns a character vector with one element per line of Stan
#'   code, or `NULL` if the model was created without a Stan file.
#' * `$print()` returns the [`CmdStanModel`] object invisibly.
#' * `$model_name()` returns the model name as a string.
#' * `$exe_file()` returns a path as a string, or `character(0)` if no
#'   executable path is set.
#' * `$include_paths()` returns a character vector of absolute paths or `NULL`.
#' * `$cmdstan_version()` returns the version of CmdStan that built the
#'   executable, as a string.
#' * `$cpp_options()` returns a named list of C++ options, with names in their
#'   `make` spelling.
#' * `$user_header()` returns the absolute path to the user header as a string,
#'   or `NULL` if the model has no user header.
#' * `$hpp_file()` returns the path to the `.hpp` file holding the C++ code
#'   generated for the Stan program when the model object was created. It
#'   errors if the model was created without a Stan file.
#' * `$save_hpp_file()` moves the `.hpp` file to `dir`, updates the stored
#'   path, and returns the new path invisibly.
#'
#' @seealso [cmdstan_model()]
#' @template seealso-docs
#'
NULL

# CmdStanModel methods -----------------------------------

#' Input and output variables of a Stan program
#'
#' @name model-method-variables
#' @aliases variables
#' @family CmdStanModel methods
#'
#' @description The `$variables()` method of a [`CmdStanModel`] object returns a
#'   list, each element representing a Stan model block: `data`, `parameters`,
#'   `transformed_parameters` and `generated_quantities`. The information
#'   describes the program the executable was built from, captured when the
#'   model object was created.
#'
#'   The standalone function `variables_stan_file()` returns the same list for a
#'   Stan program as it is now, without creating a model object or compiling.
#'
#'   Each element in the returned object contains a list of variables, with each
#'   variable represented as a list with information on its scalar type (`real`
#'   or `int`) and number of dimensions.
#'
#'   The number of dimensions reported is the number of indexing dimensions in
#'   the declared Stan variable, equivalently the number of indices needed to
#'   access one scalar element. This means a scalar has 0 dimensions, a vector
#'   or one-dimensional array has 1, and a matrix or two-dimensional array has
#'   2. Array dimensions are added to any vector or matrix dimensions, so
#'   `array[J] matrix[N, K]` has 3 dimensions. See **Examples**.
#'
#'   `transformed data` is not included, as variables in that block are not
#'   part of the model's input or output.
#'
#' @return A list with information on input and output variables for each of
#'   the Stan model blocks.
#'
#' @seealso [write_stan_json()] for writing data for CmdStan.
#'
#' @examples
#' \dontrun{
#' stan_file <- write_stan_file("
#' data {
#'   int N;
#'   array[2, 3] int y;
#' }
#' parameters {
#'   real alpha;
#'   vector[N] beta;
#'   array[2] matrix[3, 4] theta;
#' }
#' ")
#'
#' mod <- cmdstan_model(stan_file)
#'
#' vars <- mod$variables()
#' str(vars)
#' }
#'
variables <- function() {
  if (length(self$stan_file()) == 0) {
    stop(
      "'$variables()' cannot be used because the 'CmdStanModel' ",
      "was not created with a Stan file.",
      call. = FALSE
    )
  }
  private$variables_
}
CmdStanModel$set("public", name = "variables", value = variables)

#' @rdname model-method-variables
#' @export
#' @param stan_file (string) The path to a Stan program.
#' @inheritParams cmdstan_model
variables_stan_file <- function(stan_file, include_paths = NULL) {
  assert_file_exists(
    stan_file, access = "r", extension = c("stan", "stanfunctions")
  )
  stan_file <- resolve_path(stan_file)
  include_paths <- effective_include_paths(stan_file, include_paths)
  variables_from_info(stanc_info(stan_file, include_paths))
}

#' Check syntax of a Stan program
#'
#' @name model-method-check_syntax
#' @aliases check_syntax
#' @family CmdStanModel methods
#'
#' @description The `$check_syntax()` method of a [`CmdStanModel`] object
#'   checks the Stan program for syntax errors and returns `TRUE` (invisibly) if
#'   parsing succeeds. If invalid syntax is found an error is thrown.
#'
#'   The standalone function `check_syntax_stan_file()` does the same for a Stan
#'   program without creating a model object, and so without compiling it.
#'
#' @param stan_file (string) The path to a Stan program.
#' @param pedantic (logical) Should pedantic mode be turned on? The default is
#'   `FALSE`. Pedantic mode attempts to warn you about potential issues in your
#'   Stan program beyond syntax errors. For details see the [*Pedantic mode*
#'   chapter](https://mc-stan.org/docs/stan-users-guide/pedantic-mode.html) in
#'   the Stan User's Guide.
#' @param include_paths (character vector) Paths to directories where Stan
#'   should look for files specified in `#include` directives in the Stan
#'   program. The method uses the model's own include paths when none are
#'   given. `check_syntax_stan_file()` uses the program's own directory when
#'   none are given and the program contains `#include` directives.
#' @param stanc_options (list) Any other Stan-to-C++ transpiler options to be
#'   used when compiling the model. See the documentation for
#'   [cmdstan_model()] for details.
#' @param quiet (logical) Should informational messages be suppressed? The
#'   default is `FALSE`, which will print a message if the Stan program is valid
#'   or the compiler error message if there are syntax errors. If `TRUE`, only
#'   the error message will be printed.
#'
#' @return `TRUE` (invisibly) if the program is valid.
#'
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' file <- write_stan_file("
#' data {
#'   int N;
#'   array[N] int y;
#' }
#' parameters {
#'   // should have <lower=0> but omitting to demonstrate pedantic mode
#'   real lambda;
#' }
#' model {
#'   y ~ poisson(lambda);
#' }
#' ")
#' mod <- cmdstan_model(file)
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
  check_syntax_stan_file(
    self$stan_file(),
    include_paths = include_paths %||% self$include_paths(),
    pedantic = pedantic,
    stanc_options = stanc_options,
    quiet = quiet
  )
}
CmdStanModel$set("public", name = "check_syntax", value = check_syntax)

#' @rdname model-method-check_syntax
#' @export
check_syntax_stan_file <- function(stan_file,
                                   include_paths = NULL,
                                   pedantic = FALSE,
                                   stanc_options = list(),
                                   quiet = FALSE) {
  assert_file_exists(
    stan_file, access = "r", extension = c("stan", "stanfunctions")
  )
  stan_file <- resolve_path(stan_file)
  stanc_options <- assert_valid_stanc_options(stanc_options)
  stanc_options[["allow-undefined"]] <- TRUE
  hpp_file <- tempfile(pattern = "model-", fileext = ".hpp")
  withr::defer(unlink(hpp_file))
  stanc_options[["o"]] <- wsl_safe_path(hpp_file)
  if (pedantic) {
    stanc_options[["warn-pedantic"]] <- TRUE
  }
  stanc_options[["name"]] <- paste0(model_name_from_path(stan_file), "_model")
  run_stanc(
    stan_file,
    c(stanc_options_to_args(stanc_options),
      include_paths_stanc3_args(
        effective_include_paths(stan_file, include_paths), direct_call = TRUE
      )),
    spinner = quiet && use_spinner()
  )
  if (!quiet) {
    message("Stan program is syntactically correct")
  }
  invisible(TRUE)
}

#' Run stanc's auto-formatter on the model code.
#'
#' @name model-method-format
#' @aliases format
#' @family CmdStanModel methods
#'
#' @description The `$format()` method of a [`CmdStanModel`] object
#'   runs stanc's auto-formatter on the model code. It either saves the
#'   formatted model directly back to the file or prints it for inspection.
#'   The standalone function `format_stan_file()` does the same for a Stan
#'   program without creating a model object.
#'
#' @param stan_file (string) The path to a Stan program.
#' @inheritParams cmdstan_model
#' @param overwrite_file (logical) Should the formatted code be written back
#'   to the input model file? The default is `FALSE`.
#' @param canonicalize (list or logical) Defines whether or not the compiler
#'   should 'canonicalize' the Stan model, removing things like deprecated syntax.
#'   Default is `FALSE`. If `TRUE`, all canonicalizations are run. You can also
#'   supply a list of strings which represent options. In that case the options
#'   are passed to `stanc`. See the
#'   [User's guide section](https://mc-stan.org/docs/stan-users-guide/stanc-pretty-printing.html#canonicalizing)
#'   for available canonicalization options.
#' @param backup (logical) If `TRUE`, create a backup before writing to the
#'   file. The backup filename is the Stan filename followed by
#'   `.bak-YYYYMMDDHHMMSS`, where the final digits encode the timestamp. Disable
#'   this option if you're sure you have other copies of the file or are using a
#'   version control system like Git. Defaults to `TRUE`. The value is ignored
#'   if `overwrite_file = FALSE`.
#' @param max_line_length (integer) The maximum length of a line when formatting.
#'   The default is `NULL`, which defers to the default line length of stanc.
#' @param quiet (logical) Should informational messages be suppressed? The
#'   default is `FALSE`.
#'
#' @return `TRUE` (invisibly) if formatting succeeds.
#'
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#'
#' # Example of removing unnecessary whitespace
#' file <- write_stan_file("
#' data {
#'   int N;
#'   array[N] int y;
#' }
#' parameters {
#'   real                     lambda;
#' }
#' model {
#'   target +=
#'  poisson_lpmf(y | lambda);
#' }
#' ")
#'
#' format_stan_file(file, canonicalize = list("deprecations"))
#'
#' # or through a model object
#' mod <- cmdstan_model(file)
#' mod$format(canonicalize = list("deprecations"))
#'
#' # overwrite the original file instead of just printing it, then create the
#' # model object again to rebuild the executable from the formatted program
#' mod$format(canonicalize = list("deprecations"), overwrite_file = TRUE)
#' mod <- cmdstan_model(file)
#' }
#'
format <- function(overwrite_file = FALSE,
                   canonicalize = FALSE,
                   backup = TRUE,
                   max_line_length = NULL,
                   quiet = FALSE) {
  if (length(self$stan_file()) == 0) {
    stop(
      "'$format()' cannot be used because the 'CmdStanModel'",
      " was not created with a Stan file.", call. = FALSE
    )
  }
  assert_stan_file_exists(self$stan_file())
  format_stan_file(
    self$stan_file(),
    include_paths = self$include_paths(),
    overwrite_file = overwrite_file,
    canonicalize = canonicalize,
    backup = backup,
    max_line_length = max_line_length,
    quiet = quiet
  )
}
CmdStanModel$set("public", name = "format", value = format)

#' @rdname model-method-format
#' @export
format_stan_file <- function(stan_file,
                             include_paths = NULL,
                             overwrite_file = FALSE,
                             canonicalize = FALSE,
                             backup = TRUE,
                             max_line_length = NULL,
                             quiet = FALSE) {
  assert_file_exists(
    stan_file, access = "r", extension = c("stan", "stanfunctions")
  )
  stan_file <- resolve_path(stan_file)
  checkmate::assert_integerish(
    max_line_length,
    lower = 1, len = 1, null.ok = TRUE
  )
  stanc_options <- list("allow-undefined" = TRUE, "auto-format" = TRUE)
  if (!is.null(max_line_length)) {
    stanc_options[["max-line-length"]] <- max_line_length
  }
  if (isTRUE(canonicalize)) {
    stanc_options[["print-canonical"]] <- TRUE
  } else if (is.list(canonicalize) && length(canonicalize) > 0) {
    stanc_options[["canonicalize"]] <- paste0(canonicalize, collapse = ",")
  }
  formatted <- run_stanc(
    stan_file,
    c(stanc_options_to_args(stanc_options),
      include_paths_stanc3_args(
        effective_include_paths(stan_file, include_paths), direct_call = TRUE
      ))
  )
  out_file <- ""
  if (isTRUE(overwrite_file)) {
    if (backup) {
      backup_file <- paste0(
        stan_file, ".bak-", base::format(Sys.time(), "%Y%m%d%H%M%S")
      )
      file.copy(stan_file, backup_file)
      if (!quiet) {
        message("Old version of the model stored to ", backup_file, ".")
      }
    }
    out_file <- stan_file
  }
  cat(formatted, file = out_file, sep = "
")
  invisible(TRUE)
}

#' Run Stan's MCMC algorithms
#'
#' @name model-method-sample
#' @aliases sample
#' @family CmdStanModel methods
#'
#' @description The `$sample()` method of a [`CmdStanModel`] object runs Stan's
#'   main Markov chain Monte Carlo algorithm.
#'
#'   After model fitting any diagnostics specified via the `diagnostics`
#'   argument will be checked and warnings will be printed if warranted.
#'
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the [CmdStan User’s
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/) for more details on the
#'   default arguments. These values are also available via the
#'   [`$cmdstan_defaults`][model-method-cmdstan_defaults] method.
#'
#' @template model-common-args
#' @template model-save-latent-dynamics-arg
#' @template model-sample-args
#'
#' @return A [`CmdStanMCMC`] object.
#'
#' @references
#' * Hoffman, M. D., and Gelman, A. (2014). The No-U-Turn sampler:
#'   adaptively setting path lengths in Hamiltonian Monte Carlo.
#'   *Journal of Machine Learning Research*, 15(47), 1593-1623.
#' * Betancourt, M. (2017). A conceptual introduction to Hamiltonian Monte Carlo.
#'   arXiv:1701.02434. Appendix A describes Stan's dynamic HMC/NUTS implementation.
#' * Stan Development Team. Stan Reference Manual (Algorithms section):
#'   https://mc-stan.org/docs/reference-manual/
#' * Stan Development Team. Stan documentation:
#'   https://mc-stan.org/users/documentation/
#' * Stan Development Team. CmdStan User's Guide:
#'   https://mc-stan.org/docs/cmdstan-guide/
#'
#' @inherit cmdstan_model examples
#'
sample <- function(data = NULL,
                   seed = NULL,
                   refresh = NULL,
                   init = NULL,
                   save_latent_dynamics = FALSE,
                   output_dir = getOption("cmdstanr_output_dir"),
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
                   save_metric = getOption("cmdstanr_save_metric", FALSE),
                   save_cmdstan_config = getOption("cmdstanr_save_config", FALSE)) {

  private$assert_current()
  if (cmdstan_version_compare(self$cmdstan_version(), "2.36.0") < 0 &&
      !fixed_param && !is.null(private$variables_) &&
      length(private$variables_$parameters) == 0) {
    stop("Model contains no parameters. Please use 'fixed_param = TRUE'.", call. = FALSE)
  }
  if (fixed_param) {
    save_warmup <- FALSE
  }
  procs <- CmdStanMCMCProcs$new(
    num_procs = checkmate::assert_integerish(chains, lower = 1, len = 1),
    parallel_procs = checkmate::assert_integerish(parallel_chains, lower = 1, null.ok = TRUE),
    threads_per_proc = assert_valid_threads(
      threads_per_chain, private$reported_features_, multiple_chains = TRUE
    ),
    show_stderr_messages = show_exceptions,
    show_stdout_messages = show_messages
  )
  model_variables <- private$variables_
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
    diagnostics = diagnostics,
    save_metric = save_metric
  )
  args <- CmdStanArgs$new(
    method_args = sample_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = private$standalone_functions(),
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
    opencl_ids = assert_valid_opencl(opencl_ids, private$reported_features_),
    model_variables = model_variables,
    save_cmdstan_config = save_cmdstan_config
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
#'   MPI (message passing interface). The target audience for MPI are
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
#'   mpi_options = list(stan_mpi = TRUE, CXX = "mpicxx", TBB_CXX_TYPE = "gcc")
#'   mod = cmdstan_model("model.stan", cpp_options = mpi_options)
#'   ```
#'   The C++ options that must be supplied to the
#'   [cmdstan_model()] call are:
#'   - `stan_mpi`: Enables the use of MPI with Stan if `TRUE`.
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
#'
#' @return A [`CmdStanMCMC`] object.
#'
#' @references
#' * Hoffman, M. D., and Gelman, A. (2014). The No-U-Turn sampler:
#'   adaptively setting path lengths in Hamiltonian Monte Carlo.
#'   *Journal of Machine Learning Research*, 15(47), 1593-1623.
#' * Betancourt, M. (2017). A conceptual introduction to Hamiltonian Monte Carlo.
#'   arXiv:1701.02434. Appendix A describes Stan's dynamic HMC/NUTS implementation.
#' * Stan Development Team. Stan Reference Manual (Algorithms section):
#'   https://mc-stan.org/docs/reference-manual/
#' * Stan Development Team. Stan documentation:
#'   https://mc-stan.org/users/documentation/
#' * Stan Development Team. CmdStan User's Guide:
#'   https://mc-stan.org/docs/cmdstan-guide/
#'
#' @seealso The Stan Math Library's documentation
#'   ([mc-stan.org/math](https://mc-stan.org/math/)) for more
#'   details on MPI support in Stan.
#'
#' @examples
#' \dontrun{
#' # mpi_options <- list(stan_mpi = TRUE, CXX = "mpicxx", TBB_CXX_TYPE = "gcc")
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
                       output_dir = getOption("cmdstanr_output_dir"),
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
                       save_cmdstan_config = getOption("cmdstanr_save_config", FALSE)) {
  private$assert_current()

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
  model_variables <- private$variables_
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
    standalone_env = private$standalone_functions(),
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
    model_variables = model_variables,
    save_cmdstan_config = save_cmdstan_config
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
#'   Stan's optimizer. Without the Jacobian adjustment (the default),
#'   optimization finds a mode of the target in the original constrained
#'   parameter space (if the mode exists). With the adjustment, it finds a mode
#'   of the corresponding density in the unconstrained parameter space.
#'
#'   The `jacobian` argument does not determine whether prior terms are
#'   included. Every contribution to the Stan program's `target`, including
#'   prior terms, is included under either setting. The Jacobian adjustment is
#'   particularly useful when making a distributional approximation in the
#'   unconstrained space (see [Laplace sampling][model-method-laplace]). If the
#'   model has only unconstrained parameters, including the Jacobian has no
#'   effect. See the [CmdStan User's
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/index.html) for more details.
#'
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the [CmdStan User’s
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/) for more details on the
#'   default arguments. These values are also available via the
#'   [`$cmdstan_defaults`][model-method-cmdstan_defaults] method.
#'
#' @template model-common-args
#' @param threads (positive integer) If the model was
#'   compiled with threading support, the number of
#'   threads to use in parallelized sections (e.g., when
#'   using the Stan functions `reduce_sum()` or `map_rect()`).
#' @param iter (positive integer) The maximum number of iterations.
#' @param algorithm (string) The optimization algorithm. One of `"lbfgs"`,
#'   `"bfgs"`, or `"newton"`. The control parameters below are only available
#'   for `"lbfgs"` and `"bfgs`. For their default values and more details see
#'   the CmdStan User's Guide. The default values can also be obtained by
#'   running `cmdstanr_example(method="optimize")$metadata()`.
#' @param jacobian (logical) Whether or not to use the Jacobian adjustment for
#'   constrained variables. For historical reasons, the default is `FALSE`.
#'   `FALSE` finds a mode of the target in the constrained parameter space and
#'   `TRUE` finds a mode in the unconstrained space. This argument does not
#'   control whether prior terms are included. See the **Description** section
#'   and the CmdStan User's Guide for more details. For use later with
#'   [`$laplace()`][model-method-laplace], the `jacobian` argument should
#'   typically be set to `TRUE`.
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
#' @references
#' * Stan Development Team. Stan Reference Manual (Algorithms section, optimization):
#'   https://mc-stan.org/docs/reference-manual/
#' * Stan Development Team. Stan documentation:
#'   https://mc-stan.org/users/documentation/
#' * Stan Development Team. CmdStan User's Guide:
#'   https://mc-stan.org/docs/cmdstan-guide/
#'
#' @inherit cmdstan_model examples
#'
optimize <- function(data = NULL,
                     seed = NULL,
                     refresh = NULL,
                     init = NULL,
                     output_dir = getOption("cmdstanr_output_dir"),
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
                     history_size = NULL,
                     show_messages = TRUE,
                     show_exceptions = TRUE,
                     save_cmdstan_config = getOption("cmdstanr_save_config", FALSE)) {
  private$assert_current()
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stderr_messages = show_exceptions,
    show_stdout_messages = show_messages,
    threads_per_proc = assert_valid_threads(threads, private$reported_features_)
  )
  model_variables <- private$variables_
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
    standalone_env = private$standalone_functions(),
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = 1,
    data_file = process_data(data, model_variables),
    save_latent_dynamics = FALSE,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = assert_valid_opencl(opencl_ids, private$reported_features_),
    model_variables = model_variables,
    save_cmdstan_config = save_cmdstan_config
  )
  runset <- CmdStanRun$new(args, procs)
  runset$run_cmdstan()
  CmdStanMLE$new(runset)
}
CmdStanModel$set("public", name = "optimize", value = optimize)


#' Run Stan's Laplace algorithm
#'
#' @name model-method-laplace
#' @aliases laplace
#' @family CmdStanModel methods
#'
#' @description The `$laplace()` method of a [`CmdStanModel`] object produces a
#'   sample from a normal approximation centered at the mode of a distribution
#'   in the unconstrained space. When the mode was found with the Jacobian
#'   adjustment, the draws provide an estimate of the mean and standard
#'   deviation of the posterior distribution. See the `jacobian` argument below
#'   for how this setting relates to the value used when running optimization,
#'   and the [CmdStan User’s Guide](https://mc-stan.org/docs/cmdstan-guide/)
#'   for more details.
#'
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the [CmdStan User’s
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/) for more details on the
#'   default arguments. These values are also available via the
#'   [`$cmdstan_defaults`][model-method-cmdstan_defaults] method.
#'
#' @template model-common-args
#' @inheritParams model-method-optimize
#' @param mode (multiple options) The mode to center the approximation at. One
#'   of the following:
#'   * A [`CmdStanMLE`] object from a previous run of [`$optimize()`][model-method-optimize].
#'   * The path to a CmdStan CSV file from running optimization.
#'   * `NULL`, in which case [$optimize()][model-method-optimize] will be run
#'   with `jacobian=jacobian` (see the `jacobian` argument below).
#'
#'   In all cases the total time reported by [`$time()`][fit-method-time] will be
#'   the time of the Laplace sampling step only and does not include the time
#'   taken to run the `$optimize()` method.
#' @param opt_args (named list) A named list of optional arguments to pass to
#'   [$optimize()][model-method-optimize] if `mode=NULL`.
#' @param draws (positive integer) The number of draws to take.
#' @param jacobian (logical) Whether or not to enable the Jacobian adjustment
#'   for constrained parameters. The default is `TRUE`. See the
#'   [Laplace Sampling](https://mc-stan.org/docs/cmdstan-guide/laplace_sample_config.html)
#'   section of the CmdStan User's Guide for more details. If `mode` is not
#'   `NULL` then the value of `jacobian` must match the value used when
#'   optimization was originally run so the mode and the Laplace approximation
#'   use the same target density. If `mode` is `NULL` then the value of
#'   `jacobian` specified here is used when running optimization.
#'
#' @return A [`CmdStanLaplace`] object.
#'
#' @references
#' * Stan Development Team. Stan Reference Manual (Algorithms section, Laplace approximation):
#'   https://mc-stan.org/docs/reference-manual/
#' * Stan Development Team. Stan documentation:
#'   https://mc-stan.org/users/documentation/
#' * Stan Development Team. CmdStan User's Guide:
#'   https://mc-stan.org/docs/cmdstan-guide/
#'
#' @examples
#' \dontrun{
#' file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(file)
#' mod$print()
#'
#' stan_data <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
#' fit_mode <- mod$optimize(data = stan_data, jacobian = TRUE)
#' fit_laplace <- mod$laplace(data = stan_data, mode = fit_mode)
#' fit_laplace$summary()
#'
#' # if mode isn't specified optimize is run internally first
#' fit_laplace <- mod$laplace(data = stan_data)
#' fit_laplace$summary()
#'
#' # plot approximate posterior
#' bayesplot::mcmc_hist(fit_laplace$draws("theta"))
#' }
#'
#'
laplace <- function(data = NULL,
                    seed = NULL,
                    refresh = NULL,
                    init = NULL,
                    output_dir = getOption("cmdstanr_output_dir"),
                    output_basename = NULL,
                    sig_figs = NULL,
                    threads = NULL,
                    opencl_ids = NULL,
                    mode = NULL,
                    opt_args = NULL,
                    jacobian = TRUE, # different default than for optimize!
                    draws = NULL,
                    show_messages = TRUE,
                    show_exceptions = TRUE,
                    save_cmdstan_config = getOption("cmdstanr_save_config", FALSE)) {
  private$assert_current()
  if (!is.null(mode) && !is.null(opt_args)) {
    stop("Cannot specify both 'opt_args' and 'mode' arguments.", call. = FALSE)
  }
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stderr_messages = show_exceptions,
    show_stdout_messages = show_messages,
    threads_per_proc = assert_valid_threads(threads, private$reported_features_)
  )
  model_variables <- private$variables_

  if (!is.null(mode)) {
    if (inherits(mode, "CmdStanMLE")) {
      cmdstan_mode <- mode
    } else {
      if (!(is.character(mode) && length(mode) == 1)) {
        stop("If not NULL or a CmdStanMLE object then 'mode' must be a path to a CSV file.", call. = FALSE)
      }
      cmdstan_mode <- as_cmdstan_fit(mode)
    }
  } else { # mode = NULL, run optimize()
    checkmate::assert_list(opt_args, any.missing = FALSE, names = "unique", null.ok = TRUE)
    mode_output_basename <- output_basename
    if (!is.null(mode_output_basename)) {
      mode_output_basename <- paste0(mode_output_basename, "-mode")
    }
    args <- list(
      data = data,
      seed = seed,
      refresh = refresh,
      init = init,
      output_dir = output_dir,
      output_basename = mode_output_basename,
      sig_figs = sig_figs,
      threads = threads,
      opencl_ids = opencl_ids,
      jacobian = jacobian
    )
    cmdstan_mode <- do.call(self$optimize, append(args, opt_args))
    if (cmdstan_mode$return_codes() != 0) {
      stop(
        "Optimization failed.\n",
        "Consider supplying the 'mode' argument or additional optimizer args.",
        call. = FALSE
      )
    }
  }
  laplace_args <- LaplaceArgs$new(
    mode = cmdstan_mode,
    draws = draws,
    jacobian = jacobian
  )
  args <- CmdStanArgs$new(
    method_args = laplace_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = private$standalone_functions(),
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = 1,
    data_file = process_data(data, model_variables),
    save_latent_dynamics = FALSE,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = assert_valid_opencl(opencl_ids, private$reported_features_),
    model_variables = model_variables,
    save_cmdstan_config = save_cmdstan_config
  )
  runset <- CmdStanRun$new(args, procs)
  runset$run_cmdstan()
  CmdStanLaplace$new(runset)
}
CmdStanModel$set("public", name = "laplace", value = laplace)


#' Run Stan's variational approximation algorithms
#'
#' @name model-method-variational
#' @aliases variational
#' @family CmdStanModel methods
#'
#' @description The `$variational()` method of a [`CmdStanModel`] object runs
#'   Stan's Automatic Differentiation Variational Inference (ADVI) algorithms.
#'   The approximation is a Gaussian in the unconstrained variable space. Stan
#'   implements two ADVI algorithms: the `algorithm="meanfield"` option uses a
#'   fully factorized Gaussian for the approximation; the `algorithm="fullrank"`
#'   option uses a Gaussian with a full-rank covariance matrix for the
#'   approximation. See the
#'   [CmdStan User’s Guide](https://mc-stan.org/docs/cmdstan-guide/)
#'   for more details.
#'
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the [CmdStan User’s
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/) for more details on the
#'   default arguments. These values are also available via the
#'   [`$cmdstan_defaults`][model-method-cmdstan_defaults] method.
#'
#' @template model-common-args
#' @template model-save-latent-dynamics-arg
#' @param threads (positive integer) If the model was
#'   compiled with threading support, the number of
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
#' @param draws (positive integer) Number of approximate posterior samples to
#'   draw and save.
#'
#' @return A [`CmdStanVB`] object.
#'
#' @references
#' * Kucukelbir, A., Tran, D., Ranganath, R., Gelman, A., and Blei, D. M.
#'   (2017). Automatic differentiation variational inference.
#'   *Journal of Machine Learning Research*, 18(14), 1-45.
#' * Stan Development Team. Stan Reference Manual (Algorithms section, variational inference):
#'   https://mc-stan.org/docs/reference-manual/
#' * Stan Development Team. Stan documentation:
#'   https://mc-stan.org/users/documentation/
#' * Stan Development Team. CmdStan User's Guide:
#'   https://mc-stan.org/docs/cmdstan-guide/
#'
#' @inherit cmdstan_model examples
#'
variational <- function(data = NULL,
                        seed = NULL,
                        refresh = NULL,
                        init = NULL,
                        save_latent_dynamics = FALSE,
                        output_dir = getOption("cmdstanr_output_dir"),
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
                        draws = NULL,
                        show_messages = TRUE,
                        show_exceptions = TRUE,
                        save_cmdstan_config = getOption("cmdstanr_save_config", FALSE)) {
  private$assert_current()
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stderr_messages = show_exceptions,
    show_stdout_messages = show_messages,
    threads_per_proc = assert_valid_threads(threads, private$reported_features_)
  )
  model_variables <- private$variables_
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
    output_samples = draws
  )
  args <- CmdStanArgs$new(
    method_args = variational_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = private$standalone_functions(),
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
    opencl_ids = assert_valid_opencl(opencl_ids, private$reported_features_),
    model_variables = model_variables,
    save_cmdstan_config = save_cmdstan_config
  )
  runset <- CmdStanRun$new(args, procs)
  runset$run_cmdstan()
  CmdStanVB$new(runset)
}
CmdStanModel$set("public", name = "variational", value = variational)


#' Run Stan's Pathfinder Variational Inference Algorithm
#'
#' @name model-method-pathfinder
#' @aliases pathfinder
#' @family CmdStanModel methods
#'
#' @description The `$pathfinder()` method of a [`CmdStanModel`] object runs
#'   Stan's Pathfinder algorithms. Pathfinder is a variational method for
#'   approximately sampling from differentiable log densities. Starting from a
#'   random initialization, Pathfinder locates normal approximations
#'   to the target density along a quasi-Newton optimization path in
#'   the unconstrained space, with local covariance estimated using
#'   the negative inverse Hessian estimates produced by the LBFGS
#'   optimizer. Pathfinder selects the normal approximation with the
#'   lowest estimated Kullback-Leibler (KL) divergence to the true
#'   posterior. Finally Pathfinder draws from that normal
#'   approximation and returns the draws transformed to the
#'   constrained scale. See the
#'   [CmdStan User’s Guide](https://mc-stan.org/docs/cmdstan-guide/)
#'   for more details.
#'
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the [CmdStan User’s
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/) for more details on the
#'   default arguments. These values are also available via the
#'   [`$cmdstan_defaults`][model-method-cmdstan_defaults] method.
#'
#' @template model-common-args
#' @param threads (positive integer) If the model was
#'   compiled with threading support, the number of
#'   threads to use in parallelized sections (e.g., for multi-path pathfinder
#'   as well as `reduce_sum`).
#' @param num_threads Deprecated and will be removed in a future release. Use
#'   `threads` instead.
#' @param init_alpha (positive real) The initial step size parameter.
#' @param tol_obj (positive real) Convergence tolerance on changes in objective function value.
#' @param tol_rel_obj (positive real) Convergence tolerance on relative changes in objective function value.
#' @param tol_grad (positive real) Convergence tolerance on the norm of the gradient.
#' @param tol_rel_grad (positive real) Convergence tolerance on the relative norm of the gradient.
#' @param tol_param (positive real) Convergence tolerance on changes in parameter value.
#' @param history_size (positive integer) The size of the history used when
#'   approximating the Hessian.
#' @param single_path_draws (positive integer) Number of draws a single
#'   pathfinder should return. The number of draws PSIS sampling samples from
#'   will be equal to `single_path_draws * num_paths`.
#' @param draws (positive integer) Number of draws to return after performing
#'   Pareto smoothed importance sampling (PSIS). This should be smaller than
#'   `single_path_draws * num_paths`.
#' @param num_paths (positive integer) Number of single pathfinders to run. The
#'   default is `4`. The paths are run sequentially unless the model was
#'   compiled with `cpp_options = list(stan_threads =
#'   TRUE)` and `threads` is set, so running multiple paths in parallel requires
#'   both.
#' @param max_lbfgs_iters (positive integer) The maximum number of iterations
#'   for LBFGS.
#' @param num_elbo_draws (positive integer) Number of draws to make when
#'   calculating the ELBO of the approximation at each iteration of LBFGS.
#' @param save_single_paths (logical) Whether to save the output from each
#'   single-Pathfinder run. For a multi-path run, CmdStan writes one Stan CSV
#'   file containing draws and one JSON file containing the L-BFGS and ELBO
#'   iterations for each path. For a single-path run, the main output CSV
#'   contains the draws and CmdStan writes an additional JSON file. The
#'   auxiliary files are written to `output_dir`, or to a temporary directory
#'   if `output_dir = NULL`. They are not included in the paths returned by the
#'   fitted object's `$output_files()` method. See the [CmdStan User's
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/pathfinder_config.html#single-path-pathfinder-outputs)
#'   for details.
#' @param psis_resample (logical) Whether to perform pareto smoothed importance sampling.
#'  If `TRUE`, the number of draws returned will be equal to `draws`.
#'  If `FALSE`, the number of draws returned will be equal to `single_path_draws * num_paths`.
#' @param calculate_lp (logical) Whether to calculate the log probability of the draws.
#' If `TRUE`, the log probability will be calculated and given in the output.
#' If `FALSE`, the log probability will only be returned for draws used to determine the
#'  ELBO in the pathfinder steps. All other draws will have a log probability of `NA`.
#'  A value of `FALSE` will also turn off pareto smoothed importance sampling as the
#'  lp calculation is needed for PSIS.
#' @return A [`CmdStanPathfinder`] object.
#'
#' @references
#' * Zhang, L., Carpenter, B., Gelman, A., and Vehtari, A. (2022).
#'   Pathfinder: parallel quasi-Newton variational inference.
#'   *Journal of Machine Learning Research*, 23(306), 1-49.
#' * Stan Development Team. Stan Reference Manual (Algorithms section, Pathfinder):
#'   https://mc-stan.org/docs/reference-manual/
#' * Stan Development Team. Stan documentation:
#'   https://mc-stan.org/users/documentation/
#' * Stan Development Team. CmdStan User's Guide:
#'   https://mc-stan.org/docs/cmdstan-guide/
#'
#' @inherit cmdstan_model examples
#'
pathfinder <- function(data = NULL,
                       seed = NULL,
                       refresh = NULL,
                       init = NULL,
                       output_dir = getOption("cmdstanr_output_dir"),
                       output_basename = NULL,
                       sig_figs = NULL,
                       threads = NULL,
                       opencl_ids = NULL,
                       num_threads = NULL,
                       init_alpha = NULL,
                       tol_obj = NULL,
                       tol_rel_obj = NULL,
                       tol_grad = NULL,
                       tol_rel_grad = NULL,
                       tol_param = NULL,
                       history_size = NULL,
                       single_path_draws = NULL,
                       draws = NULL,
                       num_paths = 4,
                       max_lbfgs_iters = NULL,
                       num_elbo_draws = NULL,
                       save_single_paths = NULL,
                       psis_resample = NULL,
                       calculate_lp = NULL,
                       show_messages = TRUE,
                       show_exceptions = TRUE,
                       save_cmdstan_config = getOption("cmdstanr_save_config", FALSE)) {
  private$assert_current()
  if (!is.null(num_threads)) {
    if (!is.null(threads)) {
      stop("Cannot specify both 'threads' and deprecated 'num_threads'.", call. = FALSE)
    }
    warning(
      "'num_threads' is deprecated as of CmdStanR 1.0.0 and will be removed in a future release. Please use 'threads' instead.",
      call. = FALSE
    )
    threads <- num_threads
  }
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stderr_messages = show_exceptions,
    show_stdout_messages = show_messages,
    threads_per_proc = assert_valid_threads(threads, private$reported_features_)
  )
  model_variables <- private$variables_
  pathfinder_args <- PathfinderArgs$new(
    init_alpha = init_alpha,
    tol_obj = tol_obj,
    tol_rel_obj = tol_rel_obj,
    tol_grad = tol_grad,
    tol_rel_grad = tol_rel_grad,
    tol_param = tol_param,
    history_size = history_size,
    draws = draws,
    single_path_draws = single_path_draws,
    num_paths = num_paths,
    max_lbfgs_iters = max_lbfgs_iters,
    num_elbo_draws = num_elbo_draws,
    save_single_paths = save_single_paths,
    psis_resample = psis_resample,
    calculate_lp = calculate_lp
  )
  args <- CmdStanArgs$new(
    method_args = pathfinder_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = private$standalone_functions(),
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = 1,
    data_file = process_data(data, model_variables),
    save_latent_dynamics = FALSE,
    seed = seed,
    init = init,
    refresh = refresh,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = assert_valid_opencl(opencl_ids, private$reported_features_),
    model_variables = model_variables,
    save_cmdstan_config = save_cmdstan_config
  )
  runset <- CmdStanRun$new(args, procs)
  runset$run_cmdstan()
  CmdStanPathfinder$new(runset)
}
CmdStanModel$set("public", name = "pathfinder", value = pathfinder)


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
#'   Any argument left as `NULL` will default to the default value used by the
#'   installed version of CmdStan. See the [CmdStan User’s
#'   Guide](https://mc-stan.org/docs/cmdstan-guide/) for more details on the
#'   default arguments. These values are also available via the
#'   [`$cmdstan_defaults`][model-method-cmdstan_defaults] method.
#'
#' @inheritParams model-method-sample
#' @param fitted_params (multiple options) The parameter draws to use. One of
#'   the following:
#'  * A [CmdStanMCMC], [CmdStanMLE], [CmdStanLaplace], [CmdStanVB], or
#'  [CmdStanPathfinder] fitted model object.
#'  * A [posterior::draws_array] or [posterior::draws_matrix] object returned by
#'  CmdStanR's [`$draws()`][fit-method-draws] method.
#'  * A character vector of paths to CmdStan CSV output files.
#'
#' For a [CmdStanMLE] object, optimization supplies one point estimate, so
#' generated quantities that use RNG functions produce only one simulation.
#' For [CmdStanLaplace], [CmdStanVB], and [CmdStanPathfinder] objects, generated
#' quantities are evaluated once per approximate draw.
#'
#' NOTE: CmdStan CSV paths are used directly. A [CmdStanMCMC] object also reuses
#' its original output files when they are available. If any of those files are
#' unavailable, CmdStanR writes the in-memory draws to temporary CSV files.
#' Other fitted model objects and posterior draws objects are converted to
#' temporary CSV files on each call. For repeated calls that require this
#' conversion, we recommend using [draws_to_csv()] once and passing the
#' resulting paths to `$generate_quantities()`.
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
#'     array[N] int<lower=0,upper=1> y;
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
#'     array[N] int<lower=0,upper=1> y;
#'   }
#'   parameters {
#'     real<lower=0,upper=1> theta;
#'   }
#'   generated quantities {
#'     array[N] int y_rep = bernoulli_rng(rep_vector(theta, N));
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
                                output_dir = getOption("cmdstanr_output_dir"),
                                output_basename = NULL,
                                sig_figs = NULL,
                                parallel_chains = getOption("mc.cores", 1),
                                threads_per_chain = NULL,
                                opencl_ids = NULL,
                                show_messages = TRUE,
                                show_exceptions = TRUE) {
  private$assert_current()
  fitted_params_files <- process_fitted_params(fitted_params)
  procs <- CmdStanGQProcs$new(
    num_procs = length(fitted_params_files),
    parallel_procs = checkmate::assert_integerish(parallel_chains, lower = 1, null.ok = TRUE),
    threads_per_proc = assert_valid_threads(
      threads_per_chain, private$reported_features_, multiple_chains = TRUE
    ),
    show_stderr_messages = show_exceptions,
    show_stdout_messages = show_messages
  )
  model_variables <- private$variables_
  gq_args <- GenerateQuantitiesArgs$new(fitted_params = fitted_params_files)
  args <- CmdStanArgs$new(
    method_args = gq_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = private$standalone_functions(),
    model_name = self$model_name(),
    exe_file = self$exe_file(),
    proc_ids = seq_along(fitted_params_files),
    data_file = process_data(data, model_variables),
    seed = seed,
    output_dir = output_dir,
    output_basename = output_basename,
    sig_figs = sig_figs,
    opencl_ids = assert_valid_opencl(opencl_ids, private$reported_features_),
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
#'   Unlike other CmdStan methods, `$diagnose()` does not expose `show_messages`
#'   or `show_exceptions` arguments. CmdStan's standard output is not printed
#'   during execution, while standard error is always displayed. The captured
#'   console output can be inspected with the returned object's
#'   [`$output()`][fit-method-output] method.
#'
#' @inheritParams model-method-sample
#' @param epsilon (positive real) The finite difference step size. Default
#'   value is 1e-6.
#' @param error (positive real)  The error threshold. Default value is 1e-6.
#'
#' @return A [`CmdStanDiagnose`] object.
#'
#' @seealso The [`$gradients()`][fit-method-gradients] method for accessing the
#'   gradients and the [`$output()`][fit-method-output] method for displaying
#'   the captured console output.
#' @template seealso-docs
#' @inherit CmdStanDiagnose examples
#'
diagnose <- function(data = NULL,
                     seed = NULL,
                     init = NULL,
                     output_dir = getOption("cmdstanr_output_dir"),
                     output_basename = NULL,
                     epsilon = NULL,
                     error = NULL) {
  private$assert_current()
  procs <- CmdStanProcs$new(
    num_procs = 1,
    show_stdout_messages = FALSE,
    show_stderr_messages = TRUE
  )
  model_variables <- private$variables_
  diagnose_args <- DiagnoseArgs$new(
    epsilon = epsilon,
    error = error
  )
  args <- CmdStanArgs$new(
    method_args = diagnose_args,
    stan_file = self$stan_file(),
    stan_code = suppressWarnings(self$code()),
    model_methods_env = private$model_methods_env_,
    standalone_env = private$standalone_functions(),
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
#'   expose them for use in \R.
#'
#'   This method is also available for all fitted model objects. See
#'   **Examples**.
#'
#'   Note: there may be many compiler warnings emitted during compilation but
#'   these can be ignored so long as they are warnings and not errors.
#'
#' @param global (logical) Should the functions be added to the Global
#'   Environment? The default is `FALSE`, in which case the functions are
#'   available via the `functions` field of the R6 object.
#' @param verbose (logical) Should detailed information about generated code be
#'   printed to the console? Defaults to `FALSE`.
#' @return `NULL`, invisibly.
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
  private$assert_current()
  expose_stan_functions(private$standalone_functions(), global, verbose)
  invisible(NULL)
}
CmdStanModel$set("public", name = "expose_functions", value = expose_functions)


#' Get CmdStan default argument values
#'
#' @name model-method-cmdstan_defaults
#' @aliases cmdstan_defaults
#' @family CmdStanModel methods
#'
#' @description The `$cmdstan_defaults()` method of a [`CmdStanModel`]
#'   object queries the compiled model binary for the default argument
#'   values used by a given inference method. The returned list uses
#'   CmdStanR-style argument names (e.g., `iter_sampling` instead of
#'   CmdStan's `num_samples`).
#'
#' @param method (string) The inference method for which to retrieve default
#'   argument values. One of `"sample"`, `"optimize"`, `"variational"`,
#'   `"pathfinder"`, or `"laplace"`. The default is `"sample"`.
#' @return A named list of default argument values for the specified
#'   method, with CmdStanR-style argument names.
#'
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' mod <- cmdstan_model(file.path(cmdstan_path(),
#'                                "examples/bernoulli/bernoulli.stan"))
#' mod$cmdstan_defaults("sample")
#' mod$cmdstan_defaults("optimize")
#' }
#'
cmdstan_defaults <- function(method = c("sample", "optimize", "variational",
                                        "pathfinder", "laplace")) {
  method <- match.arg(method)
  private$assert_current()
  parse_cmdstan_args(self$exe_file(), method, self$stan_file())
}
CmdStanModel$set("public", name = "cmdstan_defaults", value = cmdstan_defaults)


#' What is known about how the model's executable was built
#'
#' @name model-method-build_info
#' @aliases build_info
#' @family CmdStanModel methods
#'
#' @description The `$build_info()` method of a [`CmdStanModel`] object
#'   calls [stan_build_info()] on the model's executable. See that page for
#'   what the result holds. The method reports on the executable as it is
#'   now, so it also works on a model whose executable was replaced or whose
#'   build record is gone.
#'
#'   This method is different than the `$cpp_options()` method, which answers a
#'   narrower question: the C++ options this model object was created with.
#'   `$build_info()` describes the executable itself, including what it reports
#'   about its own build when run. The difference is clear when considering a
#'   model created with `cmdstan_model(exe_file = )` from just an executable
#'   with no build record: `$cpp_options()` is empty, since no options were
#'   given, but `$build_info()` still reports whether the executable was built
#'   with threading, OpenCL and so on.
#'
#' @return See [stan_build_info()].
#'
#' @template seealso-docs
#'
#' @examples
#' \dontrun{
#' mod <- cmdstan_model(
#'   file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan"),
#'   cpp_options = list(stan_threads = TRUE)
#' )
#' info <- mod$build_info()
#' info
#' info$reported_features$stan_threads
#' }
#'
build_info <- function() {
  stan_build_info(self$exe_file())
}
CmdStanModel$set("public", name = "build_info", value = build_info)



# internal ----------------------------------------------------------------
#' The error for a build argument supplied with no `stan_file`
#'
#' With no `stan_file` there is nothing to build, so the executable is used
#' as it is and none of these arguments apply.
#'
#' @param cpp_options,stanc_options,include_paths,user_header The arguments
#'   the user passed to `cmdstan_model()`.
#' @param force_recompile,pedantic,dir The arguments the user passed to
#'   `cmdstan_model()`.
#' @return `NULL`, invisibly. The first argument supplied is an error.
#' @noRd
assert_no_build_args_for_exe_only <- function(cpp_options, stanc_options,
                                              include_paths, user_header,
                                              force_recompile, pedantic, dir) {
  build_arg_message <- function(arg) {
    sprintf(
      paste0(
        "`%s` cannot be supplied for a model created from an executable alone. ",
        "With no Stan file there is nothing to build, so the executable is used as it is."
      ),
      arg
    )
  }
  if (!is.null(cpp_options)) {
    stop(build_arg_message("cpp_options"), call. = FALSE)
  }
  if (!is.null(stanc_options)) {
    stop(build_arg_message("stanc_options"), call. = FALSE)
  }
  if (!is.null(include_paths)) {
    stop(
      "`include_paths` cannot be supplied for a model created from an executable alone. ",
      "Include paths resolve `#include` lines in a Stan file, and there is none.",
      call. = FALSE
    )
  }
  if (!is.null(user_header)) {
    stop(build_arg_message("user_header"), call. = FALSE)
  }
  if (!is.null(force_recompile)) {
    stop(build_arg_message("force_recompile"), call. = FALSE)
  }
  if (isTRUE(pedantic)) {
    stop(
      "`pedantic` cannot be supplied for a model created from an executable alone. ",
      "Pedantic mode checks a Stan program, and there is none.",
      call. = FALSE
    )
  }
  if (!is.null(dir)) {
    stop(build_arg_message("dir"), call. = FALSE)
  }
  invisible(NULL)
}

assert_stan_file_exists <- function(stan_file) {
  if (!file.exists(stan_file)) {
    stop(
      "The Stan file '", stan_file, "' this model was created from no longer ",
      "exists. To run the executable without its program, create the model ",
      "with `cmdstan_model(exe_file = )`.",
      call. = FALSE
    )
  }
}

# cmdstan_defaults() helpers

#' Parse CmdStan default argument values from model binary
#'
#' Runs a CmdStan model binary with `help-all` to extract valid arguments
#' and their default values for a given inference method, returning them
#' with CmdStanR argument names.
#'
#' @noRd
#' @param model_binary Path to the CmdStan model binary.
#' @param method Inference method: `"sample"`, `"optimize"`,
#'   `"variational"`, `"pathfinder"`, or `"laplace"`.
#' @param stan_file The model's Stan file, for the error when the binary
#'   will not run.
#' @return A named list with cmdstanr-style argument names and default
#'   values.
parse_cmdstan_args <- function(model_binary, method, stan_file) {
  withr::with_path(
    c(
      toolchain_PATH_env_var(),
      tbb_path()
    ),
    ret <- tryCatch(
      wsl_compatible_run(
        command = wsl_safe_path(model_binary),
        args = c(method, "help-all"),
        error_on_status = FALSE
      ),
      error = function(e) {
        stop_cannot_run(model_binary, stan_file, conditionMessage(e))
      }
    )
  )
  # A CmdStan executable answers help-all with status 0, so anything else
  # means it couldn't get that far, e.g. a missing library.
  if (is.na(ret$status) || ret$status != 0) {
    output <- trimws(paste0(ret$stderr, ret$stdout))
    stop_cannot_run(
      model_binary, stan_file,
      if (nzchar(output)) output else paste("exit status", ret$status)
    )
  }
  # CmdStan may write help text to stdout or stderr depending on the platform
  raw <- paste0(ret$stdout, ret$stderr)
  output <- strsplit(raw, "\r?\n")[[1]]

  argument_map <- map_cmdstan_to_cmdstanr(method)
  cmdstan_keys <- unname(argument_map)
  public_names <- names(argument_map)

  defaults <- list()
  n <- length(output)
  # Track the current hierarchical argument key using section indentation.
  section_indents <- integer(0)
  section_names <- character(0)

  for (i in seq_len(n)) {
    line <- output[i]
    content <- trimws(line)

    # Skip blank lines so they don't reset the section stack
    if (!nzchar(content)) next

    indent <- nchar(sub("^(\\s*).*", "\\1", line))

    # Drop sections at deeper or equal indentation
    while (length(section_indents) > 0 &&
           section_indents[[length(section_indents)]] >= indent) {
      section_indents <- section_indents[-length(section_indents)]
      section_names <- section_names[-length(section_names)]
    }

    section_name <- parse_cmdstan_section_name(content)
    if (!is.null(section_name)) {
      section_indents <- c(section_indents, indent)
      section_names <- c(section_names, section_name)
      next
    }

    arg_name <- parse_cmdstan_arg_name(content)
    if (!is.null(arg_name)) {

      # Build the full dotted argument key: method.section1.section2...arg_name
      # The top-level method heading (e.g. "sample") is tracked as a section,
      # so it becomes the first segment of the key.
      full_key <- paste(c(section_names, arg_name), collapse = ".")

      # Check if this full argument key matches one of our target arguments
      match_idx <- match(full_key, cmdstan_keys, nomatch = 0L)

      if (match_idx > 0L) {
        default_value <- find_cmdstan_default_value(output, i, n)
        defaults[[public_names[[match_idx]]]] <- default_value
      }
    }
  }

  defaults
}

#' Parse CmdStan section name from a help-all line
#' @noRd
parse_cmdstan_section_name <- function(line) {
  match <- regmatches(line, regexec("^([a-z_][a-z0-9_]*)$", line))[[1]]
  if (length(match) >= 2) match[2] else NULL
}

#' Parse CmdStan argument name from a help-all line
#' @noRd
parse_cmdstan_arg_name <- function(line) {
  match <- regmatches(line, regexec("^([a-z_][a-z0-9_]*)=", line))[[1]]
  if (length(match) >= 2) match[2] else NULL
}

#' Find CmdStan default value following a help-all argument line
#' @noRd
find_cmdstan_default_value <- function(output, line_idx, n_lines) {
  default_value <- NULL

  for (j in (line_idx + 1):min(line_idx + 5, n_lines)) {
    next_content <- trimws(output[j])
    if (grepl("^Defaults to", next_content)) {
      default_value <- parse_default_value(next_content)
      break
    }
    # Stop if we hit another argument
    if (grepl("^[a-z_][a-z0-9_]*=", next_content)) break
  }

  default_value
}

#' Parse default value from "Defaults to ..." line
#' @noRd
parse_default_value <- function(line) {
  val_str <- sub("^Defaults to\\s*", "", line)
  if (val_str %in% c("true", "false")) return(val_str == "true")
  if (grepl("^-?[0-9]+$", val_str)) return(as.integer(val_str))
  if (grepl("^-?[0-9]*\\.?[0-9]+([eE][+-]?[0-9]+)?$", val_str)) return(as.numeric(val_str))
  val_str
}

#' Map CmdStan argument names to CmdStanR argument names
#' @noRd
map_cmdstan_to_cmdstanr <- function(method) {
  switch(method,
         sample = c(
           iter_sampling = "sample.num_samples",
           iter_warmup = "sample.num_warmup",
           save_warmup = "sample.save_warmup",
           thin = "sample.thin",
           adapt_engaged = "sample.adapt.engaged",
           adapt_delta = "sample.adapt.delta",
           init_buffer = "sample.adapt.init_buffer",
           term_buffer = "sample.adapt.term_buffer",
           window = "sample.adapt.window",
           save_metric = "sample.adapt.save_metric",
           max_treedepth = "sample.hmc.nuts.max_depth",
           metric = "sample.hmc.metric",
           metric_file = "sample.hmc.metric_file",
           step_size = "sample.hmc.stepsize"
         ),
         optimize = c(
           algorithm = "optimize.algorithm",
           jacobian = "optimize.jacobian",
           iter = "optimize.iter",
           init_alpha = "optimize.lbfgs.init_alpha",
           tol_obj = "optimize.lbfgs.tol_obj",
           tol_rel_obj = "optimize.lbfgs.tol_rel_obj",
           tol_grad = "optimize.lbfgs.tol_grad",
           tol_rel_grad = "optimize.lbfgs.tol_rel_grad",
           tol_param = "optimize.lbfgs.tol_param",
           history_size = "optimize.lbfgs.history_size"
         ),
         variational = c(
           algorithm = "variational.algorithm",
           iter = "variational.iter",
           grad_samples = "variational.grad_samples",
           elbo_samples = "variational.elbo_samples",
           eta = "variational.eta",
           adapt_engaged = "variational.adapt.engaged",
           adapt_iter = "variational.adapt.iter",
           tol_rel_obj = "variational.tol_rel_obj",
           eval_elbo = "variational.eval_elbo",
           draws = "variational.output_samples"
         ),
         pathfinder = c(
           init_alpha = "pathfinder.init_alpha",
           tol_obj = "pathfinder.tol_obj",
           tol_rel_obj = "pathfinder.tol_rel_obj",
           tol_grad = "pathfinder.tol_grad",
           tol_rel_grad = "pathfinder.tol_rel_grad",
           tol_param = "pathfinder.tol_param",
           history_size = "pathfinder.history_size",
           draws = "pathfinder.num_psis_draws",
           num_paths = "pathfinder.num_paths",
           save_single_paths = "pathfinder.save_single_paths",
           psis_resample = "pathfinder.psis_resample",
           calculate_lp = "pathfinder.calculate_lp",
           max_lbfgs_iters = "pathfinder.max_lbfgs_iters",
           single_path_draws = "pathfinder.num_draws",
           num_elbo_draws = "pathfinder.num_elbo_draws"
         ),
         laplace = c(
           jacobian = "laplace.jacobian",
           draws = "laplace.draws"
         ),
         character(0)
  )
}
