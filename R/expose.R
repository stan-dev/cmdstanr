# Exposing a model's C++ to R through Rcpp: the model methods behind
# $init_model_methods() and the standalone functions behind
# $expose_functions().

check_sundials_fpic <- function(verbose) {
  if (!os_is_linux()){
    return(invisible(NULL))
  }
  sundials_flags <- get_cmdstan_flags("CPPFLAGS_SUNDIALS")
  local_flags <- cmdstan_make_local()
  if (any(grepl("-fPIC", c(sundials_flags, local_flags), fixed = TRUE))) {
    return(invisible(NULL))
  }
  if (interactive()) {
    message(
      "SUNDIALS needs to be compiled with -fPIC when exposing functions or ",
      "model methods on Linux.\n",
      "Updating your make/local file to include -fPIC and rebuilding CmdStan now..."
    )
  }
  cmdstan_make_local(cpp_options = list("CPPFLAGS_SUNDIALS += -fPIC"), append = TRUE)
  rebuild_cmdstan(quiet = !verbose)
  if (interactive()) {
    message("CmdStan has been rebuilt, continuing with model compilation...")
  }
}

rcpp_source_stan <- function(code, env, verbose = FALSE, ...) {
  check_sundials_fpic(verbose)
  cxxflags <- get_cmdstan_flags("CXXFLAGS")
  cppflags <- get_cmdstan_flags("CPPFLAGS")
  cmdstanr_includes <- system.file("include", package = "cmdstanr", mustWork = TRUE)
  cmdstanr_includes <- paste0(" -I\"", cmdstanr_includes,"\"")
  libs <- c("LDLIBS", "LIBSUNDIALS", "TBB_TARGETS", "LDFLAGS_TBB", "SUNDIALS_TARGETS")
  libs <- paste(sapply(libs, get_cmdstan_flags), collapse = " ")
  if (.Platform$OS.type == "windows") {
    libs <- paste(libs, "-fopenmp")
  }
  withr::with_path(repair_path(file.path(cmdstan_path(),"stan/lib/stan_math/lib/tbb")),
    withr::with_makevars(
      c(
        USE_CXX14 = 1,
        PKG_CPPFLAGS = cppflags,
        PKG_CXXFLAGS = paste0(cxxflags, cmdstanr_includes, collapse = " "),
        PKG_LIBS = libs
      ),
      Rcpp::sourceCpp(code = code, env = env, verbose = verbose, ...)
    )
  )
  invisible(NULL)
}

# Can the compiled model-method bindings in `env` be called in this session?
model_methods_are_live <- function(env) {
  model_ptr <- env$model_ptr_
  typeof(model_ptr) == "externalptr" &&
    is.null(attributes(model_ptr)) &&
    !identical(model_ptr, .cmdstanr$NULL_EXTERNAL_POINTER)
}

# Detect serialized sourceCpp wrappers whose native symbol was lost after reload.
source_cpp_native_symbol_is_null <- function(fun) {
  if (!is.function(fun)) {
    return(FALSE)
  }
  fun_body <- body(fun)
  if (!rlang::is_call(fun_body, ".Call") || length(fun_body) < 2) {
    return(FALSE)
  }
  # Rcpp::sourceCpp() wrappers call into a NativeSymbol via `.Call(...)`.
  # After reloading a serialized object that symbol can degrade to `<pointer: 0x0>`.
  symbol <- fun_body[[2]]
  if (!inherits(symbol, "NativeSymbol")) {
    return(FALSE)
  }
  identical(symbol, unserialize(serialize(symbol, NULL)))
}

# Drop stale compiled bindings but keep the generated C++ so model methods
# can be rebuilt lazily in the current session if they are later requested.
# This avoids an error when a CmdStanModel object with compiled bindings is
# loaded from an older session: https://github.com/stan-dev/cmdstanr/issues/1157
drop_stale_model_methods <- function(env) {
  if (is.null(env$model_ptr) || !source_cpp_native_symbol_is_null(env$model_ptr)) {
    return(invisible(FALSE))
  }
  rm(list = setdiff(ls(env, all.names = TRUE), "hpp_code_"), envir = env)
  invisible(TRUE)
}

#' Drop standalone-function bindings that no longer point at compiled code
#'
#' After `readRDS()` the compiled wrappers point at nothing, so drop them and
#' let `expose_functions()` compile again.
#'
#' @param env The model's `functions` environment.
#' @return Whether anything was dropped, invisibly.
#' @noRd
drop_stale_standalone_functions <- function(env) {
  if (!isTRUE(env$compiled) ||
      !source_cpp_native_symbol_is_null(env[[env$fun_names[1]]])) {
    return(invisible(FALSE))
  }
  rm(list = setdiff(ls(env, all.names = TRUE), "hpp_code"), envir = env)
  env$compiled <- FALSE
  invisible(TRUE)
}

expose_model_methods <- function(env, verbose = FALSE, quiet = FALSE) {
  if (!quiet && rlang::is_interactive()) {
    message("Compiling additional model methods...")
  }
  code <- c(env$hpp_code_,
            readLines(system.file("include", "model_methods.cpp",
                                  package = "cmdstanr", mustWork = TRUE)))

  code <- paste(code, collapse = "\n")
  rcpp_source_stan(code, env, verbose)
  invisible(NULL)
}

initialize_model_pointer <- function(env, datafile_path, seed = 0) {
  ptr_and_rng <- env$model_ptr(ifelse(is.null(datafile_path), "", datafile_path), seed)
  env$model_ptr_ <- ptr_and_rng$model_ptr
  env$model_rng_ <- ptr_and_rng$base_rng
  env$num_upars_ <- env$get_num_upars(env$model_ptr_)
  env$param_metadata_ <- env$get_param_metadata(env$model_ptr_)
  invisible(NULL)
}

create_skeleton <- function(param_metadata, model_variables,
                            transformed_parameters, generated_quantities) {
  target_params <- names(model_variables$parameters)
  if (transformed_parameters) {
    target_params <- c(target_params,
                       names(model_variables$transformed_parameters))
  }
  if (generated_quantities) {
    target_params <- c(target_params,
                       names(model_variables$generated_quantities))
  }
  lapply(param_metadata[target_params], function(par_dims) {
    if ((length(par_dims) == 0)) {
      array(0, dim = 1)
    } else {
      array(0, dim = par_dims)
    }
  })
}

get_function_name <- function(fun_start, fun_end, model_lines) {
  fun_string <- paste(model_lines[(fun_start+1):fun_end], collapse = " ")
  types <- c(
    "auto",
    "int",
    "double",
    "Eigen::Matrix<(.*)>",
    "std::vector<(.*)>",
    "std::tuple<(.*)>",
    "std::complex<(.*)>"
  )
  pattern <- paste0(
    # Only match if the type occurs at start of string
    "^(\\s*)?(",
    paste0(types, collapse="|"),
    # Only match if type followed by a function name and opening bracket
    ")\\s*(?=\\w*\\()")
  fun_name <- gsub(pattern, "", fun_string, perl = TRUE)
  sub("\\(.*", "", fun_name, perl = TRUE)
}

# Prepare the c++ code for a standalone function so that it can be exported to R:
# - Replace the auto return type with the plain type
# - Add Rcpp::export attribute
# - Remove the pstream__ argument and pass Rcpp::Rcout by default
# - Replace the boost::ecuyer1988& base_rng__ argument with an integer seed argument
#     that instantiates an RNG
prep_fun_cpp <- function(fun_start, fun_end, model_lines) {
  fun_body <- paste(model_lines[fun_start:fun_end], collapse = " ")
  fun_body <- gsub("// [[stan::function]]", "// [[Rcpp::export]]\n", fun_body, fixed = TRUE)
  fun_body <- gsub("std::ostream\\*\\s*pstream__\\s*=\\s*nullptr", "", fun_body)
  if (grepl("stan::rng_t", fun_body)) {
    fun_body <- gsub("stan::rng_t&\\s*base_rng__", "SEXP base_rng_ptr, SEXP seed", fun_body)
    rng_seed <- "Rcpp::XPtr<stan::rng_t> base_rng(base_rng_ptr);base_rng->seed(Rcpp::as<int>(seed));"
    fun_body <- gsub("return", paste(rng_seed, "return"), fun_body)
    fun_body <- gsub("base_rng__,", "*(base_rng.get()),", fun_body, fixed = TRUE)
  }
  fun_body <- gsub("pstream__", "&Rcpp::Rcout", fun_body, fixed = TRUE)
  fun_body <- paste(fun_body, collapse = "\n")
  gsub(pattern = ",\\s*)", replacement = ")", fun_body)
}

compile_functions <- function(env, verbose = FALSE, global = FALSE) {
  funs <- grep("// [[stan::function]]", env$hpp_code, fixed = TRUE)
  funs <- c(funs, length(env$hpp_code))

  stan_funs <- sapply(seq_len(length(funs) - 1), function(ind) {
    fun_end <- funs[ind + 1]
    fun_end <- ifelse(env$hpp_code[fun_end] == "}", fun_end, fun_end - 1)
    prep_fun_cpp(funs[ind], fun_end, env$hpp_code)
  })

  reserved_names <- unique(
    unlist(
      lapply(stan_funs, function(stan_fun) {
        regmatches(
          stan_fun,
          gregexpr("(?<=_stan_)[[:alnum:]_]+", stan_fun, perl = TRUE)
        )[[1]]
      }),
      use.names = FALSE
    )
  )

  if (length(reserved_names) > 0) {
    stop(
      paste0(
        "expose_functions() can't expose this Stan function because the function ",
        "name and/or one or more argument names use a reserved keyword ",
        "(typically in the C++ toolchain used to compile Stan). Please rename ",
        "the function/arguments in your Stan functions block and try again. ",
        "Conflicting names: ",
        paste(reserved_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  env$fun_names <- sapply(seq_len(length(funs) - 1), function(ind) {
    get_function_name(funs[ind], funs[ind + 1], env$hpp_code)
  })

  dups <- env$fun_names[duplicated(env$fun_names)]

  if (length(dups) > 0) {
    stop("Overloaded functions are currently not able to be exposed to R!",
          " The following overloaded functions were found: ",
          paste(dups, collapse=", "),
          call. = FALSE)
  }

  mod_stan_funs <- paste(c(
    env$hpp_code[1:(funs[1] - 1)],
    "#include <rcpp_tuple_interop.hpp>",
    "#include <rcpp_eigen_interop.hpp>",
    "#include <stan_rng.hpp>",
    stan_funs),
  collapse = "\n")
  if (global) {
    rcpp_source_stan(mod_stan_funs, globalenv(), verbose)
  } else {
    rcpp_source_stan(mod_stan_funs, env, verbose)
  }

  # If an RNG function is exposed, initialise a Boost RNG object stored in the
  # environment
  rng_funs <- grep("rng\\b", env$fun_names, value = TRUE)
  if (length(rng_funs) > 0) {
    rng_cpp <- system.file("include", "base_rng.cpp", package = "cmdstanr", mustWork = TRUE)
    rcpp_source_stan(paste0(readLines(rng_cpp), collapse="\n"), env, verbose)
    env$rng_ptr <- env$base_rng(seed=1)
  }

  # For all RNG functions, pass the initialised Boost RNG by default
  for (fun in rng_funs) {
    if (global) {
      fun_env <- globalenv()
    } else {
      fun_env <- env
    }
    fundef <- get(fun, envir = fun_env)
    funargs <- formals(fundef)
    funargs$base_rng_ptr <- env$rng_ptr
    # To allow for exported RNG functions to respect the R 'set.seed()' call,
    # we need to derive a seed deterministically from the current RNG state
    funargs$seed <- quote(sample.int(.Machine$integer.max, 1))
    formals(fundef) <- funargs
    assign(fun, fundef, envir = fun_env)
  }

  env$compiled <- TRUE
  invisible(NULL)
}

expose_stan_functions <- function(function_env, global = FALSE,
                                   verbose = FALSE, quiet = FALSE) {
  if (os_is_wsl()) {
    stop("Standalone functions are not currently available with ",
          "WSL CmdStan and will not be compiled",
          call. = FALSE)
  }
  if (is.null(function_env$hpp_code)) {
    stop("Standalone functions cannot be exposed for a model created from ",
         "an executable alone. There is no Stan program to take them from.",
         call. = FALSE)
  }
  if (!any(grepl("[[stan::function]]", function_env$hpp_code, fixed = TRUE))) {
    warning("No standalone functions found to compile and expose to R!", call. = FALSE)
    return(invisible(NULL))
  }
  require_suggested_package("Rcpp")
  drop_stale_standalone_functions(function_env)
  if (function_env$compiled) {
    if (!global) {
      if (!quiet) {
        message("Functions already compiled, nothing to do!")
      }
    } else {
      if (!quiet) {
        message("Functions already compiled, copying to global environment")
      }
      # Create reference to global environment, avoids NOTE about assigning to global
      pos <- 1
      envir <- as.environment(pos)
      lapply(function_env$fun_names, function(fun_name) {
        assign(fun_name, get(fun_name, function_env), envir)
      })
    }
  } else {
    if (!quiet && rlang::is_interactive()) {
      message("Compiling standalone functions...")
    }
    compile_functions(function_env, verbose, global)
  }
  invisible(NULL)
}
