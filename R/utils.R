# misc --------------------------------------------------------------------

#' Check for Windows
#' @return `TRUE` if OS is Windows, `FALSE` if not.
#' @noRd
os_is_windows <- function() {
  isTRUE(.Platform$OS.type == "windows")
}

#' Famous helper for switching on `NULL`
#' @param x,y Any \R objects.
#' @return `x` if not `NULL`, otherwise `y` regardless of whether `y` is `NULL`.
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

#' Returns the type of make command to use to compile
#' @return Returns "mingw32-make" if using cmdstan 2.21+ and running Windows
#' @noRd
make_cmd <- function() {
  # Cmdstan 2.21 introduced TBB that requires mingw32-make on Windows
  ver <- .cmdstanr$VERSION
  if (os_is_windows() && (is.null(ver) || ver >= "2.21")) {
    "mingw32-make.exe"
  } else {
    "make"
  }
}

check_target_exe <- function(exe) {
  exe_path <- file.path(cmdstan_path(), exe)
  if (!file.exists(exe_path)) {
    run_log <- processx::run(
      command = make_cmd(),
      args = exe,
      wd = cmdstan_path(),
      echo_cmd = TRUE,
      echo = TRUE,
      error_on_status = TRUE
    )
  }
}


# paths and extensions ----------------------------------------------------

# Replace `\\` with `/` in a path
# Needed for windows if CmdStan version is < 2.21:
# https://github.com/stan-dev/cmdstanr/issues/1#issuecomment-539118598
repair_path <- function(path) {
  if (!length(path) || !is.character(path)) {
    return(path)
  }
  path <- path.expand(path)
  path <- gsub("\\\\", "/", path)
  path <- gsub("//", "/", path)
  if (endsWith(path, "/")) {
    # remove trailing "/" (is this necessary?)
    path <- substr(path, 1, nchar(path) - 1)
  }
  path
}

#' Get extension for executable depending on OS
#' @noRd
#' @param path If not `NULL` then a path to add the extension to.
#' @return If `path` is `NULL` then `".exe"` on Windows and `""` otherwise. If
#'   `path` is not `NULL` then `.exe` is added as the extension on Windows.
cmdstan_ext <- function(path = NULL) {
  ext <- if (os_is_windows()) ".exe" else ""
  if (is.null(path)) {
    return(ext)
  }
  path <- repair_path(path)
  paste0(path, ext)
}

# Strip extension from a file path
strip_ext <- function(file) {
  tools::file_path_sans_ext(file)
}

# If a file/dir exists return its absolute path
# doesn't error if not found
.absolute_path <- function(path) {
  if (file.exists(path)) {
    new_path <- repair_path(path)
  } else {
    new_path <- path
  }

  if (grepl("^~", path) || grepl("^(/+|[A-Za-z]:)", new_path)) {
    return(new_path)
  }
  repair_path(file.path(getwd(), new_path))
}
absolute_path <- Vectorize(.absolute_path, USE.NAMES = FALSE)



# read, write, and copy files --------------------------------------------

#' Copy temporary files (e.g., output, data) to a different location
#'
#' Copies to specified directory using specified basename,
#' appending suffix `-id.ext` to each. If files with the specified
#' names already exist they are overwritten.
#'
#' @noRd
#' @param current_paths Paths to current temporary files.
#' @param new_dir Path to directory where the files should be saved.
#' @param new_basename Base filename to use.
#' @param ids Unique identifiers (e.g., `chain_ids`).
#' @param timestamp Add a timestamp to the file names?
#' @param ext Extension to use for all saved files (default is `ext=".csv"`).
#' @return The paths to the new files or `NA` for any that couldn't be
#'   copied.
copy_temp_files <-
  function(current_paths,
           new_dir,
           new_basename,
           ids = NULL,
           timestamp = TRUE,
           random = TRUE,
           ext = ".csv") {
    checkmate::assert_directory_exists(new_dir, access = "w")
    destinations <- generate_file_names(
      basename = new_basename,
      ext = ext,
      ids = ids,
      timestamp = timestamp,
      random = random
    )
    if (new_dir != ".") {
      destinations <- file.path(new_dir, destinations)
    }

    copied <- file.copy(
      from = current_paths,
      to = destinations,
      overwrite = TRUE
    )
    if (!all(copied)) {
      destinations[!copied] <- NA_character_
    }
    absolute_path(destinations)
  }

# generate new file names
# see doc above for copy_temp_files
generate_file_names <-
  function(basename,
           ext = ".csv",
           ids = NULL,
           timestamp = TRUE,
           random = TRUE) {
    new_names <- basename
    if (timestamp) {
      stamp <- format(Sys.time(), "%Y%m%d%H%M")
      new_names <- paste0(new_names, "-", stamp)
    }
    if (!is.null(ids)) {
      new_names <- paste0(new_names, "-", ids)
    }

    if (random) {
      tf <- tempfile()
      rand <- substr(tf, nchar(tf) - 5, nchar(tf))
      new_names <- paste0(new_names, "-", rand)
    }

    if (length(ext)) {
      ext <- if (startsWith(ext, ".")) ext else paste0(".", ext)
      new_names <- paste0(new_names, ext)
    }
    new_names
  }


cpp_options_to_compile_flags <- function(cpp_options) {
  if (length(cpp_options) == 0) {
    return(NULL)
  }
  cpp_built_options = c()
  for (i in seq_len(length(cpp_options))) {
    option_name <- names(cpp_options)[i]
    cpp_built_options = c(cpp_built_options, paste0(toupper(option_name), "=", cpp_options[[i]]))
  }
  paste0(cpp_built_options, collapse = " ")
}

prepare_precompiled <- function(cpp_options = list(), quiet = FALSE) {
  flags <- NULL
  if (!is.null(cpp_options$stan_threads)) {
    flags <- c(flags, "threads")
  }
  if (!is.null(cpp_options$stan_mpi)) {
    flags <- c(flags, "mpi")
  }
  if (!is.null(cpp_options$stan_opencl)) {
    flags <- c(flags, "opencl")
  }
  if (is.null(flags)) {
    flags <- "noflags"
  } else {
    flags <- paste0(flags, collapse = "_")
  }
  main_path_w_flags <- file.path(cmdstan_path(), "src", "cmdstan", paste0("main_", flags, ".o"))
  main_path_o <- file.path(cmdstan_path(), "src", "cmdstan", "main.o")
  model_header_path_w_flags <- file.path(cmdstan_path(), "stan", "src", "stan", "model", paste0("model_header_", flags, ".hpp.gch"))
  model_header_path_gch <- file.path(cmdstan_path(), "stan", "src", "stan", "model", "model_header.hpp.gch")
  if (file.exists(model_header_path_gch)) {
    model_header_gch_used <- TRUE
  } else {
    model_header_gch_used <- FALSE
  }
  if (!file.exists(main_path_w_flags)) {
    message(
      "Compiling the main object file and precompiled headers (may take up to a few minutes). ",
      "This is only necessary the first time a model is compiled after installation or when ",
      "threading, MPI or OpenCL are used for the first time."
    )
    clean_compile_helper_files()
    run_log <- processx::run(
      command = make_cmd(),
      args = c(cpp_options_to_compile_flags(cpp_options),
               main_path_o),
      wd = cmdstan_path(),
      echo_cmd = !quiet,
      echo = !quiet,
      spinner = quiet,
      stderr_line_callback = function(x,p) { if (!quiet) message(x) },
      error_on_status = TRUE
    )
    file.copy(main_path_o, main_path_w_flags)
    if (model_header_gch_used) {
      run_log <- processx::run(
        command = make_cmd(),
        args = c(cpp_options_to_compile_flags(cpp_options),
                 file.path("stan", "src", "stan", "model", "model_header.hpp.gch")),
        wd = cmdstan_path(),
        echo_cmd = !quiet,
        echo = !quiet,
        spinner = quiet,
        stderr_line_callback = function(x,p) { if (!quiet) message(x) },
        error_on_status = TRUE
      )
      file.copy(model_header_path_gch, model_header_path_w_flags)
    }
  }
}

#' Set or get the number of threads used to execute Stan models
#'
#' DEPRECATED. Please use the `threads_per_chain` argument when fitting the model.
#'
#' @name stan_threads
NULL

#' @rdname stan_threads
#' @export
#' @return The value of the environment variable `STAN_NUM_THREADS`.
num_threads <- function() {
  warning("'num_threads()' is deprecated. Please use the 'metadata()' method of the fit object to obtain the number of threads used.")
  as.integer(Sys.getenv("STAN_NUM_THREADS"))
}

#' @rdname stan_threads
#' @export
#' @param num_threads (positive integer) The number of threads to set.
set_num_threads <- function(num_threads) {
  stop("Please use the 'threads_per_chain' argument in the $sample() method instead of set_num_threads().")
}

check_divergences <- function(data_csv) {
  if (!is.null(data_csv$post_warmup_sampler_diagnostics)) {
    divergences <- posterior::extract_variable_matrix(data_csv$post_warmup_sampler_diagnostics, "divergent__")
    num_of_draws <- length(divergences)
    num_of_divergences <- sum(divergences)
    if (num_of_divergences > 0) {
      percentage_divergences <- (num_of_divergences)/num_of_draws*100
      message(num_of_divergences, " of ", num_of_draws, " (", (format(round(percentage_divergences, 0), nsmall = 1)), "%)",
              " transitions ended with a divergence.\n",
              "These divergent transitions indicate that HMC is not fully able to explore the posterior distribution.\n",
              "Try increasing adapt delta closer to 1.\n",
              "If this doesn't remove all divergences, try to reparameterize the model.\n")
    }
  }
}

check_sampler_transitions_treedepth <- function(data_csv) {
  if (!is.null(data_csv$post_warmup_sampler_diagnostics)) {
    treedepth <- posterior::extract_variable_matrix(data_csv$post_warmup_sampler_diagnostics, "treedepth__")
    num_of_draws <- length(treedepth)
    max_treedepth <- data_csv$metadata$max_treedepth
    max_treedepth_hit <- sum(treedepth >= max_treedepth)
    if (max_treedepth_hit > 0) {
      percentage_max_treedepth <- (max_treedepth_hit)/num_of_draws*100
      message(max_treedepth_hit, " of ", num_of_draws, " (", (format(round(percentage_max_treedepth, 0), nsmall = 1)), "%)",
              " transitions hit the maximum treedepth limit of ", max_treedepth,
              " or 2^", max_treedepth, "-1 leapfrog steps.\n",
              "Trajectories that are prematurely terminated due to this limit will result in slow exploration.\n",
              "Increasing the max_treedepth limit can avoid this at the expense of more computation.\n",
              "If increasing max_treedepth does not remove warnings, try to reparameterize the model.\n")
    }
  }
}

matching_variables <- function(variable_filters, variables) {
  not_found <- NULL
  variable_filters <- unrepair_variable_names(variable_filters)
  variables <- unrepair_variable_names(variables)
  selected <- c()
  for (p in variable_filters) {
    matches <- which(variables == p | startsWith(variables, paste0(p, ".")))
    if (length(matches)) {
      selected <- c(selected, matches)
    } else {
      not_found <- c(not_found, p)
    }
  }
  list(
    matching = variables[unique(selected)],
    not_found = not_found
  )
}
